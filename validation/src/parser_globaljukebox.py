"""

Parse Global Jukebox data

Requires:
    Global Jukebox audio files
    'codings.json' file
    results from Matija Marolt's classification algorithm

 John M. McBride 02/2021

"""
import json
import os
from pathlib import Path
import sys

import matplotlib.pyplot as plt
from multiprocessing import Pool
import numpy as np
import pandas as pd

import val_io
import val_utils as utils

N_PROC = 20


PATH_BASE = val_io.PATH_BASE
PATH_GLOBJUK = PATH_BASE.joinpath('Data', 'GlobalJukebox')

AUDIO_LIST = [a.stem for a in PATH_GLOBJUK.glob("audio/*mp3")]


#--------------------------------------------------#
#   General 
#---#

def load_pitch_trace_all(db='GJ'):
    base = PATH_GLOBJUK.joinpath("pitch_trace")
    out = {}
    for f in base.glob("*npy"):
        try:
            out.update({f.stem: np.load(f)})
        except ValueError:
            out.update({f.stem: np.load(f, allow_pickle=True)})
            print(f"{f.stem} is not a standard numpy array")
    return out


def plot_pitch_trace(data):
    X, Y = data
    plt.plot(X[Y>0], Y[Y>0], 'o')




#--------------------------------------------------#
#   Global Jukebox
#---#

def convert_GJ_codings_to_dataframe():
    codings = json.load(open(PATH_GLOBJUK.joinpath("codings.json"), 'r'))
    canto_coding_cols = [f"cv_{i}" for i in range(1,38)] + \
                        [f"{a}_value_{b}" for a in ["ensemble", "instrument"] for b in ["id", "label"]] + \
                        ["orv_1", "orv_2"]
    other_cols = ["audio_file_id", "canto_coding_id", "culture_id", "duration", "genre", "local_latitude",
                  "local_longitude", "location", "song_title", "song_description", "people_culture", "recorded_by", "date"]

    df = pd.DataFrame(columns = other_cols + canto_coding_cols)
    for c in codings:
        data = [c[k] for k in other_cols] + [c["canto_coding_data"][k] for k in canto_coding_cols]
        df.loc[len(df)] = data

    for i in range(1, 38):
        df[f"CV_{i}"] = df[f"cv_{i}"].apply(utils.int_to_bin_vec)

    df = translate_culture_id(df)
    df['audio_exists'] = df.audio_file_id.apply(GJ.check_audio_exists)

    df.to_pickle(PATH_GLOBJUK.joinpath("codings.pickle"))


def parse_duration(duration):
    read_time_fn = lambda x, y, z: x*60 + y + z/60
    splt = duration.split(':')
    if len(splt) != 3:
        return None
    else:
        return read_time_fn(*[float(y) for y in splt])


def translate_culture_id(df):
    culfo = pd.read_csv(PATH_GLOBJUK.joinpath("All_Studies_Culture_Classification_Metadata_-_All_Cultures.csv"), encoding='latin1')
    id_key1 = {int(cid): region for cids, region in zip(*culfo.loc[:, ['All_cid', 'Region']].values.T) for cid in cids.split(';')}
    id_key2 = {int(cid): region for cids, region in zip(*culfo.loc[:, ['All_cid', 'Division']].values.T) for cid in cids.split(';')}

    ### Some corrections for missing data
    id_list = [18245, 28070, 30064]
    reg_list = ['South Asia', 'North America', 'North America']
    div_list = ["India", "United States", "United States"]
    for i, r, d in zip(id_list, reg_list, div_list):
        id_key1.update({i:r})
        id_key2.update({i:d})

    for i in df.index:
        k = df.loc[i, 'culture_id']
        if k not in id_key1.keys():
            print(k)
            continue
        df.loc[i, 'Region'] = id_key1[k]
        df.loc[i, 'Division'] = id_key2[k]
    return df


def check_audio_exists(ID):
    return ID in AUDIO_LIST and ID != 'PENDING'


def process_GJ_codings(df):
    df = df.copy()
    df.rename(columns={"audio_file_id":"ID"})
    df['duration_min'] = df.duration.apply(parse_duration)

    vocal_codings = {0:"random", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    inst_codings = {0:"none", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    df['vocal'] = df.CV_4.apply(lambda x: ';'.join([vocal_codings[i] for i in np.where(x)[0]]))
    df['inst'] = df.CV_7.apply(lambda x: ';'.join([inst_codings[i] for i in np.where(x)[0]]))
    df['no_inst1'] = df.CV_2.apply(lambda x: x[0])
    df['no_inst2'] = df.CV_3.apply(lambda x: x[0])
    
    return df


#--------------------------------------------------#
#   Marolt et al. segmentation results
#---#


# Load results from NN classification algorithm
def add_segmentation_labels(df):
    seg_gj = val_io.load_all_matija(val_io.PATH_GLOBJUK_AUDIO)
    seg_ave = df.audio_file_id.apply(lambda x: np.array(seg_gj.get(x, [[0,0,0,0]])).mean(axis=0))
    seg_ave = np.array([[x for x in y] for y in seg_ave])

    lbls = ['Solo', 'Group', 'Inst', 'Speech']
    for i, l in enumerate(lbls):
        df[l] = seg_ave[:,i]

    df['pitch_extracted'] = df.loc[:, lbls].sum(axis=1).astype(bool)

    return df


if __name__ == "__main__":
    pass


