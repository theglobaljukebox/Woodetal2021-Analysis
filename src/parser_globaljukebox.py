"""

Parse Global Jukebox data

"""
import json
import os
from pathlib import Path
import sys

import matplotlib.pyplot as plt
from multiprocessing import Pool
import numpy as np
import pandas as pd

import audio_tools
import bss_io
import bss_utils as utils

N_PROC = 20


PATH_BASE = bss_io.PATH_BASE
PATH_FIG  = PATH_BASE.joinpath('Figures')

PATH_BIRD_DISC = PATH_BASE.joinpath("Data/Recordings/Sub_sample/Bird songs (17+13)/discrete (17)")
PATH_BIRD_NONDISC = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Bird songs (17+13)/non-discrete (13)")

PATH_HMN = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Human songs (11)")
PATH_SPE = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Speech (12)")

PATH_HPB = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/birthday_poor-pitch_singers")
PATH_INST = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Inst_test")

PATH_GLOBJUK = PATH_BASE.joinpath('Data', 'GlobalJukebox')
PATH_NHS = PATH_BASE.joinpath('Data', 'NHS')


AUDIO_LIST = [a.stem for a in PATH_GLOBJUK.glob("audio/*mp3")]


#--------------------------------------------------#
#   General 
#---#

def load_pitch_trace_all(db='GJ'):
    if db == 'GJ':
        base = PATH_GLOBJUK.joinpath("pitch_trace")
    elif db == 'NHS':
        base = PATH_NHS.joinpath("pitch_trace")
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
#   old_df = pd.read_pickle(PATH_GLOBJUK.joinpath("codings.pickle"))
#   df = old_df.loc[:, ["audio_file_id", "culture_id"]]
    df.rename(columns={"audio_file_id":"ID"})
#   df['duration'] = old_df.duration.apply(parse_duration)
    df['duration_min'] = df.duration.apply(parse_duration)

    vocal_codings = {0:"random", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    inst_codings = {0:"none", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
#   df['vocal'] = old_df.CV_4.apply(lambda x: ';'.join([vocal_codings[i] for i in np.where(x)[0]]))
#   df['inst'] = old_df.CV_7.apply(lambda x: ';'.join([inst_codings[i] for i in np.where(x)[0]]))
    df['vocal'] = df.CV_4.apply(lambda x: ';'.join([vocal_codings[i] for i in np.where(x)[0]]))
    df['inst'] = df.CV_7.apply(lambda x: ';'.join([inst_codings[i] for i in np.where(x)[0]]))
    df['no_inst1'] = df.CV_2.apply(lambda x: x[0])
    df['no_inst2'] = df.CV_3.apply(lambda x: x[0])
    
    return df


def add_segmentation_labels(df):
    seg_gj = bss_io.load_all_matija(bss_io.PATH_GLOBJUK_AUDIO)
    seg_ave = df.audio_file_id.apply(lambda x: np.array(seg_gj.get(x, [[0,0,0,0]])).mean(axis=0))
    seg_ave = np.array([[x for x in y] for y in seg_ave])

    lbls = ['Solo', 'Group', 'Inst', 'Speech']
    for i, l in enumerate(lbls):
        df[l] = seg_ave[:,i]

    df['pitch_extracted'] = df.loc[:, lbls].sum(axis=1).astype(bool)

    return df




#--------------------------------------------------#
#   Natural History of Song
#---#


def load_nhs_transcriptions():
    base = PATH_NHS.joinpath("Transcriptions_all", "Transcriptions", "ngrams")
    out = {}
    for i in range(1, 119):
        pitch_file = base.joinpath(f"NHSDiscography-{i:03d}-pitches.txt") 
        pitches = [int(x) for y in open(pitch_file, 'r') for x in y.split() if x!='.' and x]
        rhythm_file = base.joinpath(f"NHSDiscography-{i:03d}-rhythms.txt") 
        rhythms = [x for y in open(rhythm_file, 'r') for x in y.split() if x!='.' and x]
        out.update({i:{'rhythm':rhythms, 'pitch':pitches}})
    return out

        




if __name__ == "__main__":
    pass



