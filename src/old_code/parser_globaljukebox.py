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

N_PROC = 20


PATH_BASE = bss_io.PATH_BASE
PATH_FIG  = PATH_BASE.joinpath('Figures')

PATH_GLOBJUK = PATH_BASE.joinpath('Data', 'GlobalJukebox')



#--------------------------------------------------#
#   Utils
#---#


def int_to_bin_vec(integer, n=13):
    vec = np.zeros(n, dtype=bool)
    for i in range(n,0,-1):
        if integer == 0:
            return vec
        if integer >= 2**i:
            vec[i-1] = True
            integer -= 2**i
    return vec


def parse_duration(duration):
    read_time_fn = lambda x, y, z: x*60 + y + z/60
    splt = duration.split(':')
    if len(splt) != 3:
        return None
    else:
        return read_time_fn(*[float(y) for y in splt])


#--------------------------------------------------#
#   Global Jukebox
#---#


# Read in json list of dicts
# Convert to DataFrame
# Convert integer codings of binary variables into binary arrays (stored as "CV_#")
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
        df[f"CV_{i}"] = df[f"cv_{i}"].apply(int_to_bin_vec)

    df = translate_culture_id(df)

    df.to_pickle(PATH_GLOBJUK.joinpath("codings.pickle"))


# Convert culture ID to something more meaningful (Region, Division)
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


# Secondary processing of GJ data
# Extract some important codings for choosing which tracks to 
# run pitch analyses on
def process_GJ_codings(df):
    old_df = pd.read_pickle(PATH_GLOBJUK.joinpath("codings.pickle"))
    df = old_df.loc[:, ["audio_file_id", "culture_id"]]
    df.rename(columns={"audio_file_id":"ID"})
    df['duration'] = old_df.duration.apply(parse_duration)

    vocal_codings = {0:"random", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    inst_codings = {0:"none", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    df['vocal'] = old_df.CV_4.apply(lambda x: ';'.join([vocal_codings[i] for i in np.where(x)[0]]))
    df['inst'] = old_df.CV_7.apply(lambda x: ';'.join([inst_codings[i] for i in np.where(x)[0]]))
    
    return df

        




if __name__ == "__main__":
    pass



