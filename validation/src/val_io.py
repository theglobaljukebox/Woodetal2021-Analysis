"""

 Directory names go here

    After cloning the git repository - https://github.com/theglobaljukebox/cantometrics -
    PATH_GLOBJUK should be changed to match the location of the repository
        e.g. "/rootpath/yourhomefolder/cantometrics"


    Alternatively, one can use the file
    "global-jukebox/Inter-rater reliability/canto_codings.csv"

 John M. McBride 02/2021

"""
import os
import sys
from pathlib import Path
import pickle

import numpy as np
import pandas as pd

import val_utils as utils

PATH_BASE = Path("./")
PATH_VAL = PATH_BASE.joinpath("validation")
PATH_DATA = PATH_VAL.joinpath("data")
PATH_SRC = PATH_VAL.joinpath("src")
PATH_OUT = PATH_VAL.joinpath("automatic_screening")

<<<<<<< HEAD

# Load full set of Cantometrics codings
=======
>>>>>>> restructure
def load_codings():
    df = pd.read_csv(PATH_BASE.joinpath("Inter-rater reliability", "canto_codings.csv"))
    return df


# Process the data so that necessary information is easily accessible
def process_codings(df):
    for i in range(1, 38):
        df[f"CV_{i}"] = df[f"line_{i}"].apply(utils.int_to_bin_vec)
    vocal_codings = {0:"random", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    inst_codings = {0:"none", 3:"mono", 6:"unison", 9:"hetero", 12:"poly"}
    df['vocal'] = df.CV_4.apply(lambda x: ';'.join([vocal_codings[i] for i in np.where(x)[0]]))
    df['inst'] = df.CV_7.apply(lambda x: ';'.join([inst_codings[i] for i in np.where(x)[0]]))
    df['no_inst1'] = df.CV_2.apply(lambda x: x[0])
    df['no_inst2'] = df.CV_3.apply(lambda x: x[0])
    return df
    

# Load class segmentation data using the algorithm
# provided by Marolt et al. 2019,
# https://doi.org/10.3390/app9030439
def load_segment_classes():
    return pd.read_csv(PATH_DATA.joinpath("marolt_average_segment_classes.csv"))


# Load the melodic ranges extracted using pYIN
# https://code.soundsoftware.ac.uk/projects/pyin
def load_melodic_range():
    return pd.read_csv(PATH_DATA.joinpath("pYIN_melodic_range.csv"))



