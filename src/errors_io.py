"""

 Directory names go here

 John M. McBride 02/2021

"""
import os
import sys
import pickle

import numpy as np
import pandas as pd

PATH_BASE = [p for p in Path.cwd().parents if p.name == 'global-jukebox'][0]

PATH_GLOBJUK = PATH_BASE.joinpath('Data', 'GlobalJukebox')
PATH_GLOBJUK_AUDIO = PATH_BASE.joinpath('Data', 'GlobalJukebox', 'audio')


def load_all_matija(base=PATH_GLOBJUK_AUDIO):
    out = {}
    for f in base.glob("*csv"):
        try:
            seg = pd.read_csv(f, sep='\t', names=['Solo', 'Group', 'Inst', 'Speech'])
            name = f.stem.split('.')[0]
            if 'NHS' in str(base):
                name = int(name.split('-')[1])
            out[name] = seg
        except Exception as e:
            print(f, "\n", e)
    return out



