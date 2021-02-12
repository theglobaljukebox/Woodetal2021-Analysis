"""
Contains code for:
    Comparing Cantometrics codings with computational analyses
    Checking Cantometrics codings for consistency

Requires:
    f0 estimation using pYIN to be saved in numpy binary files (.npy)

 John M. McBride 02/2021

"""

from pathlib import Path
import os
import sys


import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


import errors_io
import parser_globaljukebox as GJ



N_PROC = 25

PATH_BASE = errors_io.PATH_BASE
PATH_VAL  = PATH_BASE.joinpath('Validation')
PATH_DATA = PATH_BASE.joinpath('Data')
PATH_GJ   = PATH_DATA.joinpath('GlobalJukebox', 'audio')


#-----------------------------------------------------------#
# Algorithm to ignore recordings that have already been
# checked

def get_already_checked(df):
    BASE = PATH_VAL.joinpath("automatic_screening")
    path_excel = BASE.joinpath("Cantometrics_Coding_Errors.xlsx")
    idx_already_checked = []
    for sheet in ['SoloVocal', 'GroupVocal', 'Inst']:
        xls = pd.read_excel(path_excel, sheet_name=sheet)
        idx_already_checked.extend(list(xls.loc[xls['Correct Coding?'].notnull(), 'Unnamed: 0']))
    return set(idx_already_checked).intersection(set(df.index))
    

#-----------------------------------------------------------#
#  Automatic identification of coding errors in Global Jukebox

#-----------------------#
# NN Classifier

def run_checks(df, threshold=0.5, save=False):
    df = df.copy()

    # Indices where: monophonic vocal; no instrument; algorithm was successful (pitch_extracted)
    #                and estimated probability of solo singing is less than threshold
    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & \
                 (df.pitch_extracted) & (df.inst=='none') & (df['Solo']<threshold)

    # Indices where: not monophonic vocal; no instrument; algorithm was successful (pitch_extracted)
    #                and estimated probability of group singing is less than threshold
    poly_vocal = (df.vocal.apply(lambda x: x in ['poly', 'hetero', 'random'])) & (df.no_inst1) & \
                 (df.no_inst2) & (df.pitch_extracted) & (df.inst=='none') & (df['Group']<threshold)

    # Indices where: contains instrument; algorithm was successful (pitch_extracted)
    #                and estimated probability of instrumental is less than threshold
    inst = (df.inst.apply(lambda x: 'none' not in x)) & (df.pitch_extracted) & (df['Inst']<threshold)

    lbls = ['SoloVocal', 'ChorusVocal', 'Inst']
    cols = ['Solo', 'Group', 'Inst', 'Speech']
    cols2print = ['audio_file_id', 'vocal', 'inst', 'no_inst1', 'no_inst2'] + cols

    # Round data for easy viewing
    for c in cols:
        df[c] = df[c].apply(lambda x:round(x,2))

    # Print to csv
    BASE = PATH_VAL.joinpath("automatic_screening")
    out_dict = {}
    for idx, lbl, c in zip([mono_vocal, poly_vocal, inst], lbls, cols):
        below_threshold = df.loc[idx, cols2print].sort_values(by=c)
        out_dict[lbl] = below_threshold
        if save:
            below_threshol.to_csv(BASE.joinpath(f"check_is_{lbl}.csv"))

    return out_dict


#-----------------------#
# pYIN f0 estimation

def get_melodic_range(f_id):
    # Data was previously extracted and stored as a binary numpy array 
    # using numpy.save
    path = PATH_DATA.joinpath("GlobalJukebox", "pitch_trace", f"{f_id}.npy")
    try:
        data = np.load(path)
        mel = data[1]
        mel = mel[mel>0]
        return np.log2(max(mel) / min(mel)) * 1200

    except:
        return 0


def melodic_range_check(df):
    BASE = PATH_VAL.joinpath("automatic_screening")
    # Only work with recordings where the algorithm was successful
    df = df.loc[df.pitch_extracted]
    # Get melodic range
    df['range'] = df.audio_file_id.apply(get_melodic_range)

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    # Select recordings that have monophonic singing and no instruments
    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & (df.inst=='none')
    # Print to csv
    cols2print = ['audio_file_id', 'vocal', 'inst', 'no_inst1', 'no_inst2', 'range']
    path_out = BASE.joinpath("melodic_range.csv")
    df.loc[mono_vocal, cols2print].sort_values(by='range', ascending=False).to_csv(path_out)

    return df


#-----------------------------------------------------------#
# Automatic identification of inconsistent codings

#-----------------------#
# Contains instrument?

def more_codings_check_inst(df):
    # CV2-1: No accompaniment (including clapping, stamping)
    # CV3-1: No instrument (does not specifically say about clapping, but clapping
    #                       usually results in a label that indicates presence of some instrument)
    # CV7-1: No instruments OR "two or more instruments... asynchronous"
    #        i.e., if other marks say 'no instruments', this should also say so.
    # CV8-1: Non-occurrence (only one instrument, or no instruments)
    #        i.e., same as CV7-1
    # CV9-1: Non-occurrence (only one instrument, or no instruments)
    #        i.e., same as CV7-1
    # CV13-1: No instruments (does not specifically say about clapping)
    # CV14-1: No orchestra or non-patterned
    #        i.e., same as CV7-1

    df = df.loc[df.pitch_extracted]
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    BASE = PATH_VAL.joinpath("automatic_screening")
    cols2print = ['audio_file_id'] + [f"CV{i}-1" for i in [2,3,7,8,9,13,14]]
    df_out = pd.DataFrame(columns=cols2print)

    # If all true or all false ignore;
    # Else flag as potential error
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][0] for j in [2,3,7,8,9,13,14]])
        all_false = all_true[[0,1,5]]
        if np.all(all_true) or np.all(all_false==False):
            continue
        df_out.loc[len(df_out)] = [df.loc[i, 'audio_file_id']] + list(all_true)

    path_out = BASE.joinpath("instrumental_coding_conflict.csv")
    df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Monophonic singing?

def more_codings_check_vocal(df):
    # CV1-2: One solo singer ## DO NOT USE
    # CV1-4: One solo singer after another ## DO NOT USE...
    #        (note that this can also be used for chorus when double-coding)
    # CV4-4: Monophony
    # CV5-1: Solo
    # CV6-1: Solo
    # CV12-1: Non-patterned (No rhythmic codeherence of any sort... Also, a solo singer)
    #         So, if the others say solo singer, this must also.

    df = df.loc[df.pitch_extracted]
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    BASE = PATH_VAL.joinpath("automatic_screening")
    cols2print = ['audio_file_id'] + [f"CV{i}-{j+1}" for i, j in zip([4,5,6,12], [3, 0, 0, 0])]
    df_out = pd.DataFrame(columns=cols2print)

    # If all true or all false ignore;
    # Else flag as potential error
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][k] for j, k in zip([4,5,6,12], [3, 0, 0, 0])])
        all_false = all_true[[0,1,2]]
        if np.all(all_true) or np.all(all_false==False):
            continue
        df_out.loc[len(df_out)] = [df.loc[i, 'audio_file_id']] + list(all_true)

    path_out = BASE.joinpath("solo_vocal_coding_conflict.csv")
    df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Rubato guide

def more_codings_check_rubato(df):
    # Rubato codings include some instuctions for labelling
    # that depends on previous codings:
    # Rubato: Vocal
    # CV26-1: Extreme rubato (true if CV11-13)
    # Rubato: Orchestra
    # CV27-1: Extreme rubato (true if CV13-13)

    df = df.loc[df.pitch_extracted]
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    BASE = PATH_VAL.joinpath("automatic_screening")
    cols2print = ['audio_file_id'] + [f"CV{i}-{j+1}" for i, j in zip([26,11,27,13], [0, 12, 0, 12])]
    df_out = pd.DataFrame(columns=cols2print)

    # Look for codings that are inconsistent with the
    # above logic
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][k] for j, k in zip([26,11,27,13], [0, 12, 0, 12])])
        no_problem = True
        to_print = [df.loc[i, 'audio_file_id']]
        if all_true[1] and not all_true[0]:
            to_print.extend(list([str(x) for x in all_true[:2]]))
            no_problem = False
        else:
            to_print.extend(['', ''])

        if all_true[3] and not all_true[2]:
            to_print.extend(list([str(x) for x in all_true[2:]]))
            no_problem = False
        else:
            to_print.extend(['', ''])

        if not no_problem:
            df_out.loc[len(df_out)] = to_print

    path_out = BASE.joinpath("rubato_coding_conflict.csv")
    df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Plot results

def plot_coding_error_rates():
    path_excel = PATH_VAL.joinpath("automatic_screening", "Cantometrics_Coding_Errors.xlsx")
    idx_already_checked = []
    fig, ax = plt.subplots()
    for sheet in ['SoloVocal', 'GroupVocal', 'Inst', 'MelodicRange']:
        xls = pd.read_excel(path_excel, sheet_name=sheet)
        idx = xls.loc[xls['Correct Coding?'].str.len()>0].index
        Y1 = np.array(xls.loc[idx, 'Correct Coding?']=='N', float)
        Y2 = np.convolve(Y1, np.ones(20)/20, mode='valid')
        X = np.arange(Y2.size)+20
        ax.plot(X, Y2, label=sheet)
    ax.legend(loc='best', frameon=False)
    ax.set_xlabel("Number of recordings checked")
    ax.set_ylabel("Fraction of errors found")

    fig.savefig(PATH_VAL.joinpath(f"coding_errors.pdf"), bbox_inches='tight')


if __name__ == "__main__":
    # Load and process GJ codings
    codings = GJ.convert_GJ_codings_to_dataframe()
    codings = GJ.process_GJ_codings(codings)
    codings = GJ.add_segmentation_labels(codings)

    # Compare codings with computational analyses
    run_matija_checks(codings, threshold=0.5)
    # Note that at this point, we checked the recordings that were
    # flagged for errors; thus when checking melodic range, we could
    # already exclude those recordings that were already checked
    melodic_range_check(codings)

    # Check codings for consistency
    more_codings_check_inst(codings)
    more_codings_check_vocal(codings)
    more_codings_check_rubato(codings)



