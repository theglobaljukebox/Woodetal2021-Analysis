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


from val_io import PATH_BASE, PATH_VAL, PATH_DATA, PATH_OUT
import val_io
import parser_globaljukebox as GJ



#---------------------------------------------------------------#
# Algorithm to ignore recordings that have already been
# checked

def get_already_checked(df):
    path_excel = PATH_OUT.joinpath("Cantometrics_Coding_Errors.xlsx")
    idx_already_checked = []
    for sheet in ['SoloVocal', 'GroupVocal', 'Inst']:
        xls = pd.read_excel(path_excel, sheet_name=sheet)
        idx_already_checked.extend(list(xls.loc[xls['Correct Coding?'].notnull(), 'Unnamed: 0']))
    return set(idx_already_checked).intersection(set(df.index))
    


#---------------------------------------------------------------#
#  Automatic identification of coding errors in Global Jukebox
#  by comparing cantometrics codings with computational analyses


#-----------------------#
# NN Classifier

def check_segment_classes(df, threshold=0.5, save=False):
    cols = ['Solo', 'Group', 'Inst', 'Speech']

    # Load segmentation classes for those songs on which the algorithm
    # ran successfully
    dfs = val_io.load_segment_classes()
    song_id_list = dfs['song_id']
    df = df.loc[df.song_id.apply(lambda i: i in song_id_list)]
    for c in cols:
        df[c] = dfs[c]


    # Indices where: monophonic vocal; no instrument;
    #                and estimated probability of solo singing is less than threshold
    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & \
                 (df.inst=='none') & (df['Solo']<threshold)

    # Indices where: not monophonic vocal; no instrument;
    #                and estimated probability of group singing is less than threshold
    poly_vocal = (df.vocal.apply(lambda x: x in ['poly', 'hetero', 'random'])) & (df.no_inst1) & \
                 (df.no_inst2) & (df.inst=='none') & (df['Group']<threshold)

    # Indices where: contains instrument;
    #                and estimated probability of instrumental is less than threshold
    inst = (df.inst.apply(lambda x: 'none' not in x)) & (df['Inst']<threshold)

    lbls = ['SoloVocal', 'ChorusVocal', 'Inst']
    cols2print = ['song_id', 'vocal', 'inst', 'no_inst1', 'no_inst2'] + cols

    # Round data for easy viewing
    for c in cols:
        df[c] = df[c].apply(lambda x:round(x,2))

    # Print to csv
    out_dict = {}
    for idx, lbl, c in zip([mono_vocal, poly_vocal, inst], lbls, cols):
        below_threshold = df.loc[idx, cols2print].sort_values(by=c)
        out_dict[lbl] = below_threshold
        if save:
            below_threshold.to_csv(PATH_OUT.joinpath(f"check_is_{lbl}.csv"))

    return out_dict


#-----------------------#
# pYIN f0 estimation


### In-house code
### This was used to load previously-computed data onf0 vs time.
### Then the melodic range was 
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


def melodic_range_check(df, save=False):
    # In-house code (no longer necessary, but kept for future reference)
#   df['range'] = df.song_id.apply(get_melodic_range)

    # Get melodic range
    dfr = val_io.load_melodic_range()
    song_id_list = dfr['song_id']
    df = df.loc[df.song_id.apply(lambda i: i in song_id_list)]
    df['range'] = dfr['range']

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    # Select recordings that have monophonic singing and no instruments
    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & (df.inst=='none')

    if save:
        # Print to csv
        cols2print = ['song_id', 'vocal', 'inst', 'no_inst1', 'no_inst2', 'range']
        path_out = PATH_OUT.joinpath("melodic_range.csv")
        df.loc[mono_vocal, cols2print].sort_values(by='range', ascending=False).to_csv(path_out)

    return df


#-----------------------------------------------------------#
# Automatic identification of inconsistent codings

#-----------------------#
# Contains instrument?

def more_codings_check_inst(df, save=False):
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

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    cols2print = ['song_id'] + [f"CV{i}-1" for i in [2,3,7,8,9,13,14]]
    df_out = pd.DataFrame(columns=cols2print)

    # If all true or all false ignore;
    # Else flag as potential error
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][0] for j in [2,3,7,8,9,13,14]])
        all_false = all_true[[0,1,5]]
        if np.all(all_true) or np.all(all_false==False):
            continue
        df_out.loc[len(df_out)] = [df.loc[i, 'song_id']] + list(all_true)

    if save:
        path_out = PATH_OUT.joinpath("instrumental_coding_conflict.csv")
        df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Monophonic singing?

def more_codings_check_vocal(df, save=False):
    # CV1-2: One solo singer ## DO NOT USE
    # CV1-4: One solo singer after another ## DO NOT USE...
    #        (note that this can also be used for chorus when double-coding)
    # CV4-4: Monophony
    # CV5-1: Solo
    # CV6-1: Solo
    #            UPDATE: Can actually mean solo singer, OR little / no rhythmic coordination
    #            between a group of singers
    # CV12-1: Non-patterned (No rhythmic codeherence of any sort... Also, a solo singer)
    #         So, if the others say solo singer, this must also.

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    cols2print = ['song_id'] + [f"CV{i}-{j+1}" for i, j in zip([4,5,12], [3, 0, 0])]
    df_out = pd.DataFrame(columns=cols2print)

    # If all true or all false ignore;
    # Else flag as potential error
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][k] for j, k in zip([4,5,12], [3, 0, 0])])
        all_false = all_true[[0,1]]
        if np.all(all_true) or np.all(all_false==False):
            continue
        df_out.loc[len(df_out)] = [df.loc[i, 'song_id']] + list(all_true)

    if save:
        path_out = PATH_OUT.joinpath("solo_vocal_coding_conflict.csv")
        df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Rubato guide

def more_codings_check_rubato(df, save=False):
    # Rubato codings include some instuctions for labelling
    # that depends on previous codings:
    # Rubato: Vocal
    # CV26-1: Extreme rubato (true if CV11-13)
    # Rubato: Orchestra
    # CV27-1: Extreme rubato (true if CV13-13)

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    cols2print = ['song_id'] + [f"CV{i}-{j+1}" for i, j in zip([26,11,27,13], [0, 12, 0, 12])]
    df_out = pd.DataFrame(columns=cols2print)

    # Look for codings that are inconsistent with the
    # above logic
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][k] for j, k in zip([26,11,27,13], [0, 12, 0, 12])])
        no_problem = True
        to_print = [df.loc[i, 'song_id']]
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

    if save:
        path_out = PATH_OUT.joinpath("rubato_coding_conflict.csv")
        df_out.to_csv(path_out)

    return df_out


#-----------------------#
# Polyphony guide

def more_codings_check_poly(df, save=False):
    # The presence or absence of polyphony necessarily affects
    # some codings. 
    # POLYPHONIC TYPE
    # CV22-1: No polyphony (Apparently this only applies to vocals, not instruments)
    # MUSICAL ORGANIZATION OF THE VOCAL PART
    # CV4-13: Polyphony
    #         If CV22-1 is true, this must be false
    #         If CV22-1 is false, either this or CV7-13 must be true
    #         UPDATE: If CV22-1 is false, then THIS MUST BE TRUE
    # MUSICAL ORGANIZATION OF THE ORCHESTRA
    # CV7-13: Polyphony or polyrhythm
    #         If CV22-1 is true, ignore this (since it can mean polyrhythm)
    #         If CV22-1 is false, either this or CV4-13 must be true
    #         UPDATE: This can be ignored, as CV22-1 is only for singing
    # MELODIC FORM
    # CV16-13: Canonic or round form (found only in polyphonic singing)
    #         If CV22-1 is true, this must be false

    # Ignore recordings that were previously checked
    idx_to_drop = get_already_checked(df)
    df = df.drop(index=list(idx_to_drop))

    cols2print = ['song_id'] + [f"CV{i}-{j+1}" for i, j in zip([22,4,16], [0, 12, 12])]
    df_out = pd.DataFrame(columns=cols2print)

    # Look for codings that are inconsistent with the
    # above logic
    for i in df.index:
        all_true = np.array([df.loc[i, f"CV_{j}"][k] for j, k in zip([22,4,16], [0, 12, 12])])
        if all_true[0] and np.all(all_true[[1,2]]==False):
            continue
        elif not all_true[0] and all_true[1]:
            continue
        df_out.loc[len(df_out)] = [df.loc[i, 'song_id']] + list(all_true)

    if save:
        path_out = PATH_OUT.joinpath("poly_coding_conflict.csv")
        df_out.to_csv(path_out)

    return df_out



def find_missing_codings(df, save=False):
    # Create a boolean array (songs X variables) indicating
    # which variables are missing
    miss = np.array([df.loc[:,f"CV_{i}"].apply(lambda x: sum(x)==0) for i in range(1, 38)]).T
    df_m = pd.DataFrame(columns=["song_id", "number_missing", "missing_codings"])

    # For each song with missing data, 
    # note which codings are missing, and how many in total
    for i in np.where(miss.sum(axis=1))[0]:
        idx_missing = np.where(miss[i])[0]
        cvs = ', '.join([f"{j+1:d}" for j in idx_missing])
        df_m.loc[len(df_m)] = [df.loc[i, "song_id"], len(idx_missing), cvs]

    if save:
        path_out = PATH_OUT.joinpath("missing_codings.csv")
        df_m.to_csv(path_out)
    return df_m



#-----------------------#
# Plot results

def plot_coding_error_rates():
    path_excel = PATH_OUT.joinpath("Cantometrics_Coding_Errors.xlsx")
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

    fig.savefig(PATH_OUT.joinpath(f"coding_errors.pdf"), bbox_inches='tight')
    fig.savefig(PATH_OUT.joinpath(f"coding_errors.png"), bbox_inches='tight')



if __name__ == "__main__":
    # Load and process GJ codings
    df = val_io.load_codings()
    df = val_io.process_codings(df)

    # Compare cantometrics codings and predicted segment classes
    check_segment_classes(df, threshold=0.5, save=True)

    # Note that at this point, we checked the recordings that were
    # flagged for errors; thus when checking melodic range, we could
    # already exclude those recordings that were already checked

    # Part of this process involved creating "Cantometrics_Coding_Errors.xlsx", 
    # which is needed for the rest of the code to run. 
    # The finished version of this file is located in "/validation/automatic_screening",
    # and it is only modified manually

    melodic_range_check(df, save=True)

    # Check codings for consistency
    more_codings_check_inst(df, save=True)
    more_codings_check_vocal(df, save=True)
    more_codings_check_rubato(df, save=True)
    more_codings_check_poly(df, save=True)

    # Check for missing codings
    find_missing_codings(df, save=True)

    # Plot error rate for the segmentation class
    # and melodic range analyses
    plot_coding_error_rates()


