"""
Contains code for:
    Creating PDFs for visual inspection to validate automated analyses
    Estimating how many GJ samples need to be manually checked, 
        given the variance in the sample



"""
from fractions import Fraction
from itertools import product
from pathlib import Path
import os
import sys
import time


from matplotlib.gridspec import GridSpec
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from multiprocessing import Pool
import numpy as np
from palettable.colorbrewer.qualitative import Paired_12, Set2_8
import pandas as pd
from scipy.signal import spectrogram
import seaborn as sns


import audio_tools
import parser_globaljukebox as GJ



N_PROC = 25

PATH_BASE = Path("INSERT_PATH_HERE")
PATH_VAL  = PATH_BASE.joinpath('Validation')
PATH_DATA = PATH_BASE.joinpath('Data')
PATH_GJ   = PATH_DATA.joinpath('GlobalJukebox', 'audio')


#-----------------------------------------------------------#
#  Creating PDFs for visual inspection


def load_sample_list(filename="GJ_samples.csv"):
    return pd.read_csv(PATH_VAL.joinpath(filename))


def load_file(f_id):
    path = PATH_GJ.joinpath(f"{f_id}.mp3")
    return audio_tools.load_mp3file(str(path))


def export_to_pdf(f_id, fr, wav, tm, mel, dt=10, tmax=60, fmax=1000, window=2048, hop=256):
    N = int(min(tmax/dt, tm[-1]/dt+1))
    h_ratio = ([1,1,1,.7]*N)[:-1]
    fig = plt.figure(figsize=(20,6*N))
    gs  = GridSpec(N*4-1,1, hspace=0.0, height_ratios=h_ratio)
    ax = [fig.add_subplot(gs[i + int(i/3),0]) for i in range(N*3)] 

    f, t, spec = spectrogram(wav, fr, nperseg=window, noverlap=hop)
#   fig, ax = plt.subplots(3,1, sharex=True)
    for i in range(N):
        xidx = (t>=i*dt) & (t<(i+1)*dt)
        yidx = (f>=0) & (f<fmax)
        spec_slice = spec[yidx]
        spec_slice = spec_slice[:,xidx]
        ax[3*i].imshow(np.log(spec_slice), cmap='Greys', extent=[i*dt, (i+1)*dt, fmax, 0], aspect='auto')
        ax[3*i].invert_yaxis()

        idx = (tm>=i*dt) & (tm<(i+1)*dt)
        ax[3*i+1].plot(tm[idx], mel[idx], 'o', ms=2)
        Y3 = np.log(mel[idx]/mel[0])/np.log(2)*1200.
        Y3 = np.array(Y3/100, dtype=int)
        ax[3*i+2].plot(tm[idx], Y3, 'o', ms=2)

        ymin, ymax = int(min(Y3)), int(max(Y3))
        for y in range(ymin, ymax):
            if abs(y)%12==0:
                ax[3*i+2].plot([i*dt, (i+1)*dt], [y]*2, '-', lw=0.2, color='k', alpha=0.7)
            else:
                ax[3*i+2].plot([i*dt, (i+1)*dt], [y]*2, '-', lw=0.2, color='grey', alpha=0.5)

        ax[3*i+1].set_xlim(i*dt, (i+1)*dt)
        ax[3*i+2].set_xlim(i*dt, (i+1)*dt)
        ax[3*i].set_xticks([])
        ax[3*i+1].set_xticks([])
        ax[3*i+2].set_xticks(np.arange(i*dt, (i+1)*dt+1, 1))
    
        ax[3*i].set_ylabel('frequency (Hz)')
        ax[3*i+1].set_ylabel('frequency (Hz)')
        ax[3*i+2].set_ylabel('note (semitones)')
        ax[3*i+2].set_xlabel('time (seconds)')

    fig.savefig(PATH_VAL.joinpath(f"{f_id}.pdf"), bbox_inches='tight')


def load_data_and_export(f_id):
    fr, wav = load_file(f_id)
    tm, mel = audio_tools.extract_pitch_from_wav(fr, wav)
    tm = tm[mel>0]
    mel = mel[mel>0]

    export_to_pdf(f_id, fr, wav, tm, mel)

    return f_id, fr, wav, tm, mel
    



#-----------------------------------------------------------#
#  Accuracy of estimated error rates for a given sample size


def pick_codings_to_sample(df):
    codings = [('CV_4', 3, 'Mono'),
               ('CV_4', 6, 'Uni'),
               ('CV_4', 9, 'Hetero'),
               ('CV_4', 12, 'Poly'),
               ('CV_5', 0, 'Solo'),
               ('CV_5', 3, 'Blend 1'),
               ('CV_5', 6, 'Blend 2'),
               ('CV_5', 9, 'Blend 3'),
               ('CV_5', 12, 'Blend max')]
    for i, (code, name) in product(range(13), [['CV_3', 'MelForm'], ['CV_16', 'SocOrg']]):
        codings.append((code, i, f"{name}_{i}"))
    print(codings)
    
    data = {}
    for code, idx, name in codings:
        data[name] = df[code].apply(lambda x: x[idx])
    return pd.DataFrame(data)


### Simulation to estimate the observed error rate as a function of
### sample size, when given a known error rate
def estimating_error_rate_single_observable(S, err, n_samp=1000, plot=True, fig='', ax=''):
    observable = np.zeros(S, bool)
    observable[np.random.choice(range(S), size=int(S*err), replace=False)] = True
#   N_arr = (S * np.array([0.0025, 0.005, 0.01, 0.025, 0.05])).astype(int)
    N_arr = (S * np.array([0.002, 0.003, 0.005, 0.0075, 0.01, 0.02, 0.03, 0.04, 0.05])).astype(int)
    quantiles = [0.05, 0.32, 0.68, 0.98]
    CI = []
    for n in N_arr:
        hits = np.array([sum(observable[np.random.choice(range(S), size=n, replace=False)])/n for i in range(n_samp)])
        CI.append(np.quantile(hits, quantiles))
    CI = np.array(CI).T

    if plot:
        if isinstance(ax, str):
            fig, ax = plt.subplots()
        patt = [':', '--', '--', ':']
        for i, p in enumerate(patt):
            ax.plot(N_arr, CI[i], p, c='k')
        ax.plot(N_arr, [err]*len(N_arr), '-k')
        ax.set_xlabel('Sample size')
        ax.set_ylabel('Error')
        ax.set_title(f"Error rate: {err}")
        handles = [Line2D([], [], ls=p, c='k') for p in ['-', '--', ':']]
        ax.legend(handles, ['Actual Error', 'Est. 68% CI', 'Est. 95% CI'], frameon=False, loc='upper right')
        ax.grid(which='both')

    return N_arr, CI 


def count_hits(inputs):
    np.random.seed(int(str(time.time()).split('.')[1]))
    observable, n = inputs
    S, n_obs = observable.shape
    return np.sum(observable[np.random.choice(range(S), size=n, replace=False)], axis=0)/n


### Extending the simulation to multi-dimensional data
def estimating_error_rate_multi_observable(S, n_obs, e_mean, e_var, n_samp=1000):
    observable = np.zeros((S, n_obs), bool)
    err = np.random.lognormal(e_mean, e_var, size=n_obs)
    for i in range(n_obs):
        observable[np.random.choice(range(S), size=int(S*err[i]), replace=False), i] = True
    N_arr = (S * np.array([0.005, 0.01, 0.025, 0.05])).astype(int)
    quantiles = [0.05, 0.32, 0.68, 0.98]
    CI = []
    for n in N_arr:
        with Pool(min(N_PROC, n)) as pool:
            hits = np.array(list(pool.imap_unordered(count_hits, [(observable, n)]*n_samp)))
        CI.append(np.quantile(hits, quantiles, axis=0))
        
    CI = np.array(CI).T

    return N_arr, err, CI 


### Plotting simulation results
def plot_error_estimates(S=6000, n_obs=40):
    fig, ax = plt.subplots(2,2)
    ax = ax.reshape(ax.size)
    for i, e in enumerate([0.005, 0.01, 0.05, 0.1]):
        N_arr, err = estimating_error_rate_single_observable(S, e, fig=fig, ax=ax[i])

    fig, ax = plt.subplots()
    N_arr, err, CI = estimating_error_rate_multi_observable(S, n_obs, -3, 0.5)
    sns.distplot(err, ax=ax)
    ax.set_xlabel("Error distribution")
    ax.set_ylabel("Density")
    ax.set_title(f"Actual errors for {n_obs} observables")

    fig, ax = plt.subplots(2,2)
    ax = ax.reshape(ax.size)
    lbls = ['95%', '68%', '68%', '95%']
    for i in range(4):
        for j in range(4):
            sns.distplot(CI[:,j,i]/err, ax=ax[i], label=lbls[j])
        ax[i].legend(loc='best', frameon=False)
        ax[i].set_xlabel("error CI / actual error")
        ax[i].set_ylabel("Density")
        ax[i].set_title(f"Sample size: {N_arr[i]}")


# Analytical probability of obtaining an observed number of errors
# S: full sample size; E: total errors; n: subsample size; e: observed errors
def probability_of_observation(S, E, n, e):
    err_term = Fraction(np.math.factorial(E), np.math.factorial(E-e)) 
    nonerr_term = Fraction(np.math.factorial(S-E), np.math.factorial(S-E-n+e)) 
    denom = Fraction(np.math.factorial(S), np.math.factorial(S-n))
    return float(err_term * nonerr_term / denom * number_of_observations(e, n))


def number_of_observations(e, n):
    return Fraction(np.math.factorial(n), np.math.factorial(e)) / np.math.factorial(n-e)


# Probability of inferred error rate as a function of sample size and observed errors
def inferring_error_rate_from_observation(S=6000):
    fig, ax = plt.subplots(3, 1)
    fig.subplots_adjust(hspace=0.3)
    all_prob = []
    for i, (a, N) in enumerate(zip(ax, [30, 100, 300])):
        for e_rate in [2/30., 0.1, 0.2]:
            e = int(N * e_rate)
            err_range = np.arange(e, S-N+e)
            with Pool(N_PROC) as pool:
                prob = np.array(pool.starmap(probability_of_observation, [(S, E, N, e) for E in err_range]))
            a.plot(err_range/S, prob/prob.sum(), label=f"errors found: {e}")
            all_prob.append((err_range/S, prob))
        a.set_title(f"Sample size: {N}")
        a.set_xlabel("Actual error rate")
        a.set_ylabel("Probability")
        a.legend(loc='best', frameon=False)
        a.set_xlim(0, 0.5)
        a.grid()
#       a.set_ylim(0, 1)
    return all_prob


def error_rate_ci(err_range, prob, S=6000):
    err_range = err_range / S
    cum_prob = np.cumsum(prob / prob.sum())
    return [err_range[np.argmin(np.abs(cum_prob-x))] for x in [0.025, 0.975]]
    
    
#-----------------------------------------------------------#
#  Automatic identification of coding errors in Global Jukebox


def run_matija_checks(df, threshold=0.5, save=False):
    df = df.copy()

    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & \
                 (df.pitch_extracted) & (df.inst=='none') & (df['Solo']<threshold)

    poly_vocal = (df.vocal.apply(lambda x: x in ['poly', 'hetero', 'random'])) & (df.no_inst1) & \
                 (df.no_inst2) & (df.pitch_extracted) & (df.inst=='none') & (df['Group']<threshold)

    inst = (df.inst.apply(lambda x: 'none' not in x)) & (df.pitch_extracted) & (df['Inst']<threshold)

    lbls = ['SoloVocal', 'ChorusVocal', 'Inst']
    cols = ['Solo', 'Group', 'Inst', 'Speech']
    cols2print = ['audio_file_id', 'vocal', 'inst', 'no_inst1', 'no_inst2'] + cols

    for c in cols:
        df[c] = df[c].apply(lambda x:round(x,2))

    BASE = PATH_VAL.joinpath("automatic_screening")
    out_dict = {}
    for idx, lbl, c in zip([mono_vocal, poly_vocal, inst], lbls, cols):
        below_threshold = df.loc[idx, cols2print].sort_values(by=c)
        out_dict[lbl] = below_threshold
        if save:
            below_threshol.to_csv(BASE.joinpath(f"check_is_{lbl}.csv"))

    return out_dict


def get_melodic_range(f_id):
    path = PATH_DATA.joinpath("GlobalJukebox", "pitch_trace", f"{f_id}.npy")
    try:
        data = np.load(path)
        mel = data[1]
        mel = mel[mel>0]
        return np.log2(max(mel) / min(mel)) * 1200

    except:
        return 0
    

def melodic_range_check(df):
    df = df.loc[df.pitch_extracted]
    cols2print = ['audio_file_id', 'vocal', 'inst', 'no_inst1', 'no_inst2', 'range']
    df['range'] = df.audio_file_id.apply(get_melodic_range)


    BASE = PATH_VAL.joinpath("automatic_screening")
    
    path_excel = BASE.joinpath("Cantometrics_Coding_Errors.xlsx")
    idx_already_checked = []
    for sheet in ['SoloVocal', 'GroupVocal', 'Inst']:
        xls = pd.read_excel(path_excel, sheet_name=sheet)
        idx_already_checked.extend(list(xls.loc[xls['Correct Coding?'].notnull(), 'Unnamed: 0']))
    idx_to_drop = set(idx_already_checked).intersection(set(df.index))

    df = df.drop(index=list(idx_to_drop))

    mono_vocal = (df.vocal=='mono') & (df.no_inst1) & (df.no_inst2) & (df.inst=='none')
    path_out = BASE.joinpath("melodic_range.csv")
    df.loc[mono_vocal, cols2print].sort_values(by='range', ascending=False).to_csv(path_out)

    return df



if __name__ == "__main__":
    # Load and process GJ codings
    codings = GJ.convert_GJ_codings_to_dataframe()
    codings = GJ.process_GJ_codings(codings)
    codings = GJ.add_segmentation_labels(codings)

    # Run checks
    run_matija_checks(codings, threshold=0.5)
    melodic_range_check(codings)



