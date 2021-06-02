"""
Contains code for:
    Comparing Cantometrics codings with computational analyses
    Checking Cantometrics codings for consistency

Requires:
    f0 estimation using pYIN to be saved in numpy binary files (.npy)
    Excel table containing cantometrics codings by two raters and their consensus ratings

 John M. McBride 03/2021

"""

from collections import Counter, defaultdict
from itertools import product

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.stats import entropy, pearsonr
from sklearn.metrics import confusion_matrix

from val_io import PATH_BASE, PATH_DATA, PATH_OUT
import val_io
from val_utils import int_to_bin_vec, evaluate_coding


FILENAME = "Cantometrics 30 random song reliability codings final.xlsx"
sheets_excel = ['Pat Anna consensus', 'Pat solo', 'Anna solo']
sheets = ['cons', 'pat', 'anna']
feats = list(range(1, 38))
feat_name = [l.strip('\n') for l in open(PATH_DATA.joinpath('variable_names.txt'), 'r')]

# Abstract codings (subjective opinion; upper bound is not well-defined)
abstract = [23, 26, 27, 28, 29, 30, 31, 34, 35]           
abstract_idx = np.array(abstract) - 1
non_abs_idx = np.array([i for i in range(37) if i not in abstract_idx])


### This loads all 4 sets of codings:
### Pat, Anna, Consensus, Original
def load_ratings():
    ratings = {}
    for s1, s2 in zip(sheets_excel, sheets):
        df = pd.read_excel(PATH_BASE.joinpath("Inter-rater reliability", FILENAME), sheet_name=s1)
        df = df.rename(columns={'canto_coding_id':'song_id'})
        ratings[s2] = df
    ratings['original'] = load_original_codings(ratings['cons'])
    return ratings


### These are the codings in the GlobakJukebox dataset
def load_original_codings(df_cons):
#   path = "/home/johnmcbride/projects/BirdSongSpeech/Data/GlobalJukebox/canto_codings.xlsx"
#   codings = pd.read_excel(path, sheet_name='canto_codings')
    codings = val_io.load_codings()
    cols = ["Recording ID for 30 random Cantometric recordings"] + list(range(1,38))
    out = pd.DataFrame(columns=cols)
    for rec in df_cons['song_id']:
        idx = codings.loc[(codings.song_id==rec)].index
        if len(idx):
            c = [evaluate_coding(int_to_bin_vec(codings.loc[idx[0], f"line_{i}"])) for i in range(1,38)]
            out.loc[len(out)] = [rec] + c
        else:
            out.loc[len(out)] = [rec] + [None] * (len(cols) - 1)
    return out


# Load the full coding key
def load_coding_key():
    df = pd.read_csv(PATH_DATA.joinpath("canto_codings_metadata.csv"))
    key = defaultdict(list)
    for row in df.itertuples():
        key[row[1]].append(int(row[2]))
    return key



### When using scipy.stats, I became aware of the fact that 
### when applying quadratic weights, it assumes how close
### categories are by simply ordering the ones that are provided.
### Thus, if a variable has 5 possible answers (1, 4, 7, 10, 13)
### and only 3 are given (1, 4, 13), it will weight a 4-13 error
### the same as a 1-3 error.

### So I rewrote the algorithm given in scipy.stats to deal with
### the fact that each variable has N possible answers, and write 
### an appropriate weighting scheme based on the actual possible answers.
### These next two functions take care of that.

### However, I found that there is practically no difference in the
### value of kappa
def confusion_matrix_pair(Y1, Y2, classes=None):
    if isinstance(classes, type(None)):
        classes = sorted(list(set(list(Y1) + list(Y2))))
    matrix = np.zeros([len(classes)]*2, float)
    key = {k: i for i, k in enumerate(classes)}
    for y1, y2 in zip(Y1, Y2):
        if np.all(np.isfinite([y1, y2])):
            l, m = key[y1], key[y2]
            matrix[l,m] += 1
    return matrix


def cohen_kappa_score(y1, y2, weights=None, classes=None):
    confusion = confusion_matrix_pair(y1, y2, classes=classes)
    n_classes = confusion.shape[0]
    sum0 = np.sum(confusion, axis=0)
    sum1 = np.sum(confusion, axis=1)
    expected = np.outer(sum0, sum1) / np.sum(sum0)

    if weights is None:
        w_mat = np.ones([n_classes, n_classes], dtype=int)
        w_mat.flat[:: n_classes + 1] = 0
    elif weights == "linear" or weights == "quadratic":
        w_mat = np.zeros([n_classes, n_classes], dtype=int)
        w_mat += np.arange(n_classes)
        if weights == "linear":
            w_mat = np.abs(w_mat - w_mat.T)
        else:
            w_mat = (w_mat - w_mat.T) ** 2

    k = np.sum(w_mat * confusion) / np.sum(w_mat * expected)
    return 1 - k


# Get kappa
def get_kappa(ratings, feat_weights=''):
    kappa = []
    acc = []
    sheets = list(ratings.keys())
    for f in feats:
        kappa.append([])
        acc.append([])
        for i, j in product(*[range(len(sheets))]*2):
            if i < j:
                ratings_i = ratings[sheets[i]][f].values
                ratings_j = ratings[sheets[j]][f].values
                idx = [k for k in range(len(ratings_i)) if np.isfinite(ratings_i[k]) and np.isfinite(ratings_j[k])]
                if feat_weights != '':
#                   kappa[-1].append(cohen_kappa_score(ratings_i[idx].astype(int), ratings_j[idx].astype(int), classes=feat_weights[f], weights='quadratic'))
                    kappa[-1].append(cohen_kappa_score(ratings_i[idx], ratings_j[idx], classes=feat_weights[f], weights='quadratic'))
                else:
#                   kappa[-1].append(cohen_kappa_score(ratings_i[idx].astype(int), ratings_j[idx].astype(int), weights='quadratic'))
                    kappa[-1].append(cohen_kappa_score(ratings_i[idx], ratings_j[idx], weights='quadratic'))
#               acc[-1].append(sum(ratings_i[idx].astype(int) == ratings_j[idx].astype(int))/len(idx))
                acc[-1].append(sum(ratings_i[idx] == ratings_j[idx])/len(idx))
    kappa = np.array(kappa)
    acc = np.array(acc)

    return kappa, acc



# Get the mean and standard deviation of each variable for each song
# across all raters
def get_mean_vals(ratings, feat_weights, norm=False, sheets=''):
    classes = load_coding_key()
    key = {f: {c:i for i, c in enumerate(vals)} for f, vals in classes.items()}
    if isinstance(sheets, str):
        sheets = ['original', 'pat', 'anna']
#       sheets = ['original', 'cons']
    mean_ratings = ratings['cons'].copy()
    for f in feats:
        mean_ratings[f] = [np.mean([key[f][ratings[n].loc[i,f]] for n in sheets if np.isfinite(ratings[n].loc[i,f])]) for i in range(30)]

    feat_std = {}
    for f in feats:
        total_vals = len([r for n in sheets for r, rm in zip(ratings[n][f], mean_ratings[f]) if np.isfinite(r)])
        feat_std[f] = np.sqrt(np.sum([(key[f][r]-rm)**2  for n in sheets for r, rm in zip(ratings[n][f], mean_ratings[f]) if np.isfinite(r)]) / total_vals)
        if norm:
            feat_std[f] /= len(feat_weights[f])
    return mean_ratings, feat_std


# Get Pearson correlation coefficient for each variable
def get_pearson(ratings, sheets=''):
    if isinstance(sheets, str):
        sheets = ['original', 'pat', 'anna']
#       sheets = ['original', 'cons']
    score = []
    for f in feats:
        Y = [ratings[s][f].values for s in sheets]
        idx = (np.isfinite(Y[0])) & (np.isfinite(Y[1]))
        score.append(pearsonr(Y[0][idx], Y[1][idx]))
    r_vals, p_vals = np.array(score).T
    return r_vals, p_vals



# Plot accuracy, kappa, and standard deviation for each variable
def plot_acc(kappa, acc, std):
    fig, ax = plt.subplots(figsize=(9,5))
    fs = 14
    ax2 = ax.twiny()
    cols = sns.color_palette()
    ax.plot(kappa, acc*100, 'o', c=cols[0], fillstyle='none')
    ax2.plot(std, acc*100, 's', c=cols[1], fillstyle='none', mew=2)

    ax.set_xlabel("Kappa", fontsize=fs)
    ax.set_ylabel("% Agreement", fontsize=fs)
    ax2.set_xlabel("Standard deviation", fontsize=fs)

    ax.tick_params(direction='in', labelsize=fs-2)
    ax2.tick_params(direction='in', labelsize=fs-2)

    handles = [Line2D([], [], linestyle=' ', marker=p, fillstyle='none', mew=2, color=cols[i]) for i, p in zip(range(2), 'os')]
    lbls = ['Kappa', 'Standard Deviation']
    ax.legend(handles, lbls, bbox_to_anchor=(0.2, 1.3), frameon=False)

    fig.tight_layout()
    fig.savefig(PATH_OUT.joinpath(f"irr_score.pdf"), bbox_inches='tight')
    fig.savefig(PATH_OUT.joinpath(f"irr_score.png"), bbox_inches='tight')



# Create an array of all relative metrics
def get_all_scores_for_table():
    ratings = load_ratings()
    feat_weights = load_coding_key()

    kappa, acc = get_kappa(ratings)
    kappa_cons, acc_cons = kappa[:,2], acc[:,2] * 100
    kappa_mean, acc_mean = kappa[:,3:].mean(axis=1), acc[:,3:].mean(axis=1) * 100

    all_scores = [[kappa_mean, acc_mean],
                  [kappa_cons, acc_cons]]

    sheets = [['original', 'pat', 'anna'], ['original', 'cons']]
    for i, s in enumerate(sheets):
        mean_ratings, feat_std = get_mean_vals(ratings, feat_weights, sheets=s)
        std = np.array(list(feat_std.values()))
        mean_ratingsn, feat_stdn = get_mean_vals(ratings, feat_weights, sheets=s, norm=True)
        stdn = np.array(list(feat_stdn.values()))
        r_vals, p_vals = get_pearson(ratings, sheets=s)

        all_scores[i].extend([std, stdn, r_vals, p_vals])

    return np.array(all_scores)


# Print all relative metrics
def print_all_scores_for_table(all_scores):
    ttls = ['Mean', 'Consensus']
    print(''.join([f"{t:35s}" for t in ttls]))
    print('  '.join(["Kappa  Agree  Stdev  StdevN r_val  p_val"]*2))
    for i in range(37):
        fmts = '  '.join([f"{all_scores[k,j,i]:5.2f}" for k in [0,1] for j in range(6)])
        print(fmts)




# NOT USED IN PAPER
# Get confusion matrices for each variable
def confusion_matrix_all(ratings, sheets=''):
    classes = load_coding_key()
    key = {f: {c:i for i, c in enumerate(vals)} for f, vals in classes.items()}
    matrix = {f: np.zeros([len(key[f])]*2, int) for f in feats}
    
    if isinstance(sheets, str):
        sheets = ['original', 'pat', 'anna']
#       sheets = ['original', 'cons']

    for f in feats:
        for i in range(30):
            for j in range(len(sheets)-1):
                for k in range(j+1, len(sheets)):
                    try:
                        l = key[f].get(ratings[sheets[j]].loc[i, f], np.nan)
                        m = key[f].get(ratings[sheets[k]].loc[i, f], np.nan)
                        if not np.all(np.isfinite([l, m])):
                            continue
                        l, m = int(l), int(m)
                        matrix[f][l,m] += 1
                        matrix[f][m,l] += 1
                    except TypeError:
                        pass
                        print('Invalid coding: ', ratings[sheets[j]].loc[i, f], ratings[sheets[k]].loc[i, f])
    return matrix


# NOT USED IN PAPER
# Plot confusion matrices for each variable
def plot_confusion_matrix(conf_mat, N, fig='', ax='', dia=False):
    mat = np.copy(conf_mat[N])
    if dia:
        np.fill_diagonal(mat, 0)
    if isinstance(fig, str):
        fig, ax = plt.subplots()
    im = ax.imshow(mat)
    fig.colorbar(im, ax=ax)
    ax.invert_yaxis()
    ax.set_xticks(range(len(mat)))
    ax.set_yticks(range(len(mat)))
    ax.set_xticklabels(range(len(mat)))
    ax.set_yticklabels(range(len(mat)))


# NOT USED IN PAPER
# Plot confusion matrices for each variable
def plot_all_conf_mat(conf_mat, dia=False):
    fig, ax = plt.subplots(6,6)
    ax = ax.reshape(ax.size)
    fig.subplots_adjust(hspace=0.4)
    for i in range(36):
        plot_confusion_matrix(conf_mat, i+1, fig=fig, ax=ax[i], dia=dia)
        ax[i].set_ylabel(f"Line {i+1}")


# NOT USED IN PAPER
# Calcualte the ratio of the entropy of each variable, to the
# maximum possible entropy given the number of answers
def entropy_ratio(ratings, feat_weight):
    e1, e2 = [], []
    count = []
    for f in feats:
        idx = ratings['original'][f].notnull()
        e1.append(entropy(list(Counter(ratings['original'].loc[idx, f]).values())) / np.log(len(feat_weight[f])))
        e2.append(entropy(list(Counter(ratings['cons'][f]).values())) / np.log(len(feat_weight[f])))
        count.append(Counter(x for s in ['original', 'cons'] for x in ratings[s].loc[idx, f]))
#       print(f, f-1 in abstract_idx, round(e1[-1], 2), round(e2[-1], 2))
    e1, e2 = np.array(e1), np.array(e2)
    return e1, e2, count


if __name__ == "__main__":


    # Load all metrics and print them to screen
    scores = get_all_scores_for_table()
    print_all_scores_for_table(scores)


    # Pick metrics pertaining to consensus comparisons
    # and plot
    kappa = scores[1,0]
    acc   = scores[1,1]
    std   = scores[1,2]
    plot_acc(kappa, acc, std)



