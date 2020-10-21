from collections import defaultdict
from itertools import product

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.metrics import cohen_kappa_score


names = ['Sato', 'Yuan', 'Daikoku', 'Tomokane', 'Chiba', 'Ozaki', 'Ding']
correct = '"Correct"(no multicodings)'
feats = [f"Line {i}" for i in range(1, 38)]

# Feature descriptions
feat_key = {f"Line {k}": v for k, v in zip(*pd.read_excel('WeightedKappacantoIRRModified.xlsx').loc[:, ['LineNumber', 'Variable name']].values.T)}


# Load the student ratings
def load_ratings():
    ratings = {}
    for n in names:
        df = pd.read_excel('CantometricsConsensusTapeData.xlsx', sheet_name=n).loc[18:47].reset_index()
        for f in feats:
            df[f] = df[f].apply(lambda x: int(''.join([y for y in x if y.isdigit()])) if isinstance(x, str) else x)
            df[f] = df[f].astype(int)
        ratings[n] = df
    return ratings


# Load the ratings given by Pat as correct
def load_correct_ratings():
    df = pd.read_excel('CantometricsConsensusTapeData.xlsx', sheet_name=correct).loc[18:47].reset_index()
    for f in feats:
        df[f] = df[f].astype(int)
    return df


# Quick function to get the number of valid encodings for each variable (to be deprecated)
# Should be fully replaced by "load_correct_codings"
def get_feat_weights():
    ratings = load_ratings()
    all_ratings = pd.concat(ratings.values())
    return {f: sorted(all_ratings[f].astype(int).unique()) for f in feats}


# Load the full coding key
def load_correct_codings():
    df = pd.read_csv("canto_codings_metadata.csv")
    key = defaultdict(list)
    for row in df.itertuples():
        key[f'Line {row[1]}'].append(int(row[2]))
    return key


def load_coding_desc():
    df = pd.read_csv("canto_codings_metadata.csv")
    key = defaultdict(list)
    for row in df.itertuples():
        key[f'Line {row[1]}'].append(row[4])
    return key


# Find any invalid codings
def find_errors(ratings, fix=False):
    feat_codes = load_correct_codings()
    errors = []
    for n in names:
        for f in feats:
            for i, r in zip(ratings[n].index, ratings[n][f]):
                if r not in feat_codes[f]:
                    errors.append((n, f, i, r))
                    if fix:
                        ratings[n].loc[i, f] = 'NA'
    if fix:
        return errors, ratings
    else:
        return errors



# I guess you would typically call this Likert rather than equidistant
def convert_ratings_to_equidistant_scale(ratings):
    df = ratings.copy()
    feat_codes = load_correct_codings()
    key = {f:{code:i for i, code in enumerate(v)} for f, v in feat_codes.items()}
    for n in names:
        for f in feats:
            for i, r in zip(df[n].index, df[n][f]):
                if r != 'NA':
                    df[n].loc[i, f] = key[f][r]
    return df


# Get the mean and standard deviation of each variable for each song
# across all raters
def get_mean_vals(ratings, feat_weights):
    mean_ratings = ratings['Sato'].copy()
    for f in feats:
        mean_ratings[f] = [np.mean([ratings[n].loc[i,f] for n in names if ratings[n].loc[i,f] != 'NA']) for i in range(30)]

    feat_std = {}
    for f in feats:
        feat_std[f] = np.sqrt(1./(30*6) * np.sum([(r-rm)**2  for n in names for r, rm in zip(ratings[n][f], mean_ratings[f]) if r != 'NA']))
#       feat_std[f] /= len(feat_weights[f])
    return mean_ratings, feat_std


# Get the standard deviation of each variable across all songs
# and all raters
def get_mean_variation(all_ratings):
    return {f: np.std([np.mean([x for x in ratings[n][f] if isinstance(x, int)]) for n in names]) for f in feats}


# Get kappa
# Don't know what I was thinking when I named this
# If the weights are on a Likert scale, I probably don't need the "feat_weights" parameter
def analyse_no_assumption(ratings, feat_weights):
    kappa = []
    acc = []
    for f in feats:
        kappa.append([])
        acc.append([])
        for i, j in product(*[range(len(names))]*2):
            if i < j:
                ratings_i = ratings[names[i]][f].values
                ratings_j = ratings[names[j]][f].values
                idx = [k for k in range(len(ratings_i)) if ratings_i[k] != 'NA' and ratings_j[k] != 'NA']
                kappa[-1].append(cohen_kappa_score(ratings_i[idx].astype(int), ratings_j[idx].astype(int), feat_weights[f], weights='quadratic'))
                acc[-1].append(sum(ratings_i[idx].astype(int) == ratings_j[idx].astype(int))/len(idx))
    kappa = np.array(kappa)
    acc = np.array(acc)

    return kappa, acc


def print_outcome(mean_kappa, mean_acc, mean_var, feat_std, feat_prob, srt=0):
    to_srt = [mean_kappa, mean_acc, mean_var, feat_std, feat_prob][srt]
    if isinstance(to_srt, dict):
        idx = [int(x[0].split()[1])-1 for x in sorted(to_srt.items(), key=lambda x:x[1], reverse=True)]
    else:
        idx = np.argsort(to_srt)[::-1]
#   for i in np.argsort(mean_kappa)[::-1]:
    for i in idx:
        k = f"Line {i+1}"
        print(f"{round(mean_kappa[i], 2):5.2f}, {round(mean_acc[i], 2):5.2f}, {round(mean_var[k],2):5.2f}, {round(feat_std[k],2):5.2f}, {round(feat_prob[k],2):5.2f},  {k},  {feat_key[k]}")


# Plot a normal distribution of integers
def plot_discretized_gaussian(mu, sigma, nsamp=10000):
    ran = np.random.normal(mu, sigma, size=nsamp)
    ran = ran.round(0)
    bins = np.arange(min(ran)-.5, max(ran)+1, 1)
    sns.distplot(ran, bins=bins, norm_hist=True, kde=False)


# Probability that the 'correct' encoding is achieved by a single rater
def prob_right_first_try(mu, sigma, nsamp=10000):
    ran = np.random.normal(mu, sigma, size=nsamp)
    ran = ran.round(0)
    return sum(ran==mu) / nsamp


# Probability that the 'correct' encoding +- 1 is achieved by a single rater
def prob_within_one_first_try(mu, sigma, nsamp=10000):
    ran = np.random.normal(mu, sigma, size=nsamp)
    ran = ran.round(0)
    return sum((ran>=mu-1)&(ran<=mu+1)) / nsamp


# Match the probability list to the "feat_std" dictionary
def match_std_to_prob(feat_std, std, prob):
    feat_prob = {}
    for k, v in feat_std.items():
        feat_prob[k] = prob[np.argmin(np.abs(std-v))]
    return feat_prob


def write_output(mean_kappa, mean_acc, mean_var, feat_std, feat_prob1, feat_prob2):
    s1 = "Probability that a single rater will accurately predict the mean"
    s2 = "Probability that a single rater will accurately predict within +-1 of the mean"
    s3 = "Standard devation between raters' mean values"
    with open("accuracy_reliability.csv", 'w') as o:
        o.write(f"variable_no,variable_name,Kappa,Accuracy,{s3},std_dev between all values,{s1},{s2}\n")
        for i in range(37):
            k = f"Line {i+1}"
            o.write(f"{k},{feat_key[k]},{round(mean_kappa[i], 2):5.2f},{round(mean_acc[i], 2):5.2f},{round(mean_var[k],2):5.2f},{round(feat_std[k],2):5.2f},{round(feat_prob1[k],2):5.2f},{round(feat_prob2[k],2):5.2f}\n")
    


def confusion_matrix(ratings, norm=False):
    if not norm:
        matrix = {f: np.zeros((13,13), float) for f in feats}
    else:
        feat_codes = load_correct_codings()
        matrix = {f: np.zeros([len(feat_codes[f])]*2, float) for f in feats}
    
    for f in feats:
        for i in range(30):
            for j in range(len(names)-1):
                for k in range(j+1, len(names)):
                    try:
                        l, m = ratings[names[j]].loc[i, f] - 1, ratings[names[k]].loc[i, f] - 1
                        matrix[f][l,m] += 1
                        matrix[f][m,l] += 1
                    except TypeError:
                        pass
#                       print('Invalid coding: ', ratings[names[j]].loc[i, f], ratings[names[k]].loc[i, f])
    return matrix


def plot_confusion_matrix(conf_mat, N, fig='', ax=''):
    mat = np.copy(conf_mat[f"Line {N}"])
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
    title = '_'.join([s[:3] for s in feat_key[f"Line {N}"].lower().split() if s not in ['of', 'the']])
    ax.set_title(title[:20])


def plot_all_conf_mat(conf_mat):
    fig, ax = plt.subplots(6,6)
    ax = ax.reshape(ax.size)
    fig.subplots_adjust(hspace=0.4)
    for i in range(36):
        plot_confusion_matrix(conf_mat, i+1, fig=fig, ax=ax[i])
        ax[i].set_ylabel(f"Line {i+1}")
    





