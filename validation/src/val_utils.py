"""

General-purpose functions

"""

import numpy as np

def int_to_bin_vec(integer, n=13):
    vec = np.zeros(n, dtype=bool)
    for i in range(n,0,-1):
        if integer == 0:
            return vec
        if integer >= 2**i:
            vec[i-1] = True
            integer -= 2**i
    return vec


def is_coding_allowed(integer, n=13):
    for i in range(n,0,-1):
        if integer == 0:
            return True
        if integer >= 2**i:
            integer -= 2**i
    return True if integer == 0 else False
    

def evaluate_coding(bin_vec):
    idx = np.where(bin_vec)[0]
    if len(idx)==1:
        return idx[0] + 1
    else:
        return None



