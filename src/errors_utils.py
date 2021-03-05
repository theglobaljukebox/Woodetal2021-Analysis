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


