"""

Tools for reading and processing audio files and extracting pitches

"""

from itertools import product
import os
from pathlib import Path
import sys

sys.path.insert(0, "./pypYINmaster/src")
sys.path.insert(0, "/home/jmcbride/projects/PitchAnalysis/Yin-master")

import crepe
import essentia.standard as ess
from librosa.core import resample, stft
import matplotlib.pyplot as plt
import numpy as np
from palettable.colorbrewer.qualitative import Paired_12
import pandas as pd
from pydub import AudioSegment
import seaborn as sns
from scipy.interpolate import CubicSpline
from scipy.io import wavfile
from scipy.signal import argrelmax, argrelmin, argrelextrema
from scipy.spatial import distance_matrix
from scipy.stats import lognorm, kstest, ks_2samp, linregress
from scipy.optimize import curve_fit
from sklearn.cluster import DBSCAN
#mport tensorflow._api.v2.compat.v1 as tf
import vamp

from pYINmain import PyinMain
#mport yin

from bss_io import PATH_BASE
import plots


PATH_FIG  = PATH_BASE.joinpath('Figures')

PATH_BIRD_DISC = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Bird songs (17+13)/discrete (17)")
PATH_BIRD_NONDISC = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Bird songs (17+13)/non-discrete (13)")

PATH_HMN = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Human songs (11)")
PATH_SPE = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Sub_sample/Speech (12)")

PATH_HPB = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/birthday_poor-pitch_singers")
PATH_INST = Path("/home/jmcbride/projects/BirdSongSpeech/Data/Recordings/Inst_test")


pitch_algorithms = ['pypyin', 'pyin_smooth', 'pyin', 'pyin_notes', 'crepe', 'yin', 'melodia']



def convert_mp3_to_wav_folder(path):
    for f in path.glob('*mp3'):
        convert_mp3_to_wav(f)


def convert_mp3_to_wav(f):
    sound = AudioSegment.from_mp3(f)
    sound.export(os.path.splitext(f)[0] + '.wav', format='wav')


def read_wav(f, return_mono=True):
    fr, wav = wavfile.read(f)
    if len(wav.shape)>1:
        return fr, wav.mean(axis=1)
    else:
        return fr, wav


def load_mp3file(path):
    """MP3 to numpy array"""
    a = AudioSegment.from_mp3(path)
    y = np.array(a.get_array_of_samples())
    if a.channels == 2:
        y = y.reshape((-1, 2))
        y = y.mean(axis=1)
    return a.frame_rate, y


def load_audio(path):
    ext = os.path.splitext(path)[1]
    if ext.lower() == '.mp3':
        return load_mp3file(path)
    elif ext.lower() == '.wav':
        return read_wav(path)


# Recreating essentia.standard.FrameGenerator without essentia
# for better compatibility across platforms
def frame_generator(wav, fr, window=2048, hop=256):
    wav = np.array(wav, dtype=np.float32) / (2.**15 + 1)
    steps = int(wav.size/hop) + 2
    for i in range(steps):
        frame = np.zeros(window, dtype=float)
        wav_start = int(i*hop-window/2)
        wav_end   = int(i*hop+window/2)
        frame_start = max(-wav_start, 0)
        frame_end   = min(window, window - (wav_end - wav.size))
        frame[frame_start:frame_end] = wav[max(0, wav_start):min(wav_end, wav.size)]
        yield frame
        


###############################################################################
####  Homemade pitch-tracking algorithm:
####  based on FFT


def spectrum_by_window(wav, window, hop, fr):
    freq = np.fft.fftfreq(window, 1./fr)
    frames = list(frame_generator(wav, fr, window=window, hop=hop))
    fft = np.abs(np.array([np.fft.fft(frame) for frame in frames]))
    amp = np.array([np.mean(np.abs(frame)) for frame in frames])
    time = np.arange(amp.size) / float(amp.size) * wav.size / fr
    idx = [i for i in range(len(freq)) if 0 < freq[i]]
    return freq[idx], fft[:,idx], time, amp
#   return freq, fft


#ef find_peaks_within_limits(fft, freq, fmin, fmax):
#   idx_out = []
#   for i in range(fft.shape[1]):
#       idx_limit = np.where((fft[:,i]>fmin)&(fft[:,i]<fmax))
#       idx_out.append(idx_limit[np.argmax(fft[idx_limit, i])])
#   return idx_out


def parabolicInterpolation(freq, amp, i):
    if i > 0 and i < len(freq)-1:
        z = np.polyfit(freq[i-1:i+2], amp[i-1:i+2], 2)
        fnew = -z[1] / (2 * z[0])
        anew = np.poly1d(z)(fnew)
        if freq[i-1] < fnew < freq[i+1]:
            return fnew, anew
    return freq[i], amp[i]


def smooth_peak_and_amp(freq, fft, idx):
    peaks = []
    amps = []
    for i, j in enumerate(idx):
        x, y = parabolicInterpolation(freq, fft[i,:], j)
        peaks.append(x)
        amps.append(y)
    return np.array(peaks), np.array(amps)


def peaks_n_amps(fft, freq, fmin=0, fmax=10000):
    idx1 = np.where((freq>=fmin)&(freq<=fmax))[0]
    idx = np.argmax(fft[:, idx1], axis=1) + idx1[0]
    return smooth_peak_and_amp(freq, fft, idx)
#   peaks = freq[idx]
#   amps = fft[list(range(fft.shape[0])), idx]
#   return peaks, amps
    

def stable_pitches(freq, window, tol):
    idx = []
    for i, f in enumerate(freq):
        if sum(freq[max(0, i-window):i+window+1] == f) >= tol:
            idx.append(i)
    return idx, np.array(freq)[idx]


def homemade_pitch_extraction(wav, fr, hop=256, window=2048, lowAmp=10000, fmin=0, fmax=10000):
    freq, fft, t, a = spectrum_by_window(wav, window, hop, fr)
    peaks, amps = peaks_n_amps(fft, freq, fmin=fmin, fmax=fmax)
    return t, peaks, amps
#   idx = np.log10(amps)>lowAmp
#   return t[idx], peaks[idx]
#   idx, stab_pitch = stable_pitches(peaks[amps>lowAmp], 2, 3)
#   return t[idx], stab_pitch


###############################################################################
####  Published pitch-tracking algorithms:


def extract_pitch_from_wav(fr, wav, alg='pyin_smooth', fmin=40, fmax=8000, lowamp=0.1, hop=256, window=2048):
    # Often wav gets read as an int;
    # a lot of this code uses modules written in C, and I guess they
    # are not flexible regarding variable types;
    # this avoids those errors
    wav = wav.astype(float)

    # pYIN
    # Viterbi - smoothed pitch track
    if alg=='pyin_smooth':
        data = vamp.collect(wav, fr, "pyin:pyin", output='smoothedpitchtrack', parameters={'outputunvoiced': 2})
        melody = data['vector'][1]
        tm = np.arange(len(melody)) * float(data['vector'][0])

    # pYIN?
    # No smoothing - just returns the most probable f0candidates
    # i.e., this is probably just YIN...
    elif alg=='pyin':
        f0 = vamp.collect(wav, fr, "pyin:pyin", output='f0candidates')
        prob = vamp.collect(wav, fr, "pyin:pyin", output='f0probs')
        tm, melody = [], []
        for f, p in zip(f0['list'], prob['list']):
            if 'values' in f.keys():
                freq = f['values'][np.argmax(p['values'])]
                if freq <= fmax:
#               if freq <= fmax and p['values'].max()>0.001:
                    tm.append(float(f['timestamp']))
                    melody.append(freq)
        tm = np.array(tm)
        melody = np.array(melody)

    # pYIN
    # Same as "pyin_smooth" but written in python instead of C
    # Gives slightly different results to "pyin_smooth"
    elif alg=='pypyin':
        pyin = PyinMain()
        pyin.initialise(inputSampleRate=fr, lowAmp=lowamp,
                        fmin=fmin, fmax=fmax)
        for i, frame in enumerate(frame_generator(wav, fr, window=window, hop=hop)):
            fs = pyin.process(frame)
        melody = pyin.getSmoothedPitchTrack()
        tm = np.arange(melody.size) * hop / fr

    # Returns note frequncies and onsets
    elif alg=='pyin_notes':
        data = vamp.collect(wav, fr, "pyin:pyin", output='notes')
        tm, melody = [], []
        for d in data['list']:
            tm.extend([d['timestamp'], d['timestamp']+d['duration']])
            melody.extend([d['values'][0]]*2)

    elif alg=='crepe':
        time, frequency, confidence, activation = crepe.predict(wav, fr)
        return time, frequency

    elif alg=='yin':
        p, h, a, t = yin.compute_yin(wav, fr, f0_min=fmin, f0_max=fmax, harmo_thresh=0.1, w_step=32, w_len=1024)
        p, t = np.array(p), np.array(t)
        melody = p[p>0]
        tm = t[p>0]

    elif alg=='melodia':
        data = vamp.collect(wav, fr, "mtg-melodia:melodia")
        melody = data['vector'][1][data['vector'][1]>0]
        tm = (np.arange(len(data['vector'][1])) * float(data['vector'][0]) + 8*128/44100)[data['vector'][1]>0]

    elif alg=='aubio':
        data = vamp.collect(wav, fr, "vamp-aubio:aubiopitch")
        tm = np.array([d['timestamp'] for d in data['list']])
        melody = np.array([d['values'][0] for d in data['list']])

    else:
        raise Exception('Incorrect algorithm name passed as argument')

    return tm, melody


def extract_polyphonic_pitch(fr, wav, alg='silvet'):
    if alg=='silvet':
        data = vamp.collect(wav, fr, "silvet:silvet", parameters={'finetune':1})
        tm, mel = np.array([(d['timestamp'], v) for d in data['list'] for v in d['values']]).T
    return tm, mel


###############################################################################
####  Published silence-tracking algorithms:


def extract_silence_from_wav(fr, wav, alg='aubio'):
    if alg=='aubio':
        data = vamp.collect(wav, fr, "vamp-aubio:aubiosilence")
        return data


def get_tf_segment_input(fr, wav, window=140, fr_resample=22050):
    # Resample to 22,050 Hz
    wav = resample(wav, fr, fr_resample)
    # Short-time Fourier transform
    Df = stft(wav, n_fft=1024, hop_length=315)
    # Reshape according to input specifications
    output = []
    for i in range(int((Df.shape[1])/window)):
        output.append(Df[:, i*140:(i+1)*140].reshape(513,140,1))
    return fr_resample, np.array(output)


def segment_speech_music(fr, wav):
    fr, Df = get_tf_segment_input(fr, wav)
    exportpath = "TMP"
    with tf.Session() as sess:
        tf.saved_model.loader.load(sess, ["scoring-tag"], exportpath)
        predictions = tf.get_default_graph().get_tensor_by_name("predictions:0")
        probabilities = sess.run([predictions], feed_dict={'xinput:0': Df})

    lbls = ['Solo', 'Choir', 'Inst', 'Speech']
    fig, ax = plt.subplots()
    for i, l in enumerate(lbls):
        ax.plot(np.arange(probabilities[0].shape[0])*140*315/fr, probabilities[0][:,i], label=l)
    ax.legend(loc='best', frameon=False)



def extract_notes_pyin(fr, wav):
    out = vamp.collect(wav, fr, "pyin:pyin", output="notes")
    notes = []
    for dic in out['list']:
        start = dic['timestamp']
        end = start + dic['duration']
        pitch = dic['values'][0]
        notes.append(([start, end], [pitch]*2))
    return notes



def pitch_alg_comparison(f):
    fig, ax = plt.subplots(2,2, sharex=True, sharey=True)
    ax = ax.reshape(ax.size)
    fr, wav = read_wav(f)
    cols = np.array(Paired_12.hex_colors)[[1,3,5,7]]
    for i, alg in enumerate(['crepe', 'melodia', 'yin', 'pyin']):
        tm, mel = extract_pitch_from_wav(fr, wav.astype(float), alg=alg)
#       for a in ax:
#           a.plot(tm, mel, 'o', c=cols[i], label=alg, alpha=0.02)
        ax[i].plot(tm, mel, 'o', c=cols[i], label=alg, alpha=1)
    for a in ax:
        a.legend(loc='best', frameon=False)


def get_y_dist(y):
    y = np.array(y)
    dist = np.zeros((y.size, y.size))
    for i in range(y.size-1):
        for j in range(i+1, y.size):
            d = 0.5 * (abs(y[i] - y[j])/y[i] + abs(y[i] - y[j])/y[j])
            dist[i,j] = d
            dist[j,i] = d
    return dist


def extract_pitch_multi_alg(f, fmax=2000, x_cut=5, y_cut=0.1, all=False):
    fr, wav = read_wav(f)
    tm, mel = [], []
    for i, alg in enumerate(['crepe', 'melodia', 'yin', 'pyin']):
        try:
            out = extract_pitch_from_wav(fr, wav.astype(float), alg=alg, fmax=fmax)
            tm.append(np.array(out[0], dtype=float))
            mel.append(out[1])
        except Exception as e:
            print(e)
            print(f)
            tm.append(np.empty())
            mel.append(np.empty())
    d_tm = min([x for t in tm for x in np.diff(t)])
    max_tm = max([max(y) for y in tm])
    X = np.arange(0, max_tm+d_tm, d_tm)
    Y = []
    for i in range(len(X)):
        x_close = []
        y_close = []
        for t, m in zip(tm, mel):
            j = np.argmin(np.abs(t-X[i]))
            if abs(t[j] - X[i]) <= x_cut*d_tm and m[j] != 0:
                x_close.append(t[j])
                y_close.append(m[j])
        if len(x_close)>1:
            clu = DBSCAN(eps=y_cut, min_samples=2).fit(get_y_dist(y_close))
            idx = np.where(clu.labels_==0)[0]
            if len(idx):
                Y.append(np.mean(np.array(y_close)[idx]))
                continue
        Y.append(0)
    Y = np.array(Y)
    if all:
        return tm + [X[Y!=0]], mel + [Y[Y!=0]]
    else:
        return X[Y!=0], Y[Y!=0]


def get_variable_fmin_array(nframes, t, var_min):
    idx = [0]
    for t0, f0 in var_min[1:-1]:
        idx.append(np.argmin(np.abs(t-t0)))
    idx.append(nframes-1)

    f0_min = np.zeros(nframes, dtype=float)
    for i, (t0, f0) in zip(idx, var_min):
        f0_min[i] = f0

    j = 0
    for i in range(1, nframes-1):
        if i in idx[1:]:
            j += 1
            continue
        f0_min[i] = f0_min[idx[j]] + (f0_min[idx[j+1]] - f0_min[idx[j]]) * (i - idx[j]) / (idx[j+1] - idx[j])

    return f0_min


def pyin_test(path, alg='pyin', fmin=40, fmax=1600, hop=256, window=2048, lowAmp=0.10, onset=0.7, prune=0.1, var_min=''):
    fr, wav = read_wav(path)
    audio = ess.MonoLoader(filename=str(path), sampleRate=fr)()
    pyin = PyinMain()
    pyin.initialise(inputSampleRate=fr, lowAmp=lowAmp, pruneThresh=prune, fmin=fmin, fmax=fmax)
    if not isinstance(var_min, str):
        nframes = int(wav.size / hop) + 2
        t = np.arange(nframes) * hop / fr
        min_prof = get_variable_fmin_array(nframes, t, var_min)
#       min_prof = np.linspace(3500, 1600, nframes)
    for i, frame in enumerate(ess.FrameGenerator(audio, frameSize=window, hopSize=hop)):
        if not isinstance(var_min, str):
            pyin.m_yin.m_fmin = min_prof[i]
        fs = pyin.process(frame)
    return pyin, pyin.getSmoothedPitchTrack()


def extract_most_likely_pitch(pyin, fmin=50, fmax=8000, var_min=None):
    pitch = []
    if not isinstance(var_min, type(None)):
        nframes = len(pyin.fs.m_oF0Candidates)
        t = np.arange(nframes) * 256 / pyin.m_inputSampleRate
        min_prof = get_variable_fmin_array(nframes, t, var_min)
        
    for i, (cand, prob) in enumerate(zip(pyin.fs.m_oF0Candidates, pyin.fs.m_oF0Probs)):
        if not isinstance(var_min, type(None)):
            fmin = min_prof[i]
        if len(cand.values)==0:
            pitch.append(0)
        elif len(cand.values)==1:
            if fmin <= cand.values[0] <= fmax:
                pitch.append(cand.values[0])
            else:
                pitch.append(0)
        else:
            data = np.array([(c,p) for c, p in zip(cand.values, prob.values) if fmin<=c<=fmax])
            if data.size==0:
                pitch.append(0)
            else:
                cand, prob = data.T
                pitch.append(cand[np.argmax(prob)])

    if not isinstance(var_min, type(None)):
        return np.array(pitch), min_prof
    else:
        return np.array(pitch), []


def fix_pitch_tracking(pyin, f1, fmin=40, fmax=1600, lowAmp=0.1, var_min=''):
    f2 = extract_most_likely_pitch(pyin, fmin=fmin, fmax=fmax, var_min=var_min)
    f3 = np.copy(f1)
    vp = np.array([sum(x.values) for x in pyin.fs.m_oVoicedProb])
    for i in range(f1.size):
        if vp[i] > lowAmp:
            if f3[i] <= 0:
                f3[i] = f2[i]
#       else:
#           f3[i] = 0
    return f3


def extract_pitch_from_file(path, hop=256, fmin=40, fmax=1600, lowAmp=0.1):
    pyin, f1 = pyin_test(path, fmin=fmin, fmax=fmax, lowAmp=0.0001)
    f2 = fix_pitch_tracking(pyin, f1, fmin=fmax, fmax=fmax, lowAmp=lowAmp)
    t = np.arange(f1.size) * hop / pyin.m_inputSampleRate
    idx = np.where((f2>=fmin)&(f2<=fmax))[0]
    return t[idx], f1[idx], f2[idx]


def run_pitch_alg_all_files():
#   lbls = ['Bird', 'Vocal', 'Speech', 'HapBir', 'Inst']
    lbls = ['Vocal', 'Speech', 'HapBir', 'Inst']
#   for i, path in enumerate([PATH_BIRD_DISC, PATH_HMN, PATH_SPE, PATH_HPB, PATH_INST]):
    for i, path in enumerate([PATH_HMN, PATH_SPE, PATH_HPB, PATH_INST]):
        for j, f in enumerate(path.glob("*wav")):
            try:
                tm, mel = extract_pitch_multi_alg(f, all=True, fmax=1000)
                plots.plot_pitch_alg_comparison(tm, mel, name=f"{lbls[i]}_{f.stem}", xmax=20)
            except Exception as e:
                print(e)
                print(f)



if __name__ == "__main__":
    run_pitch_alg_all_files()




