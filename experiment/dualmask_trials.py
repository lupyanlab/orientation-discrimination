#!/usr/bin/env python
import os
import yaml

import pandas as pd
import numpy as np

from resources.trials_functions import *
from resources.generator_functions import *

def load_stim_info(stim_dir):
    """ Load all the `_[stim]_info.csv` files as pd.DataFrames """
    stim_info = {}

    cue_info_pth = os.path.join(stim_dir, 'cues', '_cue_info.csv')
    stim_info['cue'] = pd.read_csv(cue_info_pth)

    noise_info_pth = os.path.join(stim_dir, 'noise', '_noise_info.csv')
    stim_info['noise'] = pd.read_csv(noise_info_pth)

    pic_info_pth = os.path.join(stim_dir, 'pics', '_pic_info.csv')
    stim_info['pic'] = pd.read_csv(pic_info_pth)

    return stim_info

def select_pics_and_cues(stim_info, trial_params, seed):
    """ Select a random subset of all possible pics and cues """
    num_pics = trial_params['num_pics']
    prng = np.random.RandomState(seed)
    selected = prng.choice(stim_info['pic']['pic'].values, num_pics,
                           replace=False)

    pic_info = stim_info['pic'][:]
    keep_pics = [pic in selected for pic in pic_info['pic'].values]
    stim_info['pic'] = pic_info[keep_pics].reset_index(drop=True)

    cue_info = stim_info['cue'][:]
    keep_cues = [cue in selected for cue in cue_info['cue'].values]
    stim_info['cue'] = cue_info[keep_cues].reset_index(drop=True)

    return stim_info

def balance_within_subj_vars(trial_parameters):
    """ Balance all within subject variables without any stimuli information """
    trials = pd.DataFrame({'up_pic':['left','right']})
    trials = expand(trials, name='is_cue_valid', values=[1,0],
                    ratio=trial_parameters['ratio_valid_cue'], sample=False)

    trials = expand(trials, name='cue_type', values=['label','noise'],
                    ratio=trial_parameters['ratio_label_cue'], sample=False)
    trials['is_cue_valid'][trials['cue_type'] == 'noise'] = -1
    trials['cue_type'][trials['is_cue_valid'] == 1] = 'valid'
    trials['cue_type'][trials['is_cue_valid'] == 0] = 'invalid'

    trials = expand(trials, name='is_cue_masked', values=[1,0],
                    ratio=trial_parameters['ratio_masked_cue'], sample=False)
    return trials

def add_pic_info(trials, pic_info, seed):
    """ Copy the within subject variables for all selected pictures """
    trials = extend(trials, reps=pic_info['pic_id'].values, rep_ix='pic_id')
    trials = trials.merge(pic_info)
    return trials

def add_cue_info(trials, cue_info, noise_info, seed):
    """ Add cue information for valid, invalid, and no cue trials """
    valid_cue = generate_matches(trials[trials['is_cue_valid'] == 1], cue_info,
                    on=['pic','cue'], seed=seed).reset_index(drop=True)
    invalid_cue = generate_but_not(trials[trials['is_cue_valid'] == 0],cue_info,
                    on=['pic','cue'], seed=seed).reset_index(drop=True)
    no_cue = generate(trials[trials['is_cue_valid'] == -1], noise_info,
                    seed=seed).reset_index(drop=True)

    trials = pd.concat([valid_cue, invalid_cue, no_cue], ignore_index=True)
    return trials

def add_practice_block(trials, pic_info, seed):
    """ For the practice trials, show one trial from each pic category """
    prng = np.random.RandomState(seed)

    def get_practice_trial_ix(chunk):
        return prng.choice(chunk.index, 1)[0]

    practice_ixs = trials.groupby('pic_id').apply(get_practice_trial_ix)
    practice = trials.ix[practice_ixs].reset_index(drop=True)
    practice['block_ix'] = -1

    trials = pd.concat([practice, trials], ignore_index=True)
    return trials

def shuffle_and_enumerate(trials, seed):
    """
    Shuffle the trials so that the same picture does not appear twice in a row
    and enumerate the trials so that trial_ix == 0 is the first test trial.
    """
    trials = smart_shuffle(trials, col='pic', block='block_ix', seed=seed)
    num_practice_trials = len(trials[trials['block_ix'] == -1])
    trials['trial_ix'] = np.arange(len(trials)) - num_practice_trials
    return trials

def make_trials(exp_dir, trial_params, seed):
    stim_info = load_stim_info(os.path.join(exp_dir, 'stimuli'))
    stim_info = select_pics_and_cues(stim_info, trial_params, seed=seed)

    trials = balance_within_subj_vars(trial_params)
    trials = add_pic_info(trials, stim_info['pic'], seed)
    trials = add_cue_info(trials, stim_info['cue'], stim_info['noise'], seed)

    trials = add_block(trials, 100, name='block_ix', start=0, groupby='pic',
                       seed=seed)
    trials = add_practice_block(trials, stim_info['pic'], seed=seed)

    trials = shuffle_and_enumerate(trials, seed)

    trials = trials[['block_ix', 'trial_ix',
                     'is_cue_valid', 'cue_type', 'is_cue_masked', 'up_pic',
                     'cue', 'cue_id', 'cue_version', 'cue_file',
                     'pic', 'pic_id', 'pic_file']]
    return trials

def write_trials(trials_pth, exp_dir, trial_params, seed):
    trials = make_trials(exp_dir, trial_params, seed)
    trials.to_csv(trials_pth, index=False)

if __name__ == '__main__':
    exp_dir = os.path.dirname(os.path.abspath(__file__))
    version_file = os.path.join(exp_dir, 'dualmask.yaml')
    trial_params = yaml.load(open(version_file, 'r'))['trials']
    seed = 105

    trials = make_trials(exp_dir, trial_params, seed)
