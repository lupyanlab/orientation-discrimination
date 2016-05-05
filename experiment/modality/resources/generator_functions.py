#!/usr/bin/env python
"""
resources.generator_functions
"""
import pandas as pd
import numpy as np
import itertools as itls

def _generator(source, prng=None):
    """
    Yield rows from source with optional randomization.
    
    :param source: pandas.DataFrame.
    :param prng: numpy.RandomState, optional.
    :return: Yields pandas.Series for rows of source
    """
    ix_options = np.array(source.index)
    num_options = len(ix_options)
    
    if prng is not None:
        prng.shuffle(ix_options)
    
    ix = 0
    while True:
        next_ix = ix_options[ix]
        yield source.ix[next_ix]
        ix = (ix+1)%num_options
        
        if prng is not None and ix%num_options == 0:
            prng.shuffle(ix_options)

def generate(frame, source, cols=None, seed=None):
    """
    Yield `length` rows from source.
    
    :param source: pandas.DataFrame.
    :param length: int. Number of items to generate from source.
    :param cols: str or list, optional.
    :param seed: int, optional.
    :return: pandas.DataFrame.
    """
    prng = None
    
    if seed is not None:
        prng = np.random.RandomState(seed)
    
    if cols is None:
        cols = source.columns
    elif not hasattr(cols, '__iter__'):
        cols = [cols,]
    
    gen = _generator(source, prng)
    g_frame = pd.concat([gen.next() for _ in xrange(len(frame))], axis=1).T
    g_frame = g_frame.convert_objects(convert_numeric=True)
    g_frame.index = frame.index
    
    frame[cols] = g_frame[cols]
    return frame

def generate_by_group(frame, by, source_map, cols=None, seed=None):
    num_seeds = len(frame[by].unique()) + 1
    if seed is not None:
        prng = np.random.RandomState(seed)
        seeds = list(prng.choice(np.arange(1000), num_seeds))
    else:
        seeds = [None]*num_seeds
        
    def _generate(group):
        grp_ix = group[by].unique()[0]
        source = source_map[grp_ix]
        g_frame = generate(group, source, cols=cols, seed=seeds.pop())
        g_frame.index = group.index
        return g_frame
    
    return frame.groupby(by, group_keys=False).apply(_generate)

def generate_matches(frame, source, on, cols=None, seed=None):
    if not isinstance(on, list):
        on = [on, on]
    f_on, s_on = on
    source_map = {val: source[source[s_on] == val] \
                  for val in frame[f_on].unique()}
    return generate_by_group(frame, f_on, source_map, cols, seed)

def generate_but_not(frame, source, on, cols=None, seed=None):
    if not isinstance(on, list):
        on = [on, on]
    f_on, s_on = on
    source_map = {val: source[source[s_on] != val] \
                  for val in frame[f_on].unique()}
    return generate_by_group(frame, f_on, source_map, cols, seed)
