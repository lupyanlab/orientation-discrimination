#!/usr/bin/env python
"""
Make the _cue_info.csv based on _pic_info.csv, with duplication based on the
number of cue file versions used.
"""
import os
import pandas as pd

def make_cue_info_from_pic_info():
    df = pd.read_csv('../pics/_pic_info.csv')
    df = df.rename(columns={'pic':'cue', 'pic_id':'cue_id'})
    df.to_csv('_cue_info.csv')

def copy_cue_info_for_versions(num_versions=4):
    reps = range(1, num_versions+1)
    df = pd.read_csv('_cue_info.csv', index_col='cue_id')
    df = pd.concat([df]*len(reps), keys=reps, names=['cue_version','cue_id'])
    df = df.reset_index()
    df = df.sort(['cue_id','cue_version'])
    df['cue_file'] = df['cue'] + '_' + df['cue_version'].astype(str) + '.wav'
    df = df[['cue','cue_id','cue_version','cue_file']]
    df.to_csv('_cue_info.csv', index=False)

if __name__ == '__main__':
    make_cue_info_from_pic_info()
    copy_cue_info_for_versions()
