#!/usr/bin/env python
"""
Copy the cues in _cue_info.csv from the archive to the current directory.
"""
import os
import shutil
import pandas as pd

used_cues = pd.read_csv('_cue_info.csv')['cue_file'].values

_ = [os.remove(name) for name in os.listdir('.') \
        if os.path.splitext(name)[1] == '.wav']

for cue in used_cues:
    try:
        shutil.copyfile(os.path.join('..', 'archive', 'cues', cue), cue)
    except IOError:
        print 'Cue not found:', cue
