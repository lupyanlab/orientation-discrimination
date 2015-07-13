#!/usr/bin/env python
"""
Copy the pictures in _pic_info.csv from the archive to the current directory.
"""
import os
import shutil
import pandas as pd

used_pics = pd.read_csv('_pic_info.csv')['pic_file'].values

_ = [os.remove(name) for name in os.listdir('.') \
        if os.path.splitext(name)[1] == '.bmp']

for pic in used_pics:
    arch = os.path.join('..', 'archive', 'pics', pic)
    try:
        shutil.copyfile(arch, pic)
    except IOError:
        print 'Picture not found:', pic
