#!/usr/bin/env python
"""
Rename original picture files in `[num].bmp` format to `[pic].bmp` format.
"""
import os
import shutil
import pandas as pd

name_map = pd.read_csv('_name_map.csv', index_col='num').to_dict()['concept']

_ = [os.remove(name) for name in os.listdir('.') \
        if os.path.splitext(name)[1] == '.bmp']

old_names = [name for name in os.listdir('_orig/') \
                if os.path.splitext(name)[1] == '.bmp']

for old in old_names:
    img_num, ext = os.path.splitext(old)
    img_num = int(img_num)
    new = name_map[img_num].replace(' ','_') + ext
    shutil.copyfile('_orig/'+old, new)
