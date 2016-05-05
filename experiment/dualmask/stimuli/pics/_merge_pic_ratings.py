#!/usr/bin/env python

import pandas as pd

ratings = pd.read_csv('../archive/pics/_orig/_norming/all.csv')
ratings.columns = [col.replace('_color', '') for col in ratings.columns]
ratings['concept'] = ratings['concept'].str.replace(' ', '_')
ratings = ratings.rename(columns = {'num':'pic_id', 'concept':'pic'})

keep = ['pic_id', 'pic', 'complex_mean', 'diagnostic', 'familiarity_mean',
        'image_mean', 'H']
ratings = ratings[keep]

pic_info = pd.read_csv('_pic_info.csv')
pic_info = pic_info.merge(ratings, how = 'left')
pic_info.to_csv('_pic_ratings.csv', index=False)
