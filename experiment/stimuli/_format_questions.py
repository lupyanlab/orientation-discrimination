#!/usr/bin/env python
import pandas as pd
import numpy as np
from psychopy import visual, event

def wide_to_long():
    wide = pd.read_excel('visual_nonvisual_questions.xls', sheetname = 'Sheet1')
    df = pd.melt(wide, id_vars = 'category', var_name = 'type', 
                 value_name = 'question')
    df[['type','id']] = df['type'].str.extract('([a-z]+)_(\d)')
    df = df[df['question'].notnull()]
    df = df.sort(['category','type','id']).reset_index(drop = True)    
    df = df[['category','type','id','question']]
    
    df.to_csv('visual_nonvisual_questions.csv')
    
    return df

def test_questions(df):
    df = df.reset_index(drop = True)
    responses = df['actual'].values
    
    win = visual.Window(units = 'pix')
    category = visual.TextStim(win, text = "", pos = [0,100])
    question = visual.TextStim(win, text = "")
    
    not_expected = (df['expected'] != df['actual'])
    not_expected_index = df.index[not_expected]
    
    for ix in not_expected_index:
        row = df.ix[ix]
        
        category.setText(row['category'])
        question.setText(row['question'])
        category.draw()
        question.draw()
        win.flip()
        
        keys = event.waitKeys(keyList = ['y','n','q'])
                
        if 'q' in keys:
            win.close()
            break
        
        responses[ix] = keys[0]
    
    df['actual'] = responses
    return df

def shuffle_not_expected(df, seed = 100):
    not_expected = (df['expected'] != df['actual'])
    
    not_expected_categories = df[not_expected]['category'].values
    
    prng = np.random.RandomState(seed)
    prng.shuffle(not_expected_categories)
    
    df['category'][not_expected] = not_expected_categories
    
    return df

if __name__ == "__main__":
    
    wide_to_long()

    
    #yes = df[:]
    #no = df[:]
    #no['expected'] = 'n'
    #no['actual'] = '*'
    
    #while (no['expected'] != no['actual']).sum() > 0:
    #    no = shuffle_not_expected(no, seed = 100)
    #    no = test_questions(no)
    
    