#!/usr/bin/env python
import os
import pandas as pd

from psychopy import sound


def load_sounds(sound_dir, ext):
    sound_names = [name for name in os.listdir(sound_dir) if name[-3:] == ext]
    sounds = {}
    for snd_name in sound_names:
        snd_path = os.path.join(sound_dir, snd_name)
        sounds[snd_name] = sound.Sound(snd_path)
    
    return sounds

sounds = load_sounds('.', 'wav')
sounds = pd.DataFrame.from_dict(sounds, orient='index')

def get_sound_dur(row):
    return row['object'].getDuration()

sounds = sounds.rename(columns={0:'object'})
sounds.index.name = 'cue_file'
sounds['cue_dur'] = sounds.apply(get_sound_dur, axis=1)

sounds = sounds.drop('object', axis=1)
sounds = sounds.reset_index()

print 'Average sound duration:', sounds['cue_dur'].mean()

sounds.to_csv('_cue_stats.csv', index=False)
