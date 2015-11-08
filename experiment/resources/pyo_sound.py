from psychopy import prefs

try:
    import pyo
except ImportError:
    print 'could not load pyo!'
else:
    prefs.general['audioLib'] = ['pyo']

from psychopy import sound

if prefs.general['audioLib'][0] == 'pyo':
    print 'initializing pyo to 48000'
    sound.init(48000,buffer=128)
    print 'Using %s(with %s) for sounds' %(sound.audioLib, sound.audioDriver)
