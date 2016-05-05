#! /usr/bin/env python
import os
import random
import socket

import unipath
import yaml

from psychopy import core, event, visual, data, gui, misc, sound

def get_subj_info(gui_yaml, check_exists, save_order=True):
    """ Create a psychopy.gui from a yaml config file.

    The first time the experiment is run, a pickle of that subject's settings
    is saved. On subsequent runs, the experiment tries to prepopulate the
    settings with those of the previous subject.

    Parameters
    ----------
    gui_yaml: str, Path to config file in yaml format.
    check_exists: function, Computes a data file path from the gui data, and
        checks for its existence. If the file exists, an error is displayed.
    save_order: bool, Should the key order be saved in "_order"? Defaults to
        True.

    Returns
    -------
    dict, with key order saved in "_order", if specified.
    """
    with open(gui_yaml, 'r') as f:
        gui_info = yaml.load(f)

    ordered_fields = [field for _, field in sorted(gui_info.items())]

    # Determine order and tips
    ordered_names = [field['name'] for field in ordered_fields]
    field_tips = {field['name']: field['prompt'] for field in ordered_fields}

    # Load the last participant's options or use the defaults
    last_subj_info = gui_yaml + '.pickle'
    try:
        gui_data = misc.fromFile(last_subj_info)
        for yaml_name in ordered_names:
            if yaml_name not in gui_data:
                # Invalid pickle
                raise AssertionError
    except IOError, AssertionError:
        gui_data = {field['name']: field['default'] for field in ordered_fields}

    # Set fixed fields
    gui_data['date'] = data.getDateStr()
    gui_data['computer'] = socket.gethostname()
    fixed_fields = ['date', 'computer']

    while True:
        # Bring up the dialogue
        dlg = gui.DlgFromDict(gui_data, order=ordered_names,
                              fixed=fixed_fields, tip=field_tips)

        if not dlg.OK:
            core.quit()

        subj_info = dict(gui_data)

        if check_exists(subj_info):
            popup_error('That subj_id already exists.')
        else:
            misc.toFile(last_subj_info, subj_info)
            break

    if save_order:
        subj_info['_order'] = ordered_names + fixed_fields
    return subj_info


def load_sounds(stim_dir, match='*.wav'):
    sound_files = unipath.Path(stim_dir).listdir(match)
    sounds = {}
    for sound_file in sound_files:
        str_path = str(sound_file)  # psychopy chokes on unipath.Path
        sounds[sound_file.stem] = sound.Sound(str_path)
    return sounds


def load_images(stim_dir, match='*.bmp', **kwargs):
    image_files = unipath.Path(stim_dir).listdir(match)
    images = {}
    for image_file in image_files:
        str_path = str(image_file)
        images[image_file.stem] = visual.ImageStim(image=str_path, **kwargs)
    return images


def import_trials(fileName, method="sequential", seed=random.randint(1,100)):
	(stimList,fieldNames)=data.importConditions(fileName,returnFieldNames=True)
	trials = data.TrialHandler(stimList,1,method=method,seed=seed)
	return (trials,fieldNames)

def show_text(win, textToShow, color=[-1,-1,-1], waitForKey=True,
              acceptOnly=0, inputDevice="keyboard", mouse=False, pos=[0,0],
              scale=1):
	global event
	event.clearEvents() #clear all events just in case
	win.flip()
	if win.units == "pix":
		height = 30*scale
		wrapWidth=int(win.size[0]*.8)
	elif win.units == "deg":
		height=.7*scale
		wrapWidth=30
	else:
		wrapWidth=None
	textStim = visual.TextStim(win, pos=pos, wrapWidth=wrapWidth, color=color,
                               height=height, text=textToShow)
	textStim.draw()
	win.flip()
	if mouse:
		while any(mouse.getPressed()):
			core.wait(.1) #waits for the user to release the mouse
		while not any(mouse.getPressed()):
			pass
		return
	elif inputDevice=="keyboard":
		if waitForKey:
			if acceptOnly==0:
				event.waitKeys()
			else:
				event.waitKeys(keyList=[acceptOnly])
			return
		else:
			event.clearEvents(eventType='keyboard')
			return
	elif inputDevice=="gamepad": #also uses mouse if mouse is not false
		while True:
			for event in pygame.event.get(): #check responses
				if mouse:
					if event.type==pygame.MOUSEBUTTONDOWN:
						pygame.event.clear()
						return
				if event.type==pygame.KEYDOWN or event.type==pygame.JOYBUTTONDOWN:
					pygame.event.clear()
					return

def popup_error(text):
	errorDlg = gui.Dlg(title="Error", pos=(200,400))
	errorDlg.addText('Error: '+text, color='Red')
	errorDlg.show()


def write_list_to_file(line, file, close=False):
    line = '\t'.join([str(value) for value in line]) + '\n'
    file.write(line)
    file.flush()
    os.fsync(file)

    if close:
        file.close()
