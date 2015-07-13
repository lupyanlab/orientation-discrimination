#! /usr/bin/env python
import os
import random

from psychopy import core, event, visual, data, gui, misc, sound

def enter_subj_info(exp_name, exp_dir, options):
    """ 
    Brings up a GUI in which to enter all the subject info. 
    """
    def inputsOK(options,expInfo):
        for curOption in sorted(options.items()):
            if curOption[1]['options'] != 'any' and expInfo[curOption[1]['name']] not in curOption[1]['options']:
                return [False,"The option you entered for " + curOption[1]['name'] + " is not in the allowable list of options: " + str(curOption[1]['options'])]
        return [True,'']
    
    version_pth = os.path.join(exp_dir, exp_name, '_last_params.pickle')
    
    try:
        expInfo = misc.fromFile(version_pth)
    except:
        expInfo={} #make the kind of dictionary that this gui can understand
        for curOption in sorted(options.items()):
            expInfo[curOption[1]['name']]=curOption[1]['default']
    #load the tips
    tips={}
    for curOption in sorted(options.items()):
        tips[curOption[1]['name']]=curOption[1]['prompt']
    expInfo['date']= data.getDateStr() 
    expInfo['exp_name']= exp_name
    dlg = gui.DlgFromDict(expInfo, title=exp_name, fixed=['date','exp_name'],order=[optionName[1]['name'] for optionName in sorted(options.items())],tip=tips)
    if dlg.OK:
        misc.toFile(version_pth, expInfo)
        [success,error] = inputsOK(options,expInfo)
        if success:
            return [True,expInfo]
        else:
            return [False,error]
    else:
        core.quit()

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

def load_images(image_dir, ext, **kwargs):
    if not isinstance(ext, list):
        ext = [ext,]
    
    image_names = [name for name in os.listdir(image_dir) if name[-3:] in ext]
    images = {}
    for img_name in image_names:
        img_path = os.path.join(image_dir, img_name)
        images[img_name] = visual.ImageStim(image=img_path, **kwargs)
    
    return images

def load_sounds(sound_dir, ext):
    sound_names = [name for name in os.listdir(sound_dir) if name[-3:] == ext]
    sounds = {}
    for snd_name in sound_names:
        snd_path = os.path.join(sound_dir, snd_name)
        sounds[snd_name] = sound.Sound(snd_path)
    
    return sounds

def write_list_to_file(line, file, close=False):
    line = '\t'.join([str(value) for value in line]) + '\n'
    file.write(line)
    file.flush()
    os.fsync(file)
    
    if close:
        file.close()
