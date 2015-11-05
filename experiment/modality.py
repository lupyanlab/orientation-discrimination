#!/usr/bin/env python
import os
import yaml
import socket
import webbrowser

# must do this *before* importing psychopy.sound
from resources.pyo_sound import *

from psychopy import visual, core, event, sound

from modality_trials import write_trials
from resources.psychopy_helper import *
from resources.dynamic_mask import DynamicMask

class Experiment(object):
    def __init__(self, exp_dir, version_file):
        """
        Start the experiment by saving the experiment directory and loading the
        version parameters stored in .yaml format.
        """
        self.exp_dir = exp_dir

        version_file_with_path = os.path.join(exp_dir, version_file)
        self.version = yaml.load(open(version_file_with_path, 'r'))

        self.version_name = self.version['version_name']
        self.version_dir = os.path.join(self.exp_dir, self.version_name)
        if not os.path.isdir(self.version_dir):
            os.mkdir(self.version_dir)

        self._get_session_info()
        self._initialize_display()
        self._load_trials_and_stims()

    def _get_session_info(self):
        """
        Pop up a GUI to get popup variables, such as the subject code,
        randomization seed, and experimenter initials.
        """
        subj_info = {'1':  {  'name':'subj_id',
                            'prompt':'EXP_XXX',
                           'options':'any',
                           'default': self.version_name+'_101'},
                     '2':  {  'name':'seed',
                            'prompt':'Seed: ',
                           'options':'any',
                           'default': 101},
                     '3' : {  'name':'initials',
                            'prompt':'Experimenter initials',
                           'options':'any',
                           'default':''}}

        data_dir = os.path.join(self.version_dir, 'data')
        if not os.path.isdir(data_dir):
            os.mkdir(data_dir)

        received = False
        file_opened = False

        while not file_opened:
            received, self.subj_vars = enter_subj_info(self.version_name,
                                                       exp_dir=self.exp_dir,
                                                       options=subj_info)
            if self.subj_vars['subj_id'] == 'test':
                data_file = os.path.join(self.version_dir, 'sample_data.txt')
                try:
                    os.remove(data_file)
                except OSError:
                    pass
            else:
                data_file = os.path.join(data_dir,
                                         self.subj_vars['subj_id']+'.txt')

            if not received:
                popup_error(self.subj_vars)
            elif not os.path.isfile(data_file):
                file_opened = True
                self.data_file = open(data_file, 'w')
            else:
                popup_error('That subject code already exists!')

        self.subj_vars['room'] = socket.gethostname() # record experiment comp.
        self.subj_vars['seed'] = int(self.subj_vars['seed'])

        # save order of subject variables in the data file
        self.subj_vars_fields = ['subj_id', 'seed', 'initials',
                                 'date', 'exp_name', 'room']

    def _initialize_display(self):
        """
        Make the window and all static objects to be used in the experiment.
        """
        self.win = visual.Window(fullscr=True,
                                 color=[.6,.6,.6],
                                 allowGUI=False,
                                 monitor=self.subj_vars['room']+'-Monitor',
                                 units='pix',
                                 winType='pyglet')

        self.stim = {} # store static objects

        self.stim['text'] = visual.TextStim(self.win, height=30, pos=[0,350],
                                            color='black',
                                            wrapWidth=int(self.win.size[0]*.8))

        self.stim['fix'] = visual.TextStim(self.win, text='+', height=100,
                                           color='black')
        self.stim['prompt'] = visual.TextStim(self.win, text='?', height=100,
                                           color='black')

        feedback_dir = os.path.join(self.exp_dir, 'stimuli', 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(os.path.join(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(os.path.join(feedback_dir, 'bleep.wav'))

        frame_params = {}
        frame_params['win'] = self.win
        frame_params['lineColor'] = 'black'
        frame_params['lineWidth'] = 2.0
        frame_params['fillColor'] = None
        frame_params['width'] = self.version['task']['pic_size'][0] + 10
        frame_params['height'] = self.version['task']['pic_size'][1] + 10

        locs = self.version['task']['pic_locs']

        self.stim['left_frame'] = visual.Rect(pos=locs['left'], **frame_params)
        self.stim['right_frame']= visual.Rect(pos=locs['right'], **frame_params)

        mask_params = {}
        mask_params['win'] = self.win
        mask_params['size'] = self.version['task']['pic_size']

        mask_dir = os.path.join(self.exp_dir, 'stimuli', 'dynamic_mask')
        self.mask_left = DynamicMask(mask_dir, 'colored', ori=0.0,
                                     pos=locs['left'], **mask_params)
        self.mask_right = DynamicMask(mask_dir, 'colored', ori=180.0,
                                     pos=locs['right'], **mask_params)

    def _load_trials_and_stims(self):
        """
        Load the trials and stimuli from external files.
        """
        show_text(self.win, 'Loading...', color='black', waitForKey=False)

        trials_dir = os.path.join(self.version_dir, 'trials')
        if not os.path.isdir(trials_dir):
            os.mkdir(trials_dir)

        if self.subj_vars['subj_id'] == 'test':
            trials_file = os.path.join(self.version_dir, 'sample_trials.csv')
            try:
                os.remove(trials_file)
            except OSError:
                pass
        else:
            trials_file = 'seed_{0}.csv'.format(self.subj_vars['seed'])
            trials_file = os.path.join(trials_dir, trials_file)

        if not os.path.isfile(trials_file):
            write_trials(trials_file, exp_dir=self.exp_dir,
                         trial_params=self.version['trials'],
                         seed=self.subj_vars['seed'])

        (self.trials, self.trials_fields) = import_trials(trials_file,
                                                          method='sequential')

        pic_params = {}
        pic_params['win'] = self.win
        pic_params['size'] = self.version['task']['pic_size']

        locs = self.version['task']['pic_locs']

        pic_dir = os.path.join(self.exp_dir, 'stimuli', 'pics')
        self.pics_left = load_images(pic_dir, 'bmp', pos=locs['left'],
                                     **pic_params)
        self.pics_right = load_images(pic_dir, 'bmp', pos=locs['right'],
                                     **pic_params)

        cue_dir = os.path.join(self.exp_dir, 'stimuli', 'cues')
        self.cues = load_sounds(cue_dir, 'wav')

        noise_dir = os.path.join(self.exp_dir, 'stimuli', 'noise')
        self.noises = load_sounds(noise_dir, 'wav')

    def _show_instructions(self):
        instructions = self.version['instructions']

        for page in instructions['pages']:
            self.stim['text'].setText(instructions['text'][page])
            self.stim['text'].draw()

            if page == 2:
                self.pics_left['donkey.bmp'].setOri(180.0)
                self.pics_left['donkey.bmp'].draw()
                self.pics_right['donkey.bmp'].draw()
                advance_keys = ['j',]

            elif page == 3:
                self.pics_right['piano.bmp'].setOri(180.0)
                self.pics_right['piano.bmp'].draw()
                self.pics_left['piano.bmp'].draw()
                advance_keys = ['f',]

            elif page == 4:
                self.mask_left.draw()
                self.mask_right.draw()
                advance_keys = ['space',]

            else:
                advance_keys = ['space',]

            self.win.flip()
            event.waitKeys(keyList = advance_keys)

    def run(self):
        """
        Run through text screens and trials.
        """
        self._show_instructions()

        self.exp_timer = core.Clock()
        self.resp_keys = self.version['task']['resp_keys']

        # save columns for the DVs in the data file
        self.resp_fields = ['exp_timer','response','rt','is_correct']

        header_pth = os.path.join(self.version_dir, 'data', '_header.txt')
        if not os.path.isfile(header_pth):
            mutable_trials_fields = list(self.trials_fields)
            header = self.subj_vars_fields+mutable_trials_fields+self.resp_fields
            header_file = open(header_pth, 'w')
            write_list_to_file(header, header_file, close=True)

        cur_block_ix = -1
        for cur_trial in self.trials:
            if cur_trial['block_ix'] > cur_block_ix:
                self._break_for_new_block(cur_block_ix)
                cur_block_ix += 1

            self._present_trial(cur_trial)

        self.data_file.close()

    def _break_for_new_block(self, prev_block_ix):
        """ Show text screens between blocks """
        if prev_block_ix == -1:
            end_practice_txt = self.version['end_of_practice']
            for page in end_practice_txt['pages']:
                show_text(self.win, end_practice_txt['text'][page], color='black')
        else:
            break_txt = self.version['break_text']
            show_text(self.win, break_txt, color='black')

    def _give_feedback(self, is_correct):
        self.feedback[is_correct].play()

    def _present_trial(self, trial):
        """ Present a single trial """
        if trial['cue_type'] == 'noise':
            cue = self.noises[trial['cue_file']]
        else:
            cue = self.cues[trial['cue_file']]
        cue_dur = cue.getDuration()

        stims_after_cue = []
        if trial['mask_type'] == 'visual':
            stims_after_cue.extend([self.mask_left, self.mask_right])

        left_pic = self.pics_left[trial['pic_file']]   # just a shortcut
        right_pic = self.pics_right[trial['pic_file']] #

        if trial['up_pic'] == 'left':
            left_pic.setOri(0.0)
            right_pic.setOri(180.0)
        else:
            left_pic.setOri(180.0)
            right_pic.setOri(0.0)

        ############################
        # BEGIN TRIAL PRESENTATION #
        ############################
        self.stim['fix'].autoDraw = True
        self.stim['left_frame'].autoDraw = True
        self.stim['right_frame'].autoDraw = True

        self.win.flip()
        core.wait(self.version['task']['fix_dur'])

        # play cue, if there is one
        if cue:
            cue.play()
        self.win.flip()
        core.wait(cue_dur)

        # if its an interference trial, present the interference
        # play auditory interference before entering visual interference loop
        if trial['mask_type'] == 'auditory':
            self.noises['noise.wav'].play()
        post_timer = core.Clock()
        while post_timer.getTime() < cue_dur:
            # draw the masks if they were added to stims_after_cue
            [stim.draw() for stim in stims_after_cue]
            self.win.flip()
            core.wait(0.01)
        self.win.flip()

        self.win.flip()
        core.wait(self.version['task']['cue_offset_to_pic_onset'])

        # show pic and start RT timer
        left_pic.draw()
        right_pic.draw()
        rt_timer = core.Clock()
        self.win.flip()
        core.wait(self.version['task']['pic_dur'])

        # remove images and replace fix with prompt
        self.stim['fix'].autoDraw = False
        self.stim['prompt'].draw()
        self.win.flip()
        response = event.waitKeys(maxWait=self.version['task']['max_wait'],
                                  keyList=self.resp_keys.keys(),
                                  timeStamped=rt_timer)
        try:
            key, rt = response[0]
            response = self.resp_keys[key]
        except TypeError:
            rt = self.version['task']['max_wait']
            response = 'timeout'

        is_correct = int(response == trial['up_pic'])

        # end trial
        self.win.flip()

        if trial['block_ix'] == -1:
            self._give_feedback(is_correct)

        if response == 'timeout':
            show_text(self.win, self.version['timeout_text'], color='black')

        core.wait(self.version['task']['iti'] - rt)

        ##########################
        # END TRIAL PRESENTATION #
        ##########################

        # record response variables
        resp = {}
        resp['exp_timer'] = self.exp_timer.getTime()
        resp['response'] = response
        resp['rt'] = rt*1000.0
        resp['is_correct'] = is_correct

        # write trial data to file
        trial_data = []
        [trial_data.append(self.subj_vars[v]) for v in self.subj_vars_fields]
        [trial_data.append(trial[v]) for v in self.trials_fields]
        [trial_data.append(resp[v]) for v in self.resp_fields]
        write_list_to_file(trial_data, self.data_file)

    def end(self):
        end_txt = self.version['end_text']
        show_text(self.win, end_txt, color='black')

        #core.quit()

        surveyURL = self.version['survey_url']
        surveyURL += '&subj_id={0}&room={1}&version={2}'.format(
            self.subj_vars['subj_id'], self.subj_vars['room'], self.version_name
        )
        webbrowser.open(surveyURL)

if __name__ == '__main__':
    root_dir = os.path.dirname(os.path.abspath(__file__))
    exp = Experiment(exp_dir=root_dir, version_file='modality.yaml')
    exp.run()
    exp.end()
