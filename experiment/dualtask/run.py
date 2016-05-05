#!/usr/bin/env python
import copy
from UserDict import UserDict
from UserList import UserList

import unipath
import pandas
from numpy import random
import yaml

try:
    import pyo
except ImportError:
    print 'pyo not installed!'
from psychopy import prefs
prefs.general['audioLib'] = ['pyo', ]
from psychopy import sound

from psychopy import visual, core, event

from labtools import DynamicMask
from labtools.trials_functions import (expand, extend, add_block, smart_shuffle,
                                       simple_shuffle)
from labtools.psychopy_helper import get_subj_info, load_sounds, load_images

class Participant(UserDict):
    """ Store participant data and provide helper functions. """
    DATA_DIR = 'data'
    DATA_DELIMITER = ','

    def __init__(self, **kwargs):
        """ Standard dict constructor.

        Saves _order if provided. Raises an AssertionError if _order
        isn't exhaustive of kwargs.
        """
        self._data_file = None
        self._order = kwargs.pop('_order', kwargs.keys())

        correct_len = len(self._order) == len(kwargs)
        kwargs_in_order = all([kwg in self._order for kwg in kwargs])
        assert correct_len & kwargs_in_order, "_order doesn't match kwargs"

        self.data = dict(**kwargs)

    @property
    def data_file(self):
        if not unipath.Path(self.DATA_DIR).exists():
            unipath.Path(self.DATA_DIR).mkdir()

        if not self._data_file:
            data_file_name = '{subj_id}.csv'.format(**self)
            self._data_file = unipath.Path(self.DATA_DIR, data_file_name)
        return self._data_file

    def write_header(self, trial_col_names):
        """ Writes the names of the columns and saves the order. """
        self._col_names = self._order + trial_col_names
        self._write_line(self.DATA_DELIMITER.join(self._col_names))

    def write_trial(self, trial):
        assert self._col_names, 'write header first to save column order'
        trial_data = dict(self)
        trial_data.update(trial)
        row_data = [str(trial_data[key]) for key in self._col_names]
        self._write_line(self.DATA_DELIMITER.join(row_data))

    def _write_line(self, row):
        with open(self.data_file, 'a') as f:
            f.write(row + '\n')


class Trials(UserList):
    STIM_DIR = 'stimuli'
    COLUMNS = [
        # Trial columns
        'block',
        'block_type',
        'trial',

        # Stimuli columns
        'cue',
        'cue_type',
        'mask_type',
        'response_type',
        'target',
        'target_loc',
        'correct_response',

        # Response columns
        'keys_pressed',
        'response',
        'rt',
        'is_correct',
    ]

    @classmethod
    def make(cls, **kwargs):
        seed = kwargs.get('seed')
        try:
            seed = int(seed)
        except TypeError, ValueError:
            seed = None
        prng = random.RandomState(seed)

        # Balance within subject variables
        trials = pandas.DataFrame({'mask_type': ['nomask', 'mask']})
        trials = expand(trials, name='cue_type', values=['valid', 'invalid'],
                        ratio=0.75, seed=seed)
        trials = expand(trials, name='response_type', values=['pic', 'word'],
                        ratio=0.75, seed=seed)

        # Set length of experiment
        trials = extend(trials, max_length = 320)

        # Determine target category on every trial
        categories_csv = unipath.Path(cls.STIM_DIR, 'categories.csv')
        categories = pandas.read_csv(categories_csv).category
        trials['target'] = prng.choice(categories, len(trials), replace=True)
        trials['target_loc'] = prng.choice(['left', 'right'], len(trials),
                                           replace=True)

        def pick_cue(trial):
            # On valid cue trials, the cue is the same as the target
            if trial['cue_type'] == 'valid':
                return trial['target']
            # On invalid cue trials, the cue is anything but the target
            elif trial['cue_type'] == 'invalid':
                distractors = list(categories)
                distractors.remove(trial['target'])
                return prng.choice(distractors)
            else:
                raise NotImplementedError('cue type %s' % trial['cue_type'])
        trials['cue'] = trials.apply(pick_cue, axis=1)

        response_map = dict(valid='up', invalid='down')
        def determine_correct_response(trial):
            # On pic trials, the correct response is the location of the target
            if trial['response_type'] == 'pic':
                return trial['target_loc']
            elif trial['response_type'] == 'word':
                return response_map[trial['cue_type']]
            else:
                raise NotImplementedError('response type %s' % trial['response_type'])
        trials['correct_response'] = trials.apply(determine_correct_response, axis=1)

        # Add block
        trials = add_block(trials, size=60, start=0, seed=seed)
        trials['block_type'] = 'test'

        # Add practice trials
        num_practice = 12
        practice_ix = prng.choice(trials.index, num_practice)
        practice_trials = trials.ix[practice_ix, ]
        trials.drop(practice_ix, inplace=True)

        practice_trials['block_type'] = 'practice'
        practice_trials['block'] = -1
        trials = pandas.concat([practice_trials, trials], ignore_index=True)

        # Shuffle
        try:
            trials = smart_shuffle(trials, col='target', block='block', seed=seed)
        except ValueError:
            print 'there was a problem shuffling the trials'
            trials = simple_shuffle(trials, block='block', seed=seed)

        # Enumerate trials
        trials['trial'] = range(1, len(trials)+1)

        # Add blank columns for response variables
        for c in ['keys_pressed', 'response', 'rt', 'is_correct']:
            trials[c] = ''

        return cls(trials.to_dict('record'))

    def write_trials(self, trials_csv):
        trials = pandas.DataFrame.from_records(self)
        trials = trials[self.COLUMNS]
        trials.to_csv(trials_csv, index=False)

    def iter_blocks(self, key='block'):
        """ Yield blocks of trials. """
        block = self[0][key]
        trials_in_block = []
        for trial in self:
            if trial[key] == block:
                trials_in_block.append(trial)
            else:
                yield trials_in_block
                block = trial[key]
                trials_in_block = []


class Experiment(object):
    STIM_DIR = 'stimuli'

    def __init__(self, settings_yaml='settings.yaml', texts_yaml='texts.yaml'):
        with open(settings_yaml) as f:
            settings = yaml.load(f)
        self.layout = settings.pop('layout')
        self.positions = self.layout.pop('positions')
        self.waits = settings.pop('waits')
        self.response_keys = settings.pop('response_keys')
        self.all_response_keys = self.response_keys['pic'].keys() +\
                                 self.response_keys['word'].keys()
        self.survey_url = settings.pop('survey_url')

        with open(texts_yaml) as f:
            self.texts = yaml.load(f)

        self.win = visual.Window(fullscr=True, allowGUI=False, units='pix')

        text_kwargs = dict(
            win=self.win,
            font='Consolas',
            height=60,
            color='black'
        )
        self.fix = visual.TextStim(text='+', **text_kwargs)
        self.prompt = visual.TextStim(text='?', **text_kwargs)

        pic_size = self.layout['pic_size']
        frame_kwargs = dict(
            win=self.win,
            lineColor='black',
            lineWidth=2.0,
            fillColor=None,
            width=pic_size[0] + 10,
            height=pic_size[0] + 10,
        )
        self.frames = [visual.Rect(pos=pos, **frame_kwargs)
                       for pos in self.positions.values()]

        self.cues = load_sounds(unipath.Path(self.STIM_DIR, 'cues'))


        mask_kwargs = dict(
            win=self.win,
            size=pic_size,
        )
        self.masks = [DynamicMask(pos=pos, **mask_kwargs)
                      for pos in self.positions.values()]

        # Targets
        # NOTE: Probably inefficient to load images twice, but
        # I was having problems trying to copy the image
        # to each location.
        image_kwargs = dict(win=self.win, size=pic_size)
        self.left_pics = load_images(unipath.Path(self.STIM_DIR, 'pics'),
                                     pos=self.positions['left'],
                                     **image_kwargs)
        self.right_pics = load_images(unipath.Path(self.STIM_DIR, 'pics'),
                                      pos=self.positions['right'],
                                      **image_kwargs)

        self.word = visual.TextStim(**text_kwargs)

        self.timer = core.Clock()

        feedback_dir = unipath.Path(self.STIM_DIR, 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(unipath.Path(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(unipath.Path(feedback_dir, 'bleep.wav'))

    def run_trial(self, trial):
        cue = self._select_cue(trial['cue'])
        cue_dur = cue.getDuration()

        target_stims = []
        if trial['response_type'] == 'pic':
            pics = self._make_pics(trial['target'], trial['target_loc'])
            target_stims.extend(pics)
        elif trial['response_type'] == 'word':
            self.word.setText(trial['target'])
            word_loc = trial['target_loc']
            self.word.setPos(self.positions[word_loc])
            target_stims.append(self.word)
        else:
            raise NotImplementedError(
                'bad response_type %s' % trial['response_type']
            )

        stim_during_cue = []
        if trial['mask_type'] == 'mask':
            stim_during_cue.extend(self.masks)

        # Only allow the trial to continue if they hit the correct keys
        acceptable_keys = self.response_keys[trial['response_type']]

        # Clear keyboard buffer
        # Necessary since we are using event.getKeys instead of
        # event.waitKeys, which automatically clears the buffer.
        event.clearEvents()

        # Begin trial presentation
        # ------------------------
        self.fix.autoDraw = True
        for frame in self.frames:
            frame.autoDraw = True
        self.win.flip()
        core.wait(self.waits['fixation_duration'])

        # Play cue (and show mask)
        self.timer.reset()
        cue.play()
        while self.timer.getTime() < cue_dur:
            [stim.draw() for stim in stim_during_cue]
            self.win.flip()
            core.wait(0.01)

        # Interval between cue offset and target onset
        self.win.flip()
        core.wait(self.waits['cue_offset_to_target_onset'])

        # Show the target
        for target in target_stims:
            target.draw()
        self.timer.reset()
        self.win.flip()
        core.wait(self.waits['target_duration'])

        # Collect response
        self.fix.autoDraw = False
        for frame in self.frames:
            frame.autoDraw = False
        self.prompt.draw()
        self.win.flip()

        # Record all key presses that occur during the response window,
        # breaking only if a correct key was pressed
        keys_pressed = []
        while self.timer.getTime() < self.waits['response_window']:
            response = event.getKeys(keyList=self.all_response_keys,
                                     timeStamped=self.timer)
            if response:
                key, rt = response[0]
                keys_pressed.append(key)

                if key in acceptable_keys:
                    response = acceptable_keys[key]
                    break
        else:
            # Only executed if while condition becomes false,
            # indicating a timeout
            rt = self.waits['response_window']
            response = 'timeout'

        self.win.flip()
        # ----------------------
        # End trial presentation

        is_correct = int(response == trial['correct_response'])

        self.feedback[is_correct].play()

        if response == 'timeout':
            if len(keys_pressed) == 0:
                self.show_screen('timeout')
            else:
                # they responded, they just used the wrong keys
                self.show_screen('wrong_keys')

        core.wait(self.waits['iti'] - rt)

        trial['keys_pressed'] = ' '.join(keys_pressed)
        trial['response'] = response
        trial['rt'] = rt * 1000
        trial['is_correct'] = is_correct

        return trial

    def show_screen(self, name):
        if name == 'instructions':
            self._show_instructions()
        elif name in self.texts:
            self._show_screen(text=self.texts[name])
        else:
            raise NotImplementedError('%s is not a valid screen' % name)

    def _show_screen(self, text):
        visual.TextStim(text=text, **self.screen_text_kwargs).draw()
        self.win.flip()
        response = event.waitKeys(keyList=['space', 'q'])[0]

        if response == 'q':
            core.quit()

    def _show_instructions(self):
        instructions = sorted(self.texts['instructions'].items())

        main = visual.TextStim(**self.screen_text_kwargs)

        paragraph_kwargs = dict(self.screen_text_kwargs)
        paragraph_kwargs['height'] = 20
        top = visual.TextStim(pos=(0, 300), **paragraph_kwargs)

        for i, info in instructions:
            advance_keys = ['space', 'q']

            if 'main' in info:
                main.setText(info['main'])
                main.draw()

            if 'top' in info:
                top.setText(info['top'])
                top.draw()

            tag = info.pop('tag', None)
            if tag == 'donkey':
                self.fix.draw()
                for pic in self._make_pics(tag, 'right'):
                    pic.draw()
                advance_keys = ['right', 'q']
            elif tag == 'cue':
                self.win.flip()
                event.waitKeys(keyList=['space', ])

                top.draw()
                cue = self._select_cue('elephant')
                cue_dur = cue.getDuration()
                cue.play()
                self.fix.draw()
                self.win.flip()
                core.wait(cue_dur)

                top.draw()
                self.fix.draw()
                for pic in self._make_pics('elephant', 'left'):
                    pic.draw()
                advance_keys = ['left', 'q']
            elif tag == 'word':
                self.win.flip()
                event.waitKeys(keyList=['space', ])

                top.draw()
                cue = self._select_cue('chair')
                cue_dur = cue.getDuration()
                cue.play()
                self.fix.draw()
                self.win.flip()
                core.wait(cue_dur)

                top.draw()
                self.fix.draw()
                self.word.setText('chair')
                self.word.setPos(self.positions['left'])
                self.word.draw()
                advance_keys = ['up', 'q']
            elif tag == 'mask':
                self.fix.draw()
                for mask in self.masks:
                    mask.draw()

            self.win.flip()
            response = event.waitKeys(keyList=advance_keys)[0]

            if response in ['left', 'right', 'up', 'down']:
                self.feedback[1].play()

            if response == 'q':
                core.quit()

    @property
    def screen_text_kwargs(self):
        if not hasattr(self, 'screen_text_kwargs'):
            self._screen_text_kwargs = dict(
                win=self.win,
                font='Consolas',
                color='black',
                height=30,
                wrapWidth=800,
            )
        return self._screen_text_kwargs

    def _make_pics(self, target, target_loc):
        left = self.left_pics[target]
        right = self.right_pics[target]

        if target_loc == 'left':
            left.setOri(0)
            right.setOri(180)
        else:
            left.setOri(180)
            right.setOri(0)

        pics = [left, right]
        return pics

    def _select_cue(self, name):
        """ Create a list of possible cues given a name.

        There are multiple versions of each cue.
        e.g., "alligator-1", "alligator-2"
        """
        cue_versions = [snd for n, snd in self.cues.items()
                        if n.find(name) == 0]
        return random.choice(cue_versions)

def main():
    participant_data = get_subj_info(
        'gui.yaml',
        # check_exists is a simple function to determine if the data file
        # exists, provided subj_info data. Here it's used to check for
        # uniqueness in subj_ids when getting info from gui.
        check_exists=lambda subj_info:
            Participant(**subj_info).data_file.exists()
    )

    participant = Participant(**participant_data)
    trials = Trials.make(**participant)

    experiment = Experiment()
    experiment.show_screen('instructions')

    participant.write_header(trials.COLUMNS)

    for block in trials.iter_blocks():
        block_type = block[0]['block_type']

        for trial in block:
            trial_data = experiment.run_trial(trial)
            participant.write_trial(trial_data)

        experiment.show_screen(block_type)

    experiment.show_screen('end')
    import webbrowser
    webbrowser.open(experiment.survey_url.format(**participant))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    command_choices = ['main', 'maketrials', 'singletrial', 'instructions', 'survey']
    parser.add_argument('command', choices=command_choices,
                        nargs='?', default='main')

    default_trial_options = dict(
        block_type='practice',
        cue='elephant',
        response_type='word',
        target='elephant',
        target_loc='left',
        mask_type='mask',
        correct_response='up',
    )

    for name, default in default_trial_options.items():
        parser.add_argument('--%s' % name, default=default,
                            help='singletrial command option, default is %s' % default)

    parser.add_argument('--seed', default=101,
                        help='maketrials command option')

    args = parser.parse_args()

    if args.command == 'maketrials':
        trials = Trials.make(seed=args.seed)
        trials.write_trials('sample_trials.csv')
    elif args.command == 'singletrial':
        trial = dict(default_trial_options)
        for name in default_trial_options:
            if hasattr(args, name):
                trial[name] = getattr(args, name)

        experiment = Experiment()
        trial_data = experiment.run_trial(trial)

        import pprint
        pprint.pprint(trial_data)
    elif args.command == 'instructions':
        experiment = Experiment()
        experiment._show_instructions()
        experiment.show_screen('test')
    elif args.command == 'survey':
        experiment = Experiment()
        import webbrowser
        webbrowser.open(experiment.survey_url.format(subj_id='TEST_SUBJ', computer='TEST_COMPUTER'))
    else:
        main()
