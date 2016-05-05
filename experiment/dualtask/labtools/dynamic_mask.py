#!/usr/bin/env python
import unipath
from psychopy.visual import ImageStim

class DynamicMask(object):
    def __init__(self, **kwargs):
        """
        :param **kwargs: args to pass to visual.ImageStim
        """
        labtools_dir = unipath.Path(__file__).absolute().parent
        mask_files = unipath.Path(labtools_dir, 'dynamic_mask').listdir('*.png')
        self.masks = [ImageStim(image = str(pth), **kwargs) for pth in mask_files]
        self.cur_ix = 0

    def draw(self):
        """ Draws a single mask """
        self.masks[self.cur_ix].draw()
        self.cur_ix = (self.cur_ix+1) % len(self.masks)

    def setPos(self, pos):
        """ Change the position for all masks"""
        for mask in self.masks:
            mask.setPos(pos)

    def reset(self):
        """ Reset the mask index counter """
        self.cur_ix = 0
