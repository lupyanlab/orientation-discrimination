#!/usr/bin/env python
import os
from psychopy.visual import ImageStim

class DynamicMask(object):
    def __init__(self, loc, key, **kwargs):
        """
        :param loc: path to mask files
        :param key: str key to identify the correct set of masks
        :param **kwargs: args to pass to visual.ImageStim
        """
        mask_files = [os.path.join(loc, fname) for fname in os.listdir(loc) \
                         if fname.find(key) != -1]
        self.masks = [ImageStim(image = msk, **kwargs) for msk in mask_files]
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
