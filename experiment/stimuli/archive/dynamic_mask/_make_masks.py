from Tkinter import *
import random
import math
import os
# needs Python Image Library (PIL)
import Image, ImageDraw


## Define the width of the stimulus (the whole image) and the maximum width of 
## individual squares
stimWidth = 288
canvasHeight = canvasWidth = stimWidth
maxSqWidth = 50
numSquares = 600

## Specifies the colors of the squares
colors = ['#0000FF','#00FFFF',"#00FF00",'#FF00FF','#FF0000','#FFFFFF','#FFFF00']

## Creates 40 stimuli
for img in range (1,40):
    
    ##Creates a canvas with a 0 pix border (add padding to with and height if desired
    canvas= Canvas(width=canvasWidth, height=canvasHeight, bg='#999999')
    canvas.pack(expand=YES, fill=BOTH) 

    ## PIL create an empty image and draw object to draw on memory only, not visible
    image1 = Image.new("RGB", (stimWidth, stimWidth),'darkGray')
    draw = ImageDraw.Draw(image1)

    ## Creates randomly colored squares in random positions, restricted by max
    ## square width.
    for square in range(1,numSquares):
            x0 = random.randrange(0,stimWidth)
            y0 = random.randrange(0,stimWidth)
            x1 = random.randint(x0-maxSqWidth,x0+maxSqWidth)
            y1 = random.randrange(y0-maxSqWidth,y0+maxSqWidth)
            color = colors[random.randrange(0,7)]
            canvas.create_rectangle(x0, y0, x1, y1, width=0, fill=color)

            # do the PIL image/draw (in memory) drawings
            draw.rectangle([x0, y0, x1, y1], fill=color)


    # PIL image can be saved as .png .jpg .gif or .bmp file
    print img
    filename = 'mask_'+str(img)+".png"
    image1.save(filename)

    # to test, view the saved file, works with Windows only
    # behaves like double-clicking on the saved file
    #os.startfile(filename)

mainloop()


