Orientation discrimination
==========================

This cognitive science research project intends to determine the extent to
which our knowledge of what things look like is represented in long-term memory
in a visual format. In this experiment, we exploit the fact that hearing
the name of an object speeds its subsequent recognition. In this case, hearing
the name of an object improves the speed at which an upright object can be
discriminated from an upside down object. For example, hearing the word
"dog" makes you faster to select the upright dog picture. This phenomenon,
which we call the cueing effect, depends on knowing what objects look like.
If this knowledge is represented visually, then presenting visual interference
should be sufficient to reduce the cueing effect.

This repo includes the materials, methods, and results of our experiments
designed to address these questions. The experiments are run using python, and
the analyses are done in R. The results of the experiments can be downloaded as
an R package from this repo.

To run the experiment using python, you need to clone the repo and install
the required python packages. It is recommended that you set up a virtualenv
to keep the python packages for this experiment isolated from the rest of your
system::

    $ virtualenv --python=python2 ~/.venvs/orientation
    $ source ~/.venvs/orientation/bin/activate
    (orientation)$ git clone https://github.com/lupyanlab/orientation-discrimination.git
    (property)$ cd orientation-discrimination/experiment
    (property)experiment/$ pip install -r requirements.txt
    (property)experiment/$ python dualmask.py  # Run the dual mask experiment

To download the data in this repo as an R package, you need to have the
devtools package installed, and then you can run this command in R to
download the data::

    > devtools::install_github("lupyanlab", "orientation-discrimination",
                               subdir = "orientationdiscrimination")
    > library(orientationdiscrimination)
    > data("dualmask") # Load the data for the dual mask experiment

For our interpretation of the results, see the reports directory, or the
OSF page for this research at <https://osf.io/yt6fh/>. The manuscript
reporting our initial findings is under review.
