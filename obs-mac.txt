*2017.05.13 14:10:50 todo
cut plywood for:
  - bathroom sink area

*2017.05.13 14:10:29 todo
PD (puredata) for android?

*2017.05.13 14:09:27 streaming screencasting learning learn_to_learn
It is helpful to be able to make screencasts when using a system
this is a helpful tool for documentation
it is still a good idea to distill those steps into written instructions with screencaptures (static images). In some cases that is sufficient (and far more effecient as far as space is concerned). It does require reading, but reading can be a lot quicker than parsing a video. 

Anyway, I'm just learning how to use these tools and Open Broadcasting System (OBS) seems to be a good choice for cross platform use. It didn't work well in a virtualized Windows system, but there are other options for that (or just capture at the root OS (linux) level using OBS... just grab the VM window).

Getting set up on a mac is a bit trickier than on linux, but it is doable.

Download and install OBS:

    https://obsproject.com/

you will also need soundflower:

    https://github.com/mattingalls/Soundflower

Good instructions for monitoring sound output while patched to soundflower included here:

    https://github.com/mattingalls/Soundflower/releases/tag/2.0b2

Relevant bits:

```
you can do a lot with a Multi-Output Aggregate Device. Here's an example:

Say i want to play some audio in iTunes, but record it in Quicktime while listening to it out of the built-in speakers. Here is how i would set it up:

*in iTunes:

start playing audio

Open Audio MIDI Setup: (found in /Applications/Utilities)

hit the '+' button in the bottom left corner and select "Create Multi Output Device"
in the the panel that appears on the right, select "Built-in Output" AND "Soundflower (2ch)"
then hit the button with the gear icon bottom left and select "use this device for sound output" (you should still be hearing your iTunes output -- it is now going to both Soundflower and the built-in speakers)

```

Back in OBS, be sure to add a display source. Also, on retina macbooks, need to set the video properties manually:

    Base (Canvas) Resolution: 2880 x 1800
    Output (Scaled) Resolution: 1440 x 900

In Audio settings, be sure to leave things set to "Default" (checking the Soundflower (2ch) source didn't work for me).
And back in Output, it's OK to use mkv and then reflow the file to mp4 afterwards. (or can risk it, and go straight to mp4... see warning in UI)
