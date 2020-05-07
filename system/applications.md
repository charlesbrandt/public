# Setup System

These can get personal. Use the tools that work for you!


## Text Editor

If I could only choose one application, it would be a text editor. The power of the written word. The power of an idea. 

There are lots of good ones out there. I'll share some of my favorites:

### Emacs

Debian 10 comes with emacs! Wow! This might be the first time I haven't had to install it from a package! Thank you!

user@system:~$ ln -s share/public/system/editors/emacs/.emacs .emacs
user@system:~$ ln -s share/public/system/editors/emacs/.emacs.d/ .emacs.d
user@system:~$ emacs &

G2G!

see also 
~/public/system/editors/emacs/emacs.md

### vi

vi is everywhere. It's good to understand the different modes so that you can accomplish basic editing. Many systems also include nano, which has the same purpose (imho). 

### VS Code

Way to go!

My only complaint is it's very memory intensive. That's where some of the lighter weight editors really shine!


## Version Control System

Git won. Way to go!

### Git

Git is installed by default on some systems. Others may need it. Be sure to grab it:

~/public/code/version_control/git.md

If you need to see what repo something goes to:

    git config --list --show-origin


## Browsers

[browsers](browsers.md)


## Recorder

### OBS

Audio / Video / Broadcast / Stream

All in one. 

    sudo apt-get install obs-studio

### Audacity

Audio Editor

    sudo apt-get install audacity



## Media

### VLC 

    sudo apt-get install vlc

Open VLC
Tools -> Preferences
Uncheck:
   - Integrate video in interface
   - Use only one instance when started from file manager

View -> Playlist
Close browser tree in playlist view. (drag panel closed via middle)
Uncheck "Docked Playlist

May need
Tools-->Preferences-->Video-->Output-->X11 Video Output (XCB)


## Inkscape

sudo apt-get install inkscape


## TODO

sudo apt-get install krita

cad?

sudo apt-get install blender
sudo apt-get install gimp
