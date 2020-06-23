# Setup System

These can get personal. Use the tools that work for you!


## Version Control System

Keep your work in sync. 

### Git

Git is installed by default on some systems. Others may need it. Be sure to grab it:

    sudo apt install git

Then you can clone this repository:

    git clone https://github.com/charlesbrandt/public
    
    git submodule update --init --recursive

[Notes on using git](~/public/code/version_control/git.md)

If you need to see what repo something goes to:

    git config --list --show-origin


## Text Editor

If I could only choose one application, it would be a text editor. The power of the written word. The power of an idea. 

There are many good ones out there. 

### Emacs

    which emacs
    
To install it:

    sudo apt-get install emacs

Then configure it with:

```
cd
ln -s public/system/editors/emacs/.emacs .emacs
ln -s public/system/editors/emacs/.emacs.d/ .emacs.d
emacs &
```

G2G!

see also 
~/public/system/editors/emacs/emacs.md

### vi

vi is everywhere. It's good to understand the different modes so that you can accomplish basic editing. Many systems also include nano, which also works when you need an editor over a console. 

### VS Code

VS Code is a great environment for editing code. 

My only complaint is it's very memory intensive. If you like to keep a lot of editors open at the same time, some of the lighter weight editors may be better options.


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

(Doesn't seem to have any effect, messages still appear:)
Gnome3 -> Settings -> Applications -> VLC media player -> Notifcations -> Off
can just go 'do not distrub'


## Inkscape

    sudo apt-get install inkscape

[Inkscape Notes](../../design/inkscape.md)


## Image Editing

    sudo apt-get install krita


## Password Managers

    sudo apt-get install keepassx

https://www.google.com/search?q=open+source+password+manager&oq=open+source+passw&aqs=chrome.0.0j69i57j0l4.6551j1j7&client=ubuntu&sourceid=chrome&ie=UTF-8
open source password manager - Google Search
https://hackernoon.com/the-best-password-manager-for-you-747b92c43d18
The Best Password Manager for You – Hacker Noon
https://www.passwordstore.org/
Pass: The Standard Unix Password Manager
https://keepass.info/
KeePass Password Safe

Don't like this one as much. 

    sudo apt-get install keepass2


## Python

    sudo apt install python-is-python3

## Utilities

    sudo apt-get install curl
    sudo apt-get install ffmpeg

## Shotcut

    sudo apt-get install shotcut


## K4DirStat

visual disk usage utility:
http://kdirstat.sourceforge.net/

    sudo apt-get install k4dirstat

## Docker

~/public/system/virtualization/docker-compose.md
~/public/system/virtualization/docker.md


## Chromium

sudo add-apt-repository ppa:canonical-chromium-builds/stage
sudo apt update
sudo apt install chromium-browser

this installs the snap version
