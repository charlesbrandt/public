# Setup System

These can get personal. Use the tools that work for you!


## Git

Git is installed by default on some systems. Others may need it. Be sure to grab it:

    sudo apt install git

Then you can clone this repository:

    git clone --recurse-submodules https://github.com/charlesbrandt/public
    
[Notes on using git](../code/version_control/git.md)

If you forgot to get submodules:

    cd public
    git submodule update --init --recursive

[Version Control](../code/version_control/README.md)


## Emacs

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

see also 
[editors/emacs/emacs.md](editors/emacs/emacs.md)

Or more generally
[text editors](editors/editors.md)


## Browsers

[browsers](browsers.md)


## Password Managers

see also administration/password_managers.md

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


## K4DirStat

visual disk usage utility:
http://kdirstat.sourceforge.net/

    sudo apt-get install k4dirstat

## Docker

~/public/system/virtualization/docker.md  
~/public/system/virtualization/docker-compose.md  

## Chromium

This method installs the snap version.

    sudo apt install chromium-browser

If you want to pull from the canonical repository, do this first. (No longer required with Ubuntu 20.04)

```
sudo add-apt-repository ppa:canonical-chromium-builds/stage
sudo apt update
```


## Recorder

### OBS

Audio / Video / Broadcast / Stream

All in one. 

    sudo apt-get install obs-studio

### VLC 

VLC is usually installed with OBS

    sudo apt-get install vlc

Open VLC
Tools -> Preferences
Uncheck:
   - Integrate video in interface
   - Use only one instance when started from file manager
   - Show Systray Icon
   

View -> Playlist
Close browser tree in playlist view. (drag panel closed via middle)

View -> Advanced Controls (check / enable)

(Doesn't seem to have any effect, messages still appear:)
Gnome3 -> Settings -> Applications -> VLC media player -> Notifcations -> Off
can just go 'do not distrub'

Change "Continue where left off" from Ask to Never

### Utilities

    sudo apt-get install ffmpeg


## VS Code

[VS Code](editors/vs_code/vs_code.md)
