# Setup System

Use the tools that work for you!


## Git

Git is installed by default on some systems. Others may need it. Be sure to grab it:

```
sudo apt install git
```

Then you can clone this repository:

```
git clone --recurse-submodules https://github.com/charlesbrandt/public
```

[Notes on using git](../code/version-control/git.md)

If you forgot to get submodules:

```
cd public
git submodule update --init --recursive
```

[Version Control](../code/version-control/)


## Emacs

```
which emacs
```

To install it:

```
sudo apt-get install emacs
```

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
[text editors](editors/)

## VS Code

[VS Code](editors/vs-code/vs-code.md)


## Browsers

[Browsers](browsers.md)


## Password Managers

See also [Password Managers](password-manager.md)

```
sudo apt-get install keepassx
```


## Python

```
sudo apt install python-is-python3
```

## Utilities

```
sudo apt-get install ffmpeg 

sudo apt-get install curl
```


## Recorder

### OBS

Audio / Video / Broadcast / Stream

All in one. 

```
sudo apt-get install obs-studio
```

### VLC 

VLC is usually installed with OBS

```
sudo apt-get install vlc
```

Open VLC
Tools -> Preferences
Uncheck:
   - Integrate video in interface
   - Use only one instance when started from file manager
   - Show Systray Icon
   - Change "Continue where left off" from Ask to Never
   

View -> Playlist
Close browser tree in playlist view. (drag panel closed via middle)

View -> Advanced Controls (check / enable)

(Doesn't seem to have any effect, messages still appear:)
Gnome3 -> Settings -> Applications -> VLC media player -> Notifcations -> Off
can just go 'do not distrub'



## Chromium

This method installs the snap version.

```
sudo apt-get install chromium-browser
```

If you want to pull from the canonical repository, do this first. (No longer required with Ubuntu 20.04)

```
sudo add-apt-repository ppa:canonical-chromium-builds/stage
sudo apt update
```


## Docker

[Docker](virtualization/docker.md)  
[Docker Compose](virtualization/docker-compose.md)  


## KVM

[KVM](virtualization/kvm.md)


## DBeaver

## Mongo Compass

(Try DBeaver for connecting to Mongo)

## Zoom

https://zoom.us/download?os=linux

Requires:

```
sudo apt-get install libgl1-mesa-glx libegl1-mesa libxcb-xtest0
```

Then install with

```
cd Downloads
sudo dpkg -i zoom_amd64.deb 
```

## Microsoft Teams

https://www.microsoft.com/en-us/microsoft-teams/download-app

Install with:

```
sudo dpkg -i teams_1.4.00.26453_amd64.deb 
```

```
sudo apt --fix-broken install
```

## Cypress

see [cypress](/code/test/cypress.md)



## IPFS

[IPFS](../code/api/ipfs.md)


## Screenshots

apt install flameshot

https://github.com/flameshot-org/flameshot
GitHub - flameshot-org/flameshot: Powerful yet simple to use screenshot software
https://flameshot.org/
Flameshot



## K4DirStat

With `Disk Usage Analyzer` included by default, this is less critical. However, I do think that Kdirstat's ability to scan files is more efficient than `Disk Usage Analyzer`

visual disk usage utility:
http://kdirstat.sourceforge.net/

    sudo apt-get install k4dirstat

