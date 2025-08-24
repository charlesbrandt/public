# Applications

Setup System

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


## Password Managers

See also [Password Managers](password-manager.md)

```
sudo apt-get install keepassxc
```

## Python

```
sudo apt-get install python-is-python3
```

Go ahead and grab pip here so it's available for things like `autopep8` for vscode. 

```
sudo apt-get install python3-pip
```

## Utilities

```
sudo apt-get install ffmpeg 
sudo apt-get install curl
```

```
sudo apt install build-essential
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


## VS Code

[VS Code](editors/vs-code/vs-code.md)


## Browsers

[Browsers](browsers.md)

## KVM

[KVM](virtualization/kvm.md)


## Docker

[Docker](virtualization/docker.md)  
[Docker Compose](virtualization/docker-compose.md)  




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


## tmux

Install and configure [tmux](./terminal/tmux.md)  


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


### File Browser 

[File Browser](filebrowser.md) 
