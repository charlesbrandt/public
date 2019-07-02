# Steps for getting a termux install going

A physcial keyboard makes things a lot easier!

Early on, before you have a text editor available, it can be handy to use the script command to log what you're doing:

    script -a test.txt

see also [topics of interest](topics_of_interest.txt)


## Termux extras

Set up shared storage. Enable it for the app in android system settings. Then:
    
   termux-setup-storage

The termux API's require another app to be installed from the app store, but seem like they could be handy for interacting with the underlying system. Once that app is installed, enable it in termux with:

   apt install termux-api


## Core tools

   pkg update

install git:

   pkg install git

install emacs:

   pkg install emacs
     

Check out repos locally. You'll need to use a git server to sychronize repositories. For ones that are public, put them in a public directory

   mkdir public

For now there aren't any other apps that are making use of these files. No need to bury them deep (from Termux's home perspective) in the shared/ folder.

   git clone https://github.com/charlesbrandt/system public/system


   ln -s public/system/editors/emacs/.emacs.d/ .emacs.d
   ln -s public/system/editors/emacs/.emacs .emacs


## Python

pkg install python
pip install --upgrade pip
python -V
Python 3.7.3
$ pip install ipython

## SSH

SSH is not available by default. 

   ssh charles@192.168.2.183
   The program 'ssh' is not installed. Install it by executing:
     pkg install dropbear
   or
     pkg install openssh
     
TODO: not sure what dropbear is

    pkg install openssh
