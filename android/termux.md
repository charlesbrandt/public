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


## Termux configuration

https://wiki.termux.com/wiki/Terminal_Settings

The Termux terminal can be configured by creating the file ~/.termux/termux.properties. This file uses a simple key=value property syntax and allows configuring the properties listed below.

After changing the properties file the changes will take effect either by executing termux-reload-settings or by restarting the Termux app (closing all sessions and starting the app again). 

I've included my copy of the configuration in this repository 

    mkdir .termux
    cp public/system/android/termux.properties .termux/
    

### Sessions

```
# Open a new terminal with...
# 3 keys does not seem to work:
# shortcut.create-session = ctrl + shift + n
shortcut.create-session = ctrl + '

# Go one session down with ...
shortcut.next-session = ctrl + .

# Go one session up 
shortcut.previous-session = ctrl + ,

# Rename a session
shortcut.rename-session = ctrl + ;
```

### Extra keys

Extra Keys row is also configurable in the same file:
https://wiki.termux.com/wiki/Touch_Keyboard

extra-keys = [ \
 ['ESC','TAB','/','CTRL','ALT','UP','DOWN'] \
]

Example configuration to enable 2-row (was in v0.65) extra keys:

extra-keys = [['ESC','/','-','HOME','UP','END','PGUP'],['TAB','CTRL','ALT','LEFT','DOWN','RIGHT','PGDN']]

The extra-keys definition itself can also be spread over multiple lines, if desired, by "backslash-escaping" the line feed at the end of each line, thus:

extra-keys = [ \
 ['ESC','|','/','HOME','UP','END','PGUP','DEL'], \
 ['TAB','CTRL','ALT','LEFT','DOWN','RIGHT','PGDN','BKSP'] \
]


## Core tools

    pkg update

for 'ps' cli command in termux:

    pkg install procps

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

pip install ipython

pip install pipenv


## SSH

SSH is not available by default. 

    ssh charles@192.168.2.183
    The program 'ssh' is not installed. Install it by executing:
      pkg install dropbear
    or
      pkg install openssh
     
TODO: not sure what dropbear is

    pkg install openssh
    
This is also a good chance to generate ssh keys to simplify connecting to primary servers:

    ssh-keygen -t rsa
    ssh-copy-id demo@198.51.100.0

## Rsync

Another good one to have

    pkg install rsync

## Sqlite3

    pkg install sqlite
    
Then can run it with command

    sqlite3
    
https://sqlite.org/cli.html

    .databases
    .tables


## Node

At this point you should be compiling the javascript application. hosting on a webserver somewhere.

develipment is better done on a full system. linux or mac. or virtual linux on windows.

see also userland.md()

both are similar in results for full node dev. (as of # 2019.08.25)


    pkg install nodejs

Not all npm modules are compatible with Termux. Userland isn't any better (worse in many ways!). Nuxt has problems... next to try is a more simple approach with Vue.

https://dev.to/joelnet/getting-nodejs-and-express-up-and-running-on-my-android-phone-3plp    


## Upgrades

Don't forget to run 

    apt list --upgradable
    
and
  
    pkg upgrade
    
every now and again

## Notes

It's a good idea to work within the same editor rather than different console tab. This way copy and paste are using the same buffer. Emacs / termux has a different clipboard buffer than the native android os.

Additionally, copying from termux using android often includes line continuation characters ('\'). Doesn't work well.

