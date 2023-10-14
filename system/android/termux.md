# Termux

Termux is like a minimal linux container running on your phone that you have full access to. 

Install F-Droid first. It's a good resource to have anyway. Allows browsing and installing open source apps.

I now need to download f-droid directly from their website. It is delivered as an APK module that needs to be installed outside of the Google Play store. 

From the motd:
The Google Play version of the Termux app no longer
receives updates. For more information, visit:
https://wiki.termux.com/wiki/Termux_Google_Play

A physcial keyboard makes things a lot easier!

## Core tools

```
pkg update
```

install git:

```
pkg install git
```

install emacs:

```
pkg install emacs
```

Check out repos locally. You'll need to use a git server to synchronize repositories. 

```
git clone https://gitlab.com/charlesbrandt/public

ln -s public/system/editors/emacs/.emacs.d/ .emacs.d
ln -s public/system/editors/emacs/.emacs .emacs
```

### SSH

SSH is not available by default. 

```
pkg install openssh
````

This is also a good chance to generate ssh keys to simplify connecting to primary servers:

```
ssh-keygen -t rsa
ssh-copy-id demo@198.51.100.0
```

`dropbear` is very lightweight... so much so it does not have `ssh-copy-id`. Better to go with `openssh` if you need that

```
pkg install dropbear
```    


### Rsync

Another good one to have

```
pkg install rsync
```


for 'ps' cli command in termux:

```
pkg install procps
```

## Sessions

https://wiki.termux.com/wiki/User_Interface

> The navigation drawer is revealed by swiping inwards from the left part of the screen (if you have gesture navigation enabled in Android, you will need to hold briefly on the edge of the screen before swiping to bring up the navigation drawer instead of going back in Android). 

## Termux configuration

https://wiki.termux.com/wiki/Terminal_Settings

The Termux terminal can be configured by creating the file ~/.termux/termux.properties. This file uses a simple key=value property syntax and allows configuring the properties listed below.

After changing the properties file the changes will take effect either by executing termux-reload-settings or by restarting the Termux app (closing all sessions and starting the app again). 

I've included my copy of the configuration in this repository 

```
mkdir .termux
cp public/system/android/termux.properties .termux/
```    

### Extra keys

Extra Keys row is also configurable in the same file:
https://wiki.termux.com/wiki/Touch_Keyboard


extra-keys = [ \
 ['ESC','|','/','-','_','UP','TAB'], \
 ['[',']','CTRL','ALT','LEFT','DOWN','RIGHT'] \
]



extra-keys = [ \
 ['[','|','/','-','_','UP','ESC'], \
 [']','TAB','CTRL','ALT','LEFT','DOWN','RIGHT'] \
]


extra-keys = [ \
 ['ESC','|','/',' ',' ','UP'], \
 [' ','TAB','CTRL','ALT','LEFT','DOWN','RIGHT'] \
]


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

## Termux API

The termux API's require another app to be installed from the app store, but seem like they could be handy for interacting with the underlying system. Once that app is installed, enable it in termux with:

    apt install termux-api


More info is available:

https://wiki.termux.com/wiki/Termux:API

https://wiki.termux.com/wiki/Termux-microphone-record

Current API implementations

termux-battery-status
    Get the status of the device battery.
termux-brightness
    Set the screen brightness between 0 and 255.
termux-call-log
    List call log history.
termux-camera-info
    Get information about device camera(s).
termux-camera-photo
    Take a photo and save it to a file in JPEG format.
termux-clipboard-get
    Get the system clipboard text.
termux-clipboard-set
    Set the system clipboard text.
termux-contact-list
    List all contacts.
termux-dialog
    Show a text entry dialog.
termux-download
    Download a resource using the system download manager.
termux-fingerprint
    Use fingerprint sensor on device to check for authentication.
termux-infrared-frequencies
    Query the infrared transmitter's supported carrier frequencies.
termux-infrared-transmit
    Transmit an infrared pattern.
termux-job-scheduler
    Schedule a Termux script to run later, or periodically.
termux-location
    Get the device location.
termux-media-player
    Play media files.
termux-media-scan
    MediaScanner interface, make file changes visible to Android Gallery
termux-microphone-record
    Recording using microphone on your device.
termux-notification
    Display a system notification.
termux-notification-remove
    Remove a notification previously shown with termux-notification --id.
termux-sensor
    Get information about types of sensors as well as live data.
termux-share
    Share a file specified as argument or the text received on stdin.
termux-sms-list
    List SMS messages.
termux-sms-send
    Send a SMS message to the specified recipient number(s).
termux-storage-get
    Request a file from the system and output it to the specified file.
termux-telephony-call
    Call a telephony number.
termux-telephony-cellinfo
    Get information about all observed cell information from all radios on the device including the primary and neighboring cells.
termux-telephony-deviceinfo
    Get information about the telephony device.
termux-toast
    Show a transient popup notification.
termux-torch
    Toggle LED Torch on device.
termux-tts-engines
    Get information about the available text-to-speech engines.
termux-tts-speak
    Speak text with a system text-to-speech engine.
termux-usb
    List or access USB devices.
termux-vibrate
    Vibrate the device.
termux-volume
    Change volume of audio stream.
termux-wallpaper
    Change wallpaper on your device.
termux-wifi-connectioninfo
    Get information about the current wifi connection.
termux-wifi-enable
    Toggle Wi-Fi On/Off.
termux-wifi-scaninfo
    Get information about the last wifi scan.


## Python

```
pkg install python
pip install --upgrade pip
python -V

pip install ipython

pip install pipenv
```


## Sqlite3

```
pkg install sqlite
```

Then can run it with command

```
sqlite3
```

https://sqlite.org/cli.html

```
.databases
.tables
```

## Upgrades

Don't forget to run 

```
apt list --upgradable
```

and
  
```
pkg upgrade
```

every now and again

## Steps for getting a termux install going

Early on, before you have a text editor available, it can be handy to use the script command to log what you're doing:

```
script -a test.txt
```

## Termux extras

Set up shared storage. Enable it for the app in android system settings. Then:
    
    termux-setup-storage



## Tmux

At this point, you probably want to be working on a remote server, not on a mobile device

```
pkg install tmux
```
    
## Notes

It's a good idea to work within the same editor rather than different console tab. This way copy and paste are using the same buffer. Emacs / termux has a different clipboard buffer than the native android os.

Additionally, copying from termux using android often includes line continuation characters ('\'). Doesn't work well.

