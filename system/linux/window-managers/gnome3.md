# Gnome 3

Super L or Super R (often a 'windows' key) will usually bring up launcher for launching tasks

Open `Settings` application to make many of these changes.

## Resetting

Sometimes the Gnome window manager will consume a lot of memory. When running with X11 (not Wayland), it's possible to restart the window manager without restarting all the applications. 

Quickest:

Alt-F2

type `r` in the window that pops up

## Zoom

When screen sharing, it can be useful to zoom in on parts of the screen to make things easier to read. Gnome can do this, but it must be enabled first:

[gnome - How to zoom screen on Ubuntu 20.04? - Ask Ubuntu](https://askubuntu.com/questions/1312980/how-to-zoom-screen-on-ubuntu-20-04)

You should be able to do this if you're using Gnome (which you should be IMHO)

- Super + Alt + 8 : Toggle zoom enabled/disabled (when enabled, the next two keyboard shortcuts become active)
    
- Super + Alt + '+' : Zoom in (increases zoom factor by 1.0)
    
- Super + Alt + '-' : Zoom out (decreases zoom factor by 1.0, until it is 1.0)

I was not able to locate the Zoom increments settings in `gnome-tweaks` (2024.01.13)

https://askubuntu.com/questions/1312980/how-to-zoom-screen-on-ubuntu-20-04  
gnome - How to zoom screen on Ubuntu 20.04? - Ask Ubuntu  

https://duckduckgo.com/?t=ffab&q=on+linux+gnome%2C+is+there+a+way+to+zoom+in+and+out+on+a+screen+the+way+there+is+on+mac%3F+&ia=web  
on linux gnome, is there a way to zoom in and out on a screen the way there is on mac? at DuckDuckGo  


## Dock Settings

Settings -> Appearance -> Auto-hide the Dock

Auto-hide the Dock (Settings->Appearance->Dock)

Update Blank Screen Preferences (Settings->Power->Blank Screen)

## Desktop Icons

Settings -> Appearance -> Desktop Icons -> Toggle "Show Personal folder"


## Dark Mode

Good for testing if web pages respond to the operating system dark mode preference:

Settings -> Appearance -> Dark Mode

https://itsfoss.com/dark-mode-ubuntu/  
How to Enable Full Dark Mode in Ubuntu 20.04  


## Alert Sounds

Disable alert sounds like screenshot photo sound:

```
dconf write /org/gnome/desktop/sound/event-sounds "false"
```

[via](https://unix.stackexchange.com/questions/444681/how-to-turn-off-alert-sounds-sound-effects-on-gnome-from-terminal)

If the `dconf` method does not work, a manual work around is to make all of the sound files silent. 

AKA create silent versions of them all.

Open Audacity. Generate 1 second silent audio. 

System alert sounds are stored here:

```
cd /usr/share/sounds/Yaru/stereo
```

Bring in the silence .ogg file 

```
sudo cp /path/to/silence-1sec.ogg .
sudo chmod 644 silence-1sec.ogg

sudo cp silence.ogg audio-volume-change.oga
sudo cp silence.ogg battery-low.oga
sudo cp silence.ogg bell.oga
sudo cp silence.ogg complete.oga
sudo cp silence.ogg desktop-login.oga
sudo cp silence.ogg desktop-logoff.oga
sudo cp silence.ogg device-added.oga
sudo cp silence.ogg device-removed.oga
sudo cp silence.ogg dialog-error.oga
sudo cp silence.ogg dialog-question.oga
sudo cp silence.ogg dialog-warning.oga
sudo cp silence.ogg message-new-email.oga
sudo cp silence.ogg message-new-instant.oga
sudo cp silence.ogg message.oga
sudo cp silence.ogg power-plug.oga
sudo cp silence.ogg power-unplug.oga
sudo cp silence.ogg system-ready.oga
sudo cp silence.ogg trash-empty.oga

```

I needed to restart for this change to take effect. Eventually they were replaced / reinstalled automatically. 

[via](https://askubuntu.com/questions/557389/how-can-i-disable-all-ubuntu-sounds)

The move approach for the sound files were ultimately re-added by the system. 

```
cd /usr/share/sounds/Yaru/stereo
sudo mkdir originals

sudo mv * originals/
```

## Gnome Tweaks

Some configurations are not exposed via Settings. 

For those there is the Tweaks application. This is where you can find the Themes setting

For everything else, there is dconf-editor.

```
sudo apt-get install gnome-tweaks
```

## Titlebar Buttons

Adjust window button placement 

Use Tweaks application to move close, minimize, and maximize to the left of the Window Titlebars.

    Tweaks -> Window Titlebars -> Placement -> left

## Desktop Background

    gsettings set org.gnome.desktop.background picture-uri file:///home/account/public/template.svg

To change the background color, it may be necessary to use:

    dconf-editor 

    gsettings set org.gnome.desktop.background primary-color '#DDDDDD'

It is possible to set by right clicking on the desktop
but so far I have had difficulty setting it to an image other than those provided by default. 


It is possible to do this via the CLI:

[2019.10.25 18:47:05]
gsettings set org.gnome.desktop.background picture-uri file:///home/account/public/16x10.png
gsettings set org.gnome.desktop.background picture-uri file:///home/account/public/16x10.svg

https://unix.stackexchange.com/questions/116541/how-to-refresh-the-desktop-background-in-various-desktop-environments/116806#116806
How to refresh the desktop background in various desktop environments? - Unix & Linux Stack Exchange

https://duckduckgo.com/?q=gnome+3+svg+desktop+background&t=ffhp&ia=web
gnome 3 svg desktop background at DuckDuckGo
https://unix.stackexchange.com/questions/101613/cant-set-background-wallpaper-in-gnome-3
gnome3 - Can't set background wallpaper in Gnome 3 - Unix & Linux Stack Exchange



## Laptop specific

### Automatic Brightness

Disable this under:

Settings -> Power -> Automatic Screen Brightness

It's annoying to have the screen dim if you get in front of the camera

### Show Battery Percentage

Settings -> Power -> Show Battery Percentage


### Disable Middle Click

For laptops with a touch pad, I prefer the following settings: 

Open gnome-tweaks and go to the "Keyboard and Mouse" tab and disable "Middle-click-Paste".

Also set the "Mouse Click Emulation" to be "Fingers"

https://www.google.com/search?client=ubuntu&channel=fs&q=gnome3+disable+mouse+buttons&ie=utf-8&oe=utf-8
gnome3 disable mouse buttons - Google Search
https://unix.stackexchange.com/questions/24330/how-can-i-turn-off-middle-mouse-button-paste-functionality-in-all-programs
xorg - How can I turn off "middle mouse button paste" functionality in all programs? - Unix & Linux Stack Exchange









## Login Screen

A bit tricky to find login screen customizations. 

    dconf-editor 

Navigate to: 

    /org/gnome/login-screen/

## Restarting

Restart GNOME by pressing Alt+F2 and running the command r (X.org) or log out and back in (Wayland). Do this whenever you want to apply and test a change of the code.

This restores functionality without a full system restart! 
This fixes when gnome-shell starts running full resources

https://github.com/mzur/gnome-shell-wsmatrix

https://duckduckgo.com/?t=ffab&q=gnome+restart+gnome-shell&ia=web  
gnome restart gnome-shell at DuckDuckGo  
https://www.linuxuprising.com/2020/07/how-to-restart-gnome-shell-from-command.html  
How To Restart GNOME Shell From Command Line Without Closing Any Applications (X11 Only) - Linux Uprising Blog  




