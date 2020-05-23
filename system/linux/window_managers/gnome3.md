# Gnome 3

Super L or Super R (often a 'windows' key) will usually bring up launcher for launching tasks

Many configurations are not exposed via the Settting. 

There is also the Tweaks application (why 2?). This is where you can find the Themes setting

For everything else, there is dconf-editor.

## Gnome Tweaks

    sudo apt-get install gnome-tweaks

## Dock Settings

Settings -> Appearance -> Auto-hide the Dock

## Alert Sounds

Disable alert sounds like screenshot photo sound:

    dconf write /org/gnome/desktop/sound/event-sounds "false"

via:

https://unix.stackexchange.com/questions/444681/how-to-turn-off-alert-sounds-sound-effects-on-gnome-from-terminal

## Titlebar Buttons

Use Tweaks application to move close, minimize, and maximize to the left of the Window Titlebars.

## Desktop Icons

Do you want to see icons on the desktop? 

Tweaks -> Extensions -> Desktop Icons -> Settings Icon -> Show icons

Click on the settings icon next to the toggle to see additional settings for e.g. show Trash on desktop

## Automatic Brightness

disable this under Settings -> Power ->
it's annoying to have the screen dim if you get in front of the camera


## Disable Middle Click

For laptops with a touch pad, I prefer the following settings: 

Open gnome-tweak-tool and go to the "Keyboard and Mouse" tab and disable "Middle-click-Paste".

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
    
## Desktop Background

This is also available as a setting in the Tweaks application

To change the background color, it may be necessary to use:

    dconf-editor 

    gsettings set org.gnome.desktop.background primary-color '#000000'

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



## Keyboard Backlight

By default, the keyboard backlight will time out after 10s on Ubuntu. Typically, if I turn on the backlight, I would prefer for it to stay on until I choose otherwise. This can be configured in Ubuntu:

To change the keyboard backlight time-out counter you will need to modify stop_timeout. You will find stop_timeout located in this directory:

    /sys/devices/platform/dell-laptop/leds/dell\:\:kbd_backlight/

Changes to this file are performed in Terminal.

    sudo vi /sys/devices/platform/dell-laptop/leds/dell\:\:kbd_backlight/stop_timeout
    
Tried using '0s'
However, I'm unable to save the changes. 

via:
https://www.google.com/search?q=keyboard+backlight+timeout+ubuntu
keyboard backlight timeout ubuntu - Google Search
https://www.dell.com/support/article/us/en/04/sln308123/how-to-configure-the-keyboard-backlight-time-out-interval-in-ubuntu-linux?lang=en
How to Configure the Keyboard Backlight Time-Out Interval in Ubuntu Linux | Dell US

## .local Domain Warning

    sudo vi /etc/avahi/avahi-daemon.conf 
    
uncomment and change the line with domain name to:
    
    domain-name=.alocal

https://www.google.com/search?q=network+service+discovery+disabled+ubuntu
network service discovery disabled ubuntu - Google Search
https://askubuntu.com/questions/339702/network-service-discovery-disabled-what-does-this-mean-for-me
wireless - Network service discovery disabled: What does this mean for me? - Ask Ubuntu

## Hot Corner (See all windows)

Can also use the Tweaks application to enable the "Activities Overview Hot Corner" in the Top Bar settings.

standard guesture control
  - Three finger swipe to different workspace
Is there a desktop that allows seeing all windows currently open on a workspace (similar to mac os x)? Possible to use three finger gesture?

Gnome3 has a very similar feature!! (Currently configured to be in the upper left hand corner of screen


