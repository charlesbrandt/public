# Pi Operating Systems

Choose the right foundation for the type of system you deploy. 

## Distributions

Choose a distribution.

### Ubuntu

Ubuntu has a server distribution tailored to the pi. For newer Pi's (e.g. >= 4, 400) you can also download a desktop version of Ubuntu. 

https://ubuntu.com/download/raspberry-pi

Choosing Ubuntu allows easy installation of any other packages in the Ubuntu / Debian ecosystem (as long as the software provides ARM binaries). 

### Raspberry Pi OS

Raspberry Pi OS is another popular choice. 

Download an image from:
Raspberry Pi Guide - Quick Start Guide for Raspberry Pi
https://www.raspberrypi.org/downloads/


## Ubuntu

https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#2-prepare-the-sd-card

https://ubuntu.com/download/raspberry-pi  
Install Ubuntu on a Raspberry Pi | Ubuntu  

If you're not installing a pre-made image configured for a specific application (e.g. Plex, Home Assistant, Motion Eye, etc...), this is a good foundation to start with. Can add a gui later if you're not sure if it's for desktop or server use. 

A 64bit OS is required to use more than 4GB of memory. If you have a Pi with 8GB of memory, find a 64 bit OS. 

If running 32bit os, don't buy anything bigger than 4GB of memory.

64 bit OS does not work with Raspberry Pi Zero W.

32 Bit ubuntu (20.04) did not boot raspberry pi zero w -- seems to just hang. 

### Change the default password

Once you boot up, you can log in with 

U: ubuntu
P: ubuntu

You will need to change this after logging in. Fire up your password manager to keep track. 

### Networking

These instructions cover setting up networking on the SD card before you boot the image:

https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#3-wifi-or-ethernet

In summary: Edit the `network-config` file on the system-boot partition

With ethernet you only need to plug in the cable and you should be assigned an IP address via DHCP. Ideally do this after you change the password, just to be sure. 

[Once the system is running, networking is configured like any other Ubuntu server.](/system/network.md)

### Variants

If you plan to use a Pi as a daily driver / desktop, you may want to customize the desktop / distribution:

https://ubuntu-pi-flavour-maker.org/download/



## Raspberry Pi OS

Hard to beat Raspberry Pi OS. Provides a common ground when documenting your projects: 

https://www.raspberrypi.org/downloads/raspbian/

Use for raspberry pi zero W

Maybe 32 Bit Ubuntu does not work with Raspberry Pi Zero W? 

In that case, 32 bit Raspberry Pi OS is the best choice?

may be good for pis intended for device service (e.g. camera

Can use a spare raspberry pi 4 (4GB memory) to run the install and set things up more efficiently)
(probably good enough for most server purposes)

<< --- install / setup on pi zero is slooooow

### Configuration

remove overscan which results in a black border for pi display
1824x984
sudo raspi-config
Advanced -> Overscan

After a restart, the issue was fixed!

### Raspbian

Previously, Raspberry Pi OS was called Raspbian. They're the same thing. Just something to be aware of if you come across documentation referring to Rasbian, you'll know. 







## Flash the image

After choosing and downloading the OS image

https://www.raspberrypi.org/software/

sudo apt install rpi-imager

run with `rpi-imager`

decide on a base OS

### Balena Etcher

Download and run Etcher:

https://etcher.io/

This has been adopted by Rasperry Pi and rebranded and expanded. At least the overall process feels the same.

Some good interesting projects shown during write phase.

https://www.balena.io/etcher/  
balenaEtcher - Flash OS images to SD cards & USB drives  


### Backing up

Before you re-use an SD card for a new device, be sure to back them up. 
Make an image. Ideally there may be a way to run it as a virtual machine if needed? At least a way to mount it as a file system and extract necessary information from the device. 

On linux, `dd` is a great option that's always available. 

1. Plug in card via card reader

```
sudo fdisk -l
sudo dd bs=4M if=/dev/sde of=~/MyImage.img
```

Ideally you'd try recreating the image in reverse to make sure everything works as expected. And use a different SD card. 

If you don't have one available for testing, try mounting the image and browsing the filesystem. [Ubuntu allows double-clicking the `.img` file to mount read-only?]


https://raspberrytips.com/create-image-sd-card/  
How to Create an Image of a Raspberry Pi SD Card? (Win/Linux/Mac) – RaspberryTips  
https://duckduckgo.com/?t=ffab&q=linux+mount+dd+image+&ia=web  
linux mount dd image at DuckDuckGo  
https://askubuntu.com/questions/445979/how-to-mount-sd-card-image-created-with-dd  
How to mount sd-card image created with dd? - Ask Ubuntu  

https://duckduckgo.com/?t=ffab&q=create+an+image+of+an+sd+card&ia=web  
create an image of an sd card at DuckDuckGo  



### dd

Run df -h to see what devices are currently mounted.
Connect Card.
Run df -h again.
(dmesg and mount are also useful commands to hone in on the correct device)

    umount /dev/sdi1

    sudo dd bs=4M if=/c/out-data/2015-05-05-raspbian-wheezy.img of=/dev/sdi

Via this guide for using dd to create images:
Installing Operating System Images - Raspberry Pi Documentation
https://www.raspberrypi.org/documentation/installation/installing-images/linux.md


## Install SD to Pi

After the flash is complete, insert the SD card into the Pi and connect all the peripherals. Power on the Pi. 



## See also

Once the base OS has been installed, explore details about how you want to configure the system:

[Linux Notes](/system/linux/index.md)

On a pi server, installing docker is a good idea:




Guide for the timetrack project that covers many similar 'getting started' topics

https://city-of-bloomington.github.io/timetrack/

https://github.com/City-of-Bloomington/timetrack

