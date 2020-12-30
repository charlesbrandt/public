# Raspberry Pi


## Parts

There are many types of Pi computers. Choose the one that best fits your project's use case. At a minimum you'll need:

  - Raspberry Pi Computer
  - Power Supply
  - Memory Card

I also often like a power switch if the device will not be powered on all of the time. 


## Distributions

Choose a distribution.

### Ubuntu

Ubuntu has a server distribution tailored to the pi. For newer Pi's (e.g. >= 4, 400) you can also download a desktop version of Ubuntu. 

https://ubuntu.com/download/raspberry-pi

Choosing Ubuntu allows easy installation of any other packages in the Ubuntu / Debian ecosystem (as long as the software provides ARM binaries). 

### Rasbian

Raspbian is another popular choice. 

Download an image from:
Raspberry Pi Guide - Quick Start Guide for Raspberry Pi
https://www.raspberrypi.org/downloads/


## Flash the image

After choosing and downloading the OS image

### Etcher

Download and run Etcher:

https://etcher.io/

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


## Rasbian Configuration

trying to remove overscan which results in a black border for pi display
1824x984
sudo raspi-config
Advanced -> Overscan

After a restart, the issue was fixed!

*2017.11.05 12:03:46
see also
https://city-of-bloomington.github.io/timetrack/

https://github.com/City-of-Bloomington/timetrack



