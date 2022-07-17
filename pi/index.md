# Raspberry Pi

[<img src="./logos/RPi-Logo-Reg-SCREEN.svg" alt="raspberry pi logo" width="200px">](./logos/logos-raspberry-pi.md)

Need a computer?
This is a great place to start. 

## Getting started

- Choose your hardware
- Choose your operating system
- Choose any services

## Hardware

There are many types of Pi computers. Choose the one that best fits your project's use case. At a minimum you'll need:

  - Raspberry Pi Computer
  - Power Supply
  - Memory Card

I also often like having a physical power switch during testing and configuration or if the device will not be powered on all of the time. 


### Power

[power supplies](hardware/power-supply.md)  

In addition to a good power supply
it's important to make sure you have a good powered usb hub before attempting to connect peripherals. Even a wired usb keyboard and mouse can be too much for a pi zero, I've found. 

### Storage 

A server should have more memory available.  

32GB is good for a server installation.  

64GB is good to use for a camera installation. Should use external storage (e.g. network or usb), but it helps to have a local buffer. 

### Details

[troubleshooting](troubleshooting.md)  
[GPIO](./hardware/gpio.md)  


## Operating System

Common topics like flashing your SD media for OS installation

[OS Selection & Installation](./os/index.md)  

### Raspberry Pi OS

Raspberry Pi OS is a popular choice. 

Download an image from:
Raspberry Pi Guide - Quick Start Guide for Raspberry Pi
https://www.raspberrypi.org/downloads/

[Raspberry Pi OS Specific Notes](os/raspberry-pi-os.md)

### Ubuntu

Raspberry Pi specific notes about Ubuntu

[Ubuntu](os/ubuntu.md)

Once you've got your system install, it's time to configure what the system runs: [Ubuntu](../system/linux/ubuntu.md)

### Monitoring

```
wget -O /tmp/netdata-kickstart.sh https://my-netdata.io/kickstart.sh && sh /tmp/netdata-kickstart.sh
```

## Camera

[Camera Hardware](./hardware/camera.md)  

### OS native features

https://www.raspberrypi.com/documentation/computers/os.html#using-a-usb-webcam

### Motion

[Motion Eyes / Security](./camera/security.md)  


### Web cam setup

If you want your pi to be able to appear as a webcam device when plugged in via usb, try the following setup.

Showmewebcam looks like it is more up-to-date than geerlingguy's version:

https://github.com/showmewebcam/showmewebcam

As mentioned in

https://github.com/geerlingguy/pi-webcam/issues/4

Jeff Geerling's webcam setup

https://github.com/geerlingguy/pi-webcam  
GitHub - geerlingguy/pi-webcam: Automation to configure a Raspberry Pi as a USB OTG webcam  

https://gist.github.com/justinschuldt/36469e2a89d95ef158a8c4df091e9cb4  
Directions for setting up a RaspberryPi to act as a generic USB webcam · GitHub  
https://newatlas.com/telecommunications/jeff-geerling-raspberry-pi-hq-camera-webcam/  
Pi Zero W and HQ camera module used as cheap webcam  
https://github.com/geerlingguy  
geerlingguy (Jeff Geerling) · GitHub  

https://duckduckgo.com/?t=ffab&q=raspberry+pi+hq+camera+maximum+video+rate&ia=web  
raspberry pi hq camera maximum video rate at DuckDuckGo  
https://www.raspberrypi.org/documentation/raspbian/applications/camera.md  
Raspberry Pi Camera Module - Raspberry Pi Documentation  
https://duckduckgo.com/?t=canonical&q=use+raspberry+pi+hq+camera+as+web+camera&ia=web  
use raspberry pi hq camera as web camera at DuckDuckGo  



## Projects Using Pi

https://github.com/topics/raspberry-pi

### Home Assistant

https://github.com/home-assistant/core

https://github.com/CCOSTAN/Home-AssistantConfig

### Voice Recognition

https://github.com/topics/raspberry-pi  
raspberry-pi · GitHub Topics · GitHub  
https://github.com/MycroftAI/mycroft-core  
GitHub - MycroftAI/mycroft-core: Mycroft Core, the Mycroft Artificial Intelligence platform.  
https://github.com/Picovoice/porcupine  
GitHub - Picovoice/porcupine: On-device wake word detection powered by deep learning.  

### Wireless access point

https://github.com/RaspAP/raspap-webgui  
GitHub - RaspAP/raspap-webgui: Simple wireless AP setup & management for Debian-based devices  
https://www.wireguard.com/  
WireGuard: fast, modern, secure VPN tunnel  


### Time clock

aka a Digital Time Keeping System

https://city-of-bloomington.github.io/timetrack/  
TimeTrack Raspberry Pi | timetrack  

https://github.com/City-of-Bloomington/timetrack  
City-of-Bloomington/timetrack: Digital time keeping system with both a web-based calendar entry and a kiosk based badge entry interface  


### Media server

Run docker to allow multiple services on the same server.  

Docker image for plex somewhere? Best to just make your own so you know what is being installed.

### Spotify

https://github.com/dtcooper/raspotify  
GitHub - dtcooper/raspotify: Spotify Connect client for the Raspberry Pi that Just Works™  

