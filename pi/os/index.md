# Pi Operating Systems

Choose the right foundation for the type of system you deploy. 

## Ubuntu

https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#2-prepare-the-sd-card

https://ubuntu.com/download/raspberry-pi  
Install Ubuntu on a Raspberry Pi | Ubuntu  

If you're not installing a pre-made image configured for a specific application (e.g. Plex, Home Assistant, Motion Eye, etc...), this is a good foundation to start with. Can add a gui later if you're not sure if it's for desktop or server use. 

A 64bit OS is required to use more than 4GB of memory. If you have a Pi with 8GB of memory, find a 64 bit OS. 

Confirmed it works with Rasberry Pi 4 (8GB)

Must use 64bit OS for 8GB of memory version...

If running 32bit os, don't buy anything bigger than 4GB of memory.

64 bit OS does not work with Raspberry Pi Zero W.

32 Bit ubuntu (20.04) did not boot raspberry pi zero w

seems to just hang. 



## Raspberry Pi OS

Use for raspberry pi zero W

Maybe 32 Bit Ubuntu does not work with Raspberry Pi Zero W? 

In that case, 32 bit Raspberry Pi OS is the best choice?

may be good for pis intended for device service (e.g. camera

Can use a spare raspberry pi 4 (4GB memory) to run the install and set things up more efficiently)
(probably good enough for most server purposes)

<< --- install / setup on pi zero is slooooow


## Images / Flashing

https://www.raspberrypi.org/software/

sudo apt install rpi-imager

run with `rpi-imager`

decide on a base OS


### Balena

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
sudo dd bs=4M if=/dev/sde of=/home/account/MyImage.img
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
