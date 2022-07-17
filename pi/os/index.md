# Pi Operating Systems

Choose the right foundation for the type of system you deploy. 

The Raspberry Pi Imager helps you choose and flash your media:

```
sudo apt install rpi-imager
rpi-imager
```

https://www.raspberrypi.com/software/


## Flash the image

Alternative ways to flash the image if the above `rpi-imager` does not suit your needs

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

https://raspberrytips.com/best-os-for-raspberry-pi/


Guide for the timetrack project that covers many similar 'getting started' topics

https://city-of-bloomington.github.io/timetrack/

https://github.com/City-of-Bloomington/timetrack

