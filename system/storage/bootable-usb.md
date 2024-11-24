# Bootable USB

A bootable USB makes it easy to install a new system on a piece of hardware.

(see also: upgrade.txt, toward the end of the file?)

## rpi-imager

`rpi-imager` works with compressed `.gz` images. No need to decompress manually (unless you want to try running the image in a VM?).

Based on Etcher code base

https://www.raspberrypi.com/software/

```
cd Downloads/
sudo dpkg -i imager_1.8.5_amd64.deb 
sudo apt-get --fix-broken install
```

## Etcher

Etcher is a good tool for creating bootable USBs

Download. Run. 


## WoeUSB

Unless you need to create a bootable Windows USB via linux. Then your options are more limited

https://github.com/WoeUSB/WoeUSB

Get the dependencies

```
sudo apt install \
    bash \
    dosfstools \
    findutils \
    grep \
    gawk \
    grub-common \
    grub-pc-bin \
    ntfs-3g \
    p7zip-full \
    parted \
    util-linux \
    wget \
    wimtools
```

https://github.com/WoeUSB/WoeUSB/wiki/Dependencies

Then download via releases (It's a very tiny bash script)

https://github.com/WoeUSB/WoeUSB/releases

```
chmod +x woeusb-5.2.4.bash 
```

Then run with

```
woeusb-5 --device Windows7_x64.iso /dev/sdX
```


## Unetbootin

Rolls off the tongue! ;)

this is a good guide for creating a bootable USB image on xubuntu...

the normal usb-creator packages are not easily available on xubuntu

http://xubuntugeek.blogspot.com/2013/04/how-to-install-xubuntu-on-usb-device.html

sudo apt-get install unetbootin extlinux -y

Run UNetbootin using the command unetbootin or using the Applications Menu > System > UNetbootin launcher.


## Links

https://www.google.com/search?q=install+linux+to+usb
install linux to usb - Google Search

https://www.tecmint.com/install-linux-os-on-usb-drive/
How to Install Linux OS on USB Drive and Run it On Any PC
https://www.tecmint.com/linux-foundation-lfcs-lfce-certification-exam-book/
Ebook: Introducing the Linux Foundation's LFCS and LFCE Certification Preparation Guide


## Manually Format

Will Etcher make a USB key bootable when flashing? Doesn't seem like I've needed to reformat any keys manually in a while.

It is important to reformat the existing drive to make sure it is bootable!!
This is also a chance to to give it a label of which OS/flavor is on the device:

```
#CHANGE DEV LOCATION!
#check where your usb is mounted:
dmesg
mount

sudo ls #prime sudo

#this is different than disks, but important!
sudo parted -a optimal /dev/sdc mklabel msdos

sudo parted -a optimal /dev/sdc rm 1
sudo parted -a optimal /dev/sdc rm 2

#1MB is the start, 2GB is the end
#sudo parted -a optimal /dev/sdc mkpart primary fat32 1MB 2GB
sudo parted -a optimal /dev/sdc mkpart primary fat32 1MB 100%
sudo mkfs.vfat -n XUBUNTU /dev/sdc1

sudo parted -a optimal /dev/sdc set 1 boot on

#not sure if it's necessary to turn off other flags:
sudo parted -a optimal /dev/sdc set 1 esp off

sudo parted -a optimal /dev/sdc print

```
