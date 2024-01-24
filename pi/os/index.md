# Pi Operating Systems

Choose the right foundation for the type of system you deploy. 

The Raspberry Pi Imager helps you choose and flash your media:

```
sudo apt install rpi-imager
rpi-imager
```

https://www.raspberrypi.com/software/

## Backing up

Before you re-use an SD card for a new device, be sure to back them up. 
Make an image. 

This looks like a good guide that includes resizing the partition:

https://www.tomshardware.com/how-to/back-up-raspberry-pi-as-disk-image

```
sudo apt-get install gparted -y
```

Go ahead and get QDirStat to help determine space utilization

```
sudo apt-get install qdirstat
```


Plug in card via card reader. Identify the device path

```
lsblk
```

On linux, `dd` is a good option to copy raw data that's usually available.


```
sudo fdisk -l
sudo dd bs=4M if=/dev/sde of=~/MyImage.img
```

If you shrank the partition to enable only dd'ing the used space, specify the size to dd. In this case, I had 9GB of data on the card:

```
sudo dd if=/dev/sda of=~/2024-pi-64bit-os-plex-calibre-web-sd.img bs=1M count=9000
```

### Shrinking

Install a shrink script

```
wget https://raw.githubusercontent.com/Drewsif/PiShrink/master/pishrink.sh
sudo chmod +x pishrink.sh
sudo mv pishrink.sh /usr/local/bin
```

Then

```
sudo pishrink.sh -z myimg.img
```

> Note: if you get an error like the following, ensure that you copied enough data in the previous dd command:

```
sudo pishrink.sh -z 2021-plex-pi-32bit-os-server-sd.img 
pishrink.sh v0.1.4
pishrink.sh: Gathering data ...
Error: Can't have a partition outside the disk!
pishrink.sh: ERROR occurred in line 288: parted failed with rc 1
``` 

### Verify in VM

Try running an image as a virtual machine.  Alternatively, mount it as a file system and extract necessary information from the device. 

https://linuxconfig.org/how-to-run-the-raspberry-pi-os-in-a-virtual-machine-with-qemu-and-kvm

```
sudo apt-get update && sudo apt-get install qemu-system-arm qemu-kvm libvirt-clients libvirt-daemon-system bridge-utils virtinst libvirt-daemon virt-manager
```

Get qemu-ready kernels

```
git clone https://github.com/dhruvvyas90/qemu-rpi-kernel
```


Ensure default network is started

```
sudo virsh --connect=qemu:///system net-start default
```

Launch the image

```
sudo virt-install \
  --name rpios  \
  --arch armv6l \
  --machine versatilepb \
  --cpu arm1176 \
  --vcpus 1 \
  --memory 256 \
  --import  \
  --osinfo debian12 \
  --disk 2024-pi-64bit-os-plex-calibre-web-sd.img,format=raw,bus=virtio \
  --network bridge,source=virbr0,model=virtio  \
  --video vga  \
  --graphics spice \
  --boot 'dtb=qemu-rpi-kernel/versatile-pb-buster.dtb,kernel=qemu-rpi-kernel/kernel-qemu-4.19.50-buster,kernel_args=root=/dev/vda2 panic=1' \
  --events on_reboot=destroy
```

use `virt-manager` to control which VMs are running (or restart stalled ones)

It can be tricky to determine which `qemu-rpi-kernel` to use. For example, debian12 / bookworm kernels are not currently available (as of 2024.01.20)

https://github.com/dhruvvyas90/qemu-rpi-kernel/issues/155


### Images

`rpi-imager` works with compressed `.gz` images. No need to decompress manually (unless you want to try running the image in a VM?).

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

## Flash the image alternatives

Alternative ways to flash the image if the above `rpi-imager` does not suit your needs

### Balena Etcher

Download and run Etcher:

https://etcher.io/

This has been adopted by Rasperry Pi and rebranded and expanded. At least the overall process feels the same.

Some good interesting projects shown during write phase.

https://www.balena.io/etcher/  
balenaEtcher - Flash OS images to SD cards & USB drives  

