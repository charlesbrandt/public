# Linux

There are many linux distributions available. Which one you run is a matter of personal preference. Communities create a distribution as a standard collection of linux applications and configurations. 

[Notes about what happens before the Operating System (OS) is running](../startup.md)

When downloading an image, torrents are a good way to share bandwidth. This supports a good cause! Be sure to use a network that you have adequate bandwidth quotas.

[Search](search.md)  

[Upgrade](upgrade.md)  
[Window Manager](window-managers/)  

[Workspaces](workspaces.md)  

[Backups](backup-system.md)  

## Create Bootable Media

This process is the same for all systems.

Whatever you choose, start by downloading and burning an image of the operating system (OS) of your choice from their site.

To transfer image for use, see [creating a bootable usb](../drives/bootable-usb.md).

## Distributions

## Ubuntu

[Ubuntu](ubuntu.md)  
[Ubuntu Server](ubuntu-server.md)  
[Ubuntu Version](ubuntu-version.md)  

Ubuntu is an open source linux operating system.

http://www.ubuntu.com

Once you have your copy of Ubuntu, boot your computer with it and Ubuntu should load into a "Live" instance.  This is a fully functional Linux system.  Cooool.  With enough memory, the only noticable performance hit comes when the system needs to access a program from the source media.

At this point you are also able to install Ubuntu to a local drive. Whether you run live or install to disk, that's up to you.

[more details on installing a new Ubuntu system](ubuntu.md)

### [Desktops / Window Managers](window-managers/)

As if it wasn't enough to choose a distribution, you may also want to choose the [window manager](window-managers/). Typically this choice is made by the specific distribution you run, but some (e.g. arch) allow you to choose as part of the installation process. 

Gnome3 in newer versions of Ubuntu is a solid choice. I enjoy the 'overview' feature where you can see all windows for a given workspace. 

Xubuntu ships with XFCE, which is another good choice. 

Ubuntu Studio is built on top of Xubuntu.
https://ubuntustudio.org/


## Servers

For servers, ubuntu server is good

This is a good local mirror for ubuntu: (don't think xubuntu is there)
http://ftp.ussg.iu.edu/linux

CentOS is another popular distribution that tracks Red Hat Enterprise Linux


## Raspberry Pi

[Raspberry Pi](raspberry-pi.md)  

[See also]((/pi/)  

Hard to beat raspbian. Helps make sure you're starting from common ground when documenting your projects: 

https://www.raspberrypi.org/downloads/raspbian/

If you plan to use a Pi as a daily driver / desktop, you may want to customize the desktop / distribution:

https://ubuntu-pi-flavour-maker.org/download/


## Arch Linux

https://www.google.com/search?q=arch+linux
arch linux - Google Search
https://www.archlinux.org/download/
Arch Linux - Downloads


## Security

### Qubes OS

https://www.google.com/search?q=qube+os
qube os - Google Search
https://www.qubes-os.org/intro/
An Introduction to Qubes OS | Qubes OS
https://www.qubes-os.org/doc/
Documentation | Qubes OS
https://www.qubes-os.org/downloads/
Download Qubes OS | Qubes OS

### Kali Linux

https://www.kali.org/downloads/  
Official Kali Linux Downloads | Kali Linux  
https://docs.kali.org/category/introduction  
01. Getting Started | Kali Linux  
https://docs.kali.org/introduction/should-i-use-kali-linux  
Should I Use Kali Linux? | Kali Linux  

https://www.google.com/search?q=secure+linux&ie=utf-8&oe=utf-8&client=firefox-b-1-ab  
secure linux - Google Search  
https://fossbytes.com/secure-linux-distros-privacy-anonymity/  
10 Most Secure Linux Distros For Complete Privacy & Anonymity | 2017 Edition  


## Privacy

https://boingboing.net/2018/01/30/happy-data-privacy-day-a-turn.html  
Happy Data Privacy Day! A turning point for anonymity, privacy, and the tools that deliver them / Boing Boing  

### Whonix

### Tails

https://tails.boum.org/install/download/index.en.html  
Tails - Download and verify  
https://tails.boum.org/index.en.html  
Tails - Privacy for anyone anywhere

To get this to boot with a VM in VirtualBox, I needed to run the following:

    Click settings before starting the VM
    Goto to System
    Look at Extended Features
    Click "Enable I/O APIC"

[via](https://unix.stackexchange.com/questions/272701/liveusb-stuck-after-probing-edd-during-boot)

If the screen appears locked with a clock, slide the screen up to unlock it.

To create a disk image from an img file:

    system/virtualization/img_files.md

Finally, needed to do the following to boot:

In order overcome the above error and proceed with booting TAILS from an older SanDisk Cruzer stick, hit the `<TAB>` key at the boot prompt. You’ll see, at the foot of the screen, a series of default switches passed to the boot loader. Arrow key back to the media=removable option and delete it. Ensure you keep the space between successive parameters. Hit `<RETURN>` and TAILS should continue booting per usual. It can be a little slow to boot, particularly from older USB sticks, so be patient.

[via](http://www.outofworkpoet.com/initramfs-unable-find-live-medium-containing-live-file-system-booting-tails)


## Custom Distribution
http://www.linuxfromscratch.org/  
Welcome to Linux From Scratch!  

https://www.google.com/search?q=build+custom+linux+distribution
build custom linux distribution - Google Search
https://www.maketecheasier.com/create-linux-distro/  
Create Your Own Linux Distro with Ubuntu Imager  
https://github.com/Distroshare/distroshare-ubuntu-imager  
GitHub - Distroshare/distroshare-ubuntu-imager: Creates an installable live CD from an installed Ubuntu or derivative distribution  
https://help.ubuntu.com/community/MakeALiveCD/DVD/BootableFlashFromHarddiskInstall  
MakeALiveCD/DVD/BootableFlashFromHarddiskInstall - Community Help Wiki  

