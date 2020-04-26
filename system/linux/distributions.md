# Distributions

[for notes about what happens before an OS is running, see overview.md](overview.md)

There are many linux distributions available.

When downloading an image, torrents are a good way to share bandwidth. This supports a good cause! Be sure to use a network that you have adequate bandwidth quotas.

To transfer image for use, see [creating a bootable usb](../drives/bootable_usb.txt).

## Create Bootable Media

This process is the same for all systems.


## Desktop

For a desktop I've been leaning toward xubuntu (yay XFCE!)

Ubuntu Studio is built on top of Xubuntu.

See also ubuntu-studio.txt

https://ubuntustudio.org/
Ubuntu Studio

### Ubuntu

Ubuntu is an open source linux operating system.  

http://www.ubuntu.com

Whatever you choose, start by downloading and burning an image of the operating system (OS) of your choice from their site.

Once you have your copy of Ubuntu, boot your computer with it and Ubuntu should load into a "Live" instance.  This is a fully functional Linux system.  Cooool.  With enough memory, the only noticable performance hit comes when the system needs to access a program from the source media.  

At this point you are also able to install Ubuntu to a local drive. Whether you run live or install to disk, that's up to you.

For more details on installing a new Ubuntu system, see:
[linux/ubuntu.md](linux/ubuntu.md)


## Servers

For servers, ubuntu server is good

This is a good local mirror for ubuntu: (don't think xubuntu is there)
http://ftp.ussg.iu.edu/linux

CentOS is another popular distribution that tracks Red Hat Enterprise Linux


## Raspberry Pi

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

Via:
https://unix.stackexchange.com/questions/272701/liveusb-stuck-after-probing-edd-during-boot

If the screen appears locked with a clock, slide the screen up to unlock it.

To create a disk image from an img file:

    system/virtualization/img_files.md

Finally, needed to do the following to boot:

In order overcome the above error and proceed with booting TAILS from an older SanDisk Cruzer stick, hit the <TAB> key at the boot prompt. You’ll see, at the foot of the screen, a series of default switches passed to the boot loader. Arrow key back to the media=removable option and delete it. Ensure you keep the space between successive parameters. Hit <RETURN> and TAILS should continue booting per usual. It can be a little slow to boot, particularly from older USB sticks, so be patient.

via:  
http://www.outofworkpoet.com/initramfs-unable-find-live-medium-containing-live-file-system-booting-tails


## Custom Distribution
http://www.linuxfromscratch.org/
Welcome to Linux From Scratch!

https://www.google.com/search?q=build+custom+linux+distribution&ie=utf-8&oe=utf-8&client=firefox-b-1-ab
build custom linux distribution - Google Search
https://www.maketecheasier.com/create-linux-distro/
Create Your Own Linux Distro with Ubuntu Imager
https://github.com/Distroshare/distroshare-ubuntu-imager
GitHub - Distroshare/distroshare-ubuntu-imager: Creates an installable live CD from an installed Ubuntu or derivative distribution
https://help.ubuntu.com/community/MakeALiveCD/DVD/BootableFlashFromHarddiskInstall
MakeALiveCD/DVD/BootableFlashFromHarddiskInstall - Community Help Wiki

