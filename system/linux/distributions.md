# Linux Distributions

To determine the distribution you're running see [Linux notes](index.md)

If looking beyond Debian / Ubuntu

## Elementary OS

Based on Ubuntu? Debian? Gnome 3? 
This is one I've been interested to try out. 

https://elementary.io/  
The thoughtful, capable, and ethical replacement for Windows and macOS ⋅ elementary OS  
https://elementary.io/docs/installation#installation  
Installation  
https://github.com/elementary/os/wiki/elementary-OS-6.1-J%C3%B3lnir-FAQ  
elementary OS 6.1 Jólnir FAQ · elementary/os Wiki  
https://store.elementary.io/  
Store ⋅ elementary Store  
https://duckduckgo.com/?t=ffab&q=elementary+os+ubuntu+22.04&ia=web  
elementary os ubuntu 22.04 at DuckDuckGo  
https://9to5linux.com/elementary-os-7-will-be-based-on-ubuntu-22-04-lts-offer-gtk4-apps-and-power-profiles  
elementary OS 7 Will Be Based on Ubuntu 22.04 LTS, Offer GTK4 Apps and Power Profiles - 9to5Linux  

## Nix OS

https://nixos.org/  
Nix: reproducible builds and deployments  
https://en.wikipedia.org/wiki/NixOS  
NixOS - Wikipedia  
https://duckduckgo.com/?q=nix+os&hps=1&atb=v343-5ja&ia=web  
nix os at DuckDuckGo  


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

