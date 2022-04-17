# Upgrades

Upgrade can mean different things in different contexts. 

If you just want to apply the latest versions of packages to your system, `apt` will call this `upgrade`

    sudo apt-get update
    sudo apt-get upgrade

## /boot 

When `sudo apt-get upgrade` includes a kernel update, sometimes `/boot` runs out of room

```
sudo dpkg --list 'linux-image*' | grep ^ii
sudo apt-get remove linux-image-VERSION
sudo apt-get autoremove

#may not be necessary, but harmless
#sudo update-grub
```

via:
http://askubuntu.com/questions/345588/what-is-the-safest-way-to-clean-up-boot-partition

e.g.:
sudo apt-get remove linux-image-4.2.0-16-generic

## Repair /boot partition

Occasionally, cleaning up old kernels can result in a system that won't boot. In that case, boot into a live instance of the OS to repair the system.

https://linuxconfig.org/ubuntu-boot-repair

Tried boot-repair utility

It helped in identifying encrypted partitions to mount


## Stale repository

Sometimes a source repository for apt changes location. This situation can cause "Software Updater" to report scary messages like "Could not retrieve updates at this time..."

To remove the offending repository, run `sudo apt-get update` from the command line to see which one fails:

```
Err:9 http://ppa.launchpad.net/canonical-chromium-builds/stage/ubuntu focal Release
  404  Not Found [IP: 91.189.95.85 80]
```

Then, to remove it:

    sudo add-apt-repository -r http://ppa.launchpad.net/canonical-chromium-builds/stage/ubuntu

[via](https://askubuntu.com/questions/717144/remove-source-from-software-updater)



## Creating a bootable USB

will need to do this before starting fresh.  On Ubuntu, there is usb-creator built in...
you can use that on a standard Ubuntu machine, but XUbuntu does not run on Gnome, so usb-creator is not available for it.

or use this utility:
http://unetbootin.sourceforge.net/

sudo apt-get install unetbootin

Or you can make the USB manually:
http://www.pendrivelinux.com/usb-xubuntu-804-persistent-install-from-linux/


(see also bootable_usb.txt)

****
make sure any data stored in browser extensions (link router) is also backed up
****

****
make sure to synchronize /c 
(especially if not restoring original directory structure)
****

