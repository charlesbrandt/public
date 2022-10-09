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


## Backup existing system

Move out media first. Use Disk Analyzer to locate it.
e.g.

```
mv ~/Pictures /media/account/path/to/external/drive
```

Ideally, get rid of any Spotify cached files. These are encrypted and only take up space. 

```
rm -R ~/snap/spotify
```

or

```
rm -R ~/.config/spotify
```

When backing things up to an external drive, it's better to make a tar file of everything so that permissions are preserved correctly (otherwise scripts get reset as not executable... a hassle) and you don't end up with a large number of inodes getting used on the destination drive.

Close everything first? (e.g. browsers)

```
tar -zcvf /media/account/externaldrive/ ~/
```

```
mkdir -p /media/account/T7/out-system/
sudo tar -zcvf /media/account/T7/out-system/20221008-ubuntu-20.04-home-directory.tgz ~/
sudo chown account: /media/account/T7/out-system/20221003-ubuntu-20.04-home-directory.tgz
```

Ideally get the system too, especially if wiping an existing system clean. This helps if there are cron configurations, git repos, etc

```
sudo tar -zcvf /media/account/T7/out-system/20221003-ubuntu-20.04-system.tgz --exclude=/media --exclude=/home/account /
sudo chown account: /media/account/T7/out-system/20221003-ubuntu-20.04-system.tgz
```

Ideally don't make changes to anything if this is a live system. 


## Creating a bootable USB

Download the latest version of the [image you want to install](index.md):

https://ubuntu.com/download/desktop

### Etcher

Balena Etcher is an easy to use utility that works the same on most OSes:

https://www.balena.io/etcher/

### usb-creator

On Ubuntu, `usb-creator-gtk` is a built in.  
XUbuntu does not run on Gnome, so `usb-creator` is not available for it.  

### unetbootin

http://unetbootin.sourceforge.net/  

```
sudo apt-get install unetbootin
```

### Manual

You can make the USB manually:  
http://www.pendrivelinux.com/usb-xubuntu-804-persistent-install-from-linux/  


(see also bootable_usb.txt)


## Setting up a new partition layout for a new system

See also [partitions](../drives/partitions.md) or general [drive](../drives/) notes

I'm always tempted to let the OS decide what scheme to use

Especially for advanced features like an encrypted drive -- sometimes it's challenging to choose the right configuration.

However, it's worth making a few good intentional choices now. 

Some things I like to change

  - EFI partition  
    I've been going with 512MB here -- has been sufficient so far

  - A bigger /boot partition  
    I know it's important to clean out old kernel images  
    But I prefer to have some room to grow here  
    Giving 4GB a try. Should be better than the default 768MB.   
    Can increase more if problems still crop up.   
    
  - Encrypted filesystem  
    for portable machines this is more important than for desktops or data drives.   
    Create a new partition  
    Use the rest of the space  
    Choose "physical volume for encryption"  
    You'll be prompted to enter your encryption key
    

Future considerations

  - If hibernation is actually necessary, just use a file based scratch drive. Easier to allocate, and prevents storage from getting allocated in a way that is sub optimal. 
  - [previously] Enough swap to enable hibernation  
    Will want to disable swap from being used by the OS. (Just use memory)  
    But it is useful to have enough space for hibernation  
    ideally it is not part of the / (root) filesystem  

    

  - Separate `/var` ???  
    I like this idea in theory -- prevent an ailing service that generates a lot of log files from filling up the main partition.   
    `/var/lib/docker` is also in here -- that can be pretty big too  
    The trick is knowing how much space is enough to allocate without tying up space that doesn't get used. 
