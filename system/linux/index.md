# Linux

[Upgrade](upgrade.md)  

[Set up Ubuntu](ubuntu.md)  

[Window Manager](window-managers/)  

[Workspaces](workspaces.md)  

[Backups](backup-system.md)  

[Keyboard](keyboard.md)


## What is Running

To see what distribution is currently running on your system:

```
lsb_release -a
```

or 

```
cat /etc/*-release
```

or 

```
hostnamectl
```

Kernel details are available with:

```
uname -a
```

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

### Servers

For servers, ubuntu server is good

This is a good local mirror for ubuntu: (don't think xubuntu is there)
http://ftp.ussg.iu.edu/linux

CentOS is another popular distribution that tracks Red Hat Enterprise Linux

## Raspberry Pi

[Raspberry Pi](/pi/)  


## Create Bootable Media

This process is the same for all systems.

Whatever you choose, start by downloading and burning an image of the operating system (OS) of your choice from their site.

To transfer image for use, see [creating a bootable usb](../storage/bootable-usb.md).



## Other Distributions

[Android](../android/)  
[tmux](../terminal/tmux.md)  

There are many linux distributions available. Which one you run is a matter of personal preference. Communities create a distribution as a standard collection of linux applications and configurations. 
[Notes about what happens before the Operating System (OS) is running](../startup.md)

When downloading an image, torrents are a good way to share bandwidth. This supports a good cause! Be sure to use a network that you have adequate bandwidth quotas.


[Distributions](distributions.md)
