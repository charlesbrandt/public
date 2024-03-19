# System

Underlying systems change over time. These notes track the tools and configurations that help use systems more efficiently.

## Applications 

[Browsers](browsers.md)  

[Editors](editors/)  

[Applications](applications.md)  

[Password Managers](password-manager.md)  

## Search

To find files that match a pattern from the command line

```
fd -E repos search-term
```

`fd` has easier syntax compared to the default `find`:

```
find * -iname "*search-term*"
```

[More about Search](search.md)

## Terminals

Other keywords include: bash, shell

[Terminal](terminal/index.md)  

https://tmuxcheatsheet.com/  
[Tmux](terminal/tmux.md)  

[SSH](terminal/ssh.md)  

Bash Colors example available in <a href="terminal/bash-colors.sh">terminal/bash-colors.sh</a>

<a href=".bashrc">Sample .bashrc file in the repository</a>


### Moving Files

`mv` command seems so basic, so straightforward. 

Reminder: `mv -b` is useful for creating backups of any files that exist in the destination directory. If merging two big directories with some overlapping files, this is a useful option to be able to compare once everything is in the same place. 

https://linuxize.com/post/how-to-move-files-in-linux-with-mv-command/

## Virtualization

[Virtualization](virtualization/)  

## Administration

[Checksums](checksums.md)  
[Firewall](firewall.md)  
[Network](network.md)  
[Nginx](nginx.md)  
[Remote Desktop](remote-desktop.md)  
[Startup Services](startup-services.md)  

[Crontab (Scheduled Jobs)](crontab.md)  

[Disk Usage](storage/disk-usage.md)  

## Hardware

A computer is hardware that runs software. These devices power the systems and applications we've come to rely on. 

If you're just getting familiar with learning about computers, the [Raspberry Pi](/pi/) ecosystem is a great place to start. Heavy overlap and an inexpensive entry point. Learning is part of the process.

Computers are defined by three general properties; how much: computational processing power (CPU), memory, and storage. 

### Storage

Memory and storage may seem like the same, but for now memory is a fast storage system that only exists while power is supplied. Storage is slightly slower for reading and writing, but the information persists after power is removed. 

[Hard Drives / Storage](storage/)   


## Operating Systems

Set up a computer with a fresh operating system.

Linux is common on desktop machines in the open source world. There are many flavors:

[Linux](linux/) ([Upgrade](linux/upgrade.md))  

[Set up Ubuntu](linux/ubuntu.md)  

In the mobile phone space, Android is a good open source solution.

[Android](android/)  

If you don't already have a preferred operating system, consider one that is [open source.](https://opensource.guide)

Windows and Mac OS X are good options too. 

### Boot

The boot sequence gets the system started so it is ready to run software. 

[Startup](startup.md)  


