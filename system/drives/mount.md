# Mount Drives

## Permanent 

In Linux, the file `/etc/fstab` is used to mount drives automatically when the system starts up. 
 
```
/dev/sda1       /media/account/system   ext4    defaults        0       1
/dev/sdb2      /media/account/Seagate4TB   exfat        defaults,uid=1000,gid=1000        0       2
/dev/sdc1      /media/account/My_Passport       ntfs     defaults,uid=1000,gid=1000        0       2
```

For some filesystems, like `exfat`, it's necessary to pass additional parameters so that access permissions are set appropriately:

```
/dev/sdb1 /media/workspace auto defaults,uid=1000,gid=1000,umask=022 0 1
```

To mount everything in `/etc/fstab`, run

```
sudo mount -a
```

## Temporary

This will not come back after a reboot. Not that the `mountpoint` needs to already exist. 

```
sudo mount /dev/sda1 ~/mountpoint
```

To cleanly unmount the drive:

```
sudo umount ~/mountpoint
```

## Links

https://unix.stackexchange.com/questions/204641/automatically-mount-a-drive-using-etc-fstab-and-limiting-access-to-all-users-o  
permissions - Automatically mount a drive using /etc/fstab, and limiting access to all users of a specific group - Unix & Linux Stack Exchange  
  
https://duckduckgo.com/?t=ffab&q=linux+fstab+mount+as+user&ia=web  
linux fstab mount as user at DuckDuckGo  


