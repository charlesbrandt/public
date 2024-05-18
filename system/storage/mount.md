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

### External drives

Predictably mount usb drive linux fstab

When the storage is connected externally (e.g. USB), it's possible that the device order or id may change. If not properly configured, this may cause the machine to not finish booting. If the machine is remote or a server, this situation can be difficult to troubleshoot. 

To skip a missing drive at boot, add `nofail` to the options in fstab. It needs to come after `auto`.

https://askubuntu.com/questions/14365/mount-an-external-drive-at-boot-time-only-if-it-is-plugged-in

Using the drive's UUID as the source parameter prevents any issues if the device gets assigned a different device path in `/dev/`. How to find the UUID of a drive linux?

```
sudo blkid /dev/sd*
```

https://unix.stackexchange.com/questions/658/linux-how-can-i-view-all-uuids-for-all-available-disks-on-my-system


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


