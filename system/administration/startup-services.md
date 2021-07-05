# Startup Services

Often when thinking about running programs, we're used to launching them with a GUI. When running a computer as a server, we want the system to start up with everything running that needs to be running. This includes:

  - Attaching file systems (e.g. external storage devices, disks, drives)
  - Starting services (systemd)
  - Starting containers (docker)
  

## Attaching file systems

If you know there is a drive that is always going to be attached to a system, you can set up the server so that it automatically mounts the drive on startup. (Note: if the drive is not present, the startup process will stall.) Automatic drive mounting is done by adding an entry to the `/etc/fstab` file. You need administrator rights to do this. Open with your favorite editor

    sudo vi /etc/fstab
    
You'll need to know the device location (usually something like `/dev/sd*`)

    sudo dmesg | grep \\[sd
    
```    
/dev/sdb1       /media/account/My_Passport   ext4    defaults        0       0
```

Unlike automatic mounting, the mount point needs to exist: 

    mkdir -p /media/account/My_Passport

Once mounted, ntfs drives will show up as type `fuseblk`. However, the format for ntfs drives in `/etc/fstab` appears to be 

```
dev/sda2   /mnt/excess ntfs-3g    permissions,locale=en_US.utf8    0   2

```

https://askubuntu.com/questions/113733/how-to-mount-a-ntfs-partition-in-etc-fstab


    sudo mount -a
    

To replicate the GUI mount behavior from the CLI (maybe does not require root?): 
    
    udisksctl mount -b /dev/sdb2
    
Useful to see what settings should be used



## Starting services (systemd)

Here's the way I did it with a systemd service.

nano /etc/systemd/system/filebrowser.service

```
[Unit]
Description=Filebrowser
After=network-online.target

[Service]
User=account
Group=account

ExecStart=/usr/local/bin/filebrowser -r /

[Install]
WantedBy=multi-user.target
```

systemctl start filebrowser
systemctl enable filebrowser

P.S. this would be a dangerous setup if your filebrowser is accessible via the public internet. In my case it is only listening on the private network.

https://github.com/filebrowser/filebrowser/issues/453

systemctl status filebrowser.service



## Starting containers (docker)

In docker-compose.yml use:

    restart: always
    

