# Swap

Swap space comes up in a number of other contexts. Notes on the topic can get fragmented. 

Generally speaking, modern systems with large amounts of RAM should not need a swap partition. If you exceed the amount of system memory in a high RAM system, close the number of open applications. 

Systems like Kubernetes go so far as to require that swap spaces be disabled. They can cause problems in cluster applications. 

## File based swap space

- Login as root.

```
sudo -i
```

- Create swap file in directory “/var” with name “swapfile”.  At the shell, create the file and set root permissions as follows:

```
cd /var
touch swapfile
chmod 600 swapfile
ls -la swapfile
```

- Use “dd” command to fill the swap file with 65 GB size (as an example) as follows :

```
dd if=/dev/zero of=/var/swapfile bs=1024k count=65000
```

- Now setup the swap file:

```
mkswap /var/swapfile
```

- Enable the swap file:

```
swapon /var/swapfile
```

To check whether the new swap file was successfully created, look in `System monitor` or try running:

```
cat /proc/swaps
swapon --show 
```

To disable swap

```
swapoff /var/swapfile 
```

Optionally, you can always remove the swap file too. 



via [How To Create Or Increase Swap Space In Linux](https://www.linuxandubuntu.com/home/how-to-create-or-increase-swap-space-in-linux)

No need to back up `/var/swapfile` (Manually exclude it from any backup processes you run)

## Hibernation

Hibernation is one example when swap space is useful. On some systems, it is possible to change the amount of system memory. In some cases, it is good to disable swap completely. 

For those reasons, I prefer to create file based swap space on demand. This is instead of creating a dedicated swap partition when setting up the drive layout at installation time. 

For hibernation, the swap file needs to be mounted automatically at startup:

```
echo '/var/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
```

### Find the UUID & Offset:

Since the file is created on Ubuntu file-system. It can be located via root UUID as well as physical offset.

To see your Ubuntu partition, run command and find the one mounted on `/`:

```
df -h
```

Then find its UUID via command:

```
blkid
```

c14da95a-1c79-45b9-8930-c938a71c1e97 1775616

To find the physical offset for /swapfile, run command:

```
sudo filefrag -v /var/swapfile
```

Edit Grub configuration file to use the new resume parameters

```
sudo vi /etc/default/grub
```


On Ubuntu:

```
systemctl hibernate
```




For other systems, hibernation is provided as part of `pm-utils`

```
sudo apt-get install pm-utils
```


Once your swap space is enabled, you can try running the following (after making sure any active changes have been saved... if it doesn't work you may need to load the system from scratch again)

```
sudo pm-hibernate
```

If that doesn't do anything, check logs:

```
head /var/log/pm-suspend.log
```


## Memory cleanup strategies

Browsers can be a big consumer of system resources if left unchecked and unmonitored. 

## Links

If you need swap, just use a swap-file

https://duckduckgo.com/?t=ffab&q=do+i+need+a+swap+partition+linux&ia=web  
do i need a swap partition linux at DuckDuckGo  
https://askubuntu.com/questions/1117947/do-i-still-need-to-create-a-swap-partition-in-modern-linux  
system installation - Do I still need to create a SWAP partition in modern Linux? - Ask Ubuntu  
https://askubuntu.com/questions/1059108/swap-on-ssd-partition-or-file  
partitioning - Swap on SSD: partition or file? - Ask Ubuntu  
