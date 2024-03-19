# Format Drive

Nice overview of the different options

https://www.howtogeek.com/howto/33552/htg-explains-which-linux-file-system-should-you-choose/

## Label

label the drive externally  
post-it notes work well  

things to include:
 - when it was first initialized  
 - what file system  
 - capacity (in case it's not clear from the drive)  

get creative if you want!


## Identify the device on the system

see what device path gets assigned after mount:

```
dmesg
```

## Existing device format information

To find out what filesystem a device is formatted for, use:

```
sudo file -s /dev/sdd1
/dev/sdd1: DOS/MBR boot sector, code offset 0x52+2, OEM-ID "NTFS    ", sectors/cluster 8, Media descriptor 0xf8, sectors/track 32, heads 64, hidden sectors 2048, dos < 4.0 BootSector (0x80), FAT (1Y bit by descriptor); NTFS, sectors/track 32, sectors 30494719, $MFT start cluster 4, $MFTMirror start cluster 1905919, bytes/RecordSegment 2^(-1*246), clusters/index block 1, serial number 02167d0f6232476c5; contains Microsoft Windows XP/VISTA bootloader BOOTMGR
```

This yields more information than mount will (fuseblk is not specific enough)

An alternative to determine current file system format:

```
sudo fdisk /dev/sdb
```

FAT32 == /dev/sdb1 on /media/charles/8698-D9F8 type vfat

Device     Boot Start       End   Sectors   Size Id Type
/dev/sdb1          32 240254975 240254944 114.6G  c W95 FAT32 (LBA)

## Partitions

If the drive is not arranged the way you want it to be, for example, a new drive, you may need to create the partition table on your own. Many operating system installers will do this step for you, but there are cases where you may want to customize it manually. 

### gdisk

https://matthew.komputerwiz.net/2015/12/13/formatting-universal-drive.html

```
sudo gdisk /dev/sdb
```

First, create a new partition table

```
Command (? for help): o
This option deletes all partitions and creates a new protective MBR.
Proceed? (Y/N): Y
```

Now create a partition. The defaults will create a new partition that spans the whole drive with the first sector already aligned. Be sure to choose the correct type 0700!

```
Command (? for help): n
Partition number (1-128, default 1):
First sector (34-16326462, default = 2048) or {+-}size{KMGTP}:
Last sector (2048-16326462, default = 16326462) or {+-}size{KMGTP}:
Current type is 8300 (Linux filesystem)
Hex code or GUID (L to show codes, Enter = 8300): 0700
Changed type of partition to 'Microsoft basic data'
Write the changes to the drive and exit

Command (? for help): w

Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING
PARTITIONS!!

Do you want to proceed? (Y/N): Y
OK; writing new GUID partition table (GPT) to /dev/sdX.
Warning: The kernel is still using the old partition table.
The new table will be used at the next reboot.
The operation has completed successfully.
```



## Formats to use

### exfat

If a device needs to be shared with non-linux operating systems (Windows, Mac, etc)

Use exfat

This includes data repository type drives


```
sudo apt install exfat-fuse exfatprogs
```

format the partition with the exFAT filesystem

```
sudo mkfs.exfat -n HOME /dev/sdX1
mkexfatfs 1.0.1
Creating... done.
Flushing... done.
File system created successfully.
```

sudo mkfs.exfat -n Storage /dev/sdb1

(android still doesn't like this as of 2019.07.17 09:43:58, pixel3. Just using android to reformat the flash drive yields: vfat (fat32))

### ext4

If the device needs to run system level services, e.g. operating systems, webservers, docker containers, 

Use ext4

If going this route, it may be a good idea to include a swap partition too!

```
sudo mkfs.ext4 -L DATA /dev/sde1
```

Permissions will work on linux with this format, so go ahead and grant some sane ones:

```
sudo chown -R account: .
```

More info is kept up-to-date in format.md.
However, can also see:

[File systems](file-systems.md)



## Wipe / Erase

To wipe:

```
sudo dd if=/dev/urandom of=/dev/sdb bs=1M
```

http://www.howtogeek.com/howto/15037/use-an-ubuntu-live-cd-to-securely-wipe-your-pcs-hard-drive/

```
sudo apt-get install wipe
```

make sure to get the right drive:

```
sudo fdisk -l
mount

sudo wipe /dev/sdb2
sudo wipe /dev/sdb1
```


## Disk Alignment

if you get the message:

Warning: The resulting partition is not properly aligned for best performance.

this is the easiest way:
sudo parted -a optimal /dev/sdc mkpart primary NTFS 0% 100%

via https://www.pantz.org/software/parted/parted_and_disk_alignment.html


This didn't work:

http://rainbow.chard.org/2013/01/30/how-to-align-partitions-for-best-performance-using-parted/


    Get the alignment parameters for your array (remember to replace sdb with the name of your device as seen by the kernel).

    # cat /sys/block/sdc/queue/optimal_io_size
    1048576
    # cat /sys/block/sdc/queue/minimum_io_size
    262144
    # cat /sys/block/sdc/alignment_offset
    0
    # cat /sys/block/sdc/queue/physical_block_size
    512

(33553920 + 0) / 4096

    Add optimal_io_size to alignment_offset and divide the result by physical_block_size. In my case this was (1048576 + 0) / 512 = 2048.
    This number is the sector at which the partition should start. Your new parted command should look like

    mkpart primary 2048s 100%

    The trailing ‘s’ is important: it tells parted that you’re talking about sectors, not bytes or megabytes.
    If all went well, the partition will have been created with no warnings. You can check the alignment thusly (replacing ‘1’ with the partition number if necessary):

    (parted) align-check optimal 1
    1 aligned



## Swap space

Generally, file based swap partitions are sufficient and more flexible than a dedicated partition on a device. 

See also [swap](swap.md)

If you're manually partitioning a system drive, you can create a swap partition. 
In programs like fdisk / gdisk, change the type (press `t`) to value 82 
That will designate it as swap space.

https://www.howtogeek.com/196238/how-big-should-your-page-file-or-swap-partition-be/

```
Command (? for help): n       
Partition number (1-128, default 1): 2
First sector (34-3907029134, default = 2048) or {+-}size{KMGTP}: 
Last sector (2048-3907029134, default = 3907029134) or {+-}size{KMGTP}: +32GB
Current type is 8300 (Linux filesystem)
Hex code or GUID (L to show codes, Enter = 8300): 8200
Changed type of partition to 'Linux swap'

Command (? for help): n
Partition number (1-128, default 1): 
First sector (34-3907029134, default = 67110912) or {+-}size{KMGTP}: 
Last sector (67110912-3907029134, default = 3907029134) or {+-}size{KMGTP}: 
Current type is 8300 (Linux filesystem)
Hex code or GUID (L to show codes, Enter = 8300): 
Changed type of partition to 'Linux filesystem'
```

Then format it with 

```
sudo mkswap /dev/sde2
```


