# Rename a drive in linux

## Determine correct device and filesystem type

Make sure it is actually FAT FAT32 section... if FUSE, may be NTFS

    mount -a

dmesg may also give some clues



sudo umount <device>




## FAT32

Set Label using :

sudo dosfslabel <device> label

https://unix.stackexchange.com/questions/94811/renaming-a-hard-drive

sudo dosfslabel /dev/sdc2 DATA-2018

sudo dosfslabel /dev/sdc2 DATA-2018
Currently, only 1 or 2 FATs are supported, not 0.

sudo mlabel -i /dev/sdc2 -s ::


## NTFS

ntfslabel /dev/sdc2 Seagate_Backup_Plus_Drive





## Drafts

# 2019.08.21 08:28:37
first draft

# 2019.09.09 16:37:23
update

