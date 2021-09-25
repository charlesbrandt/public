# Rename a drive in linux

## Determine correct device and filesystem type

[See format notes](format.md)

## Unmount

once you've determined the filesystem type, you'll need to unmount the device. This is different than ejecting:

```
    sudo umount <device>
```

for example:

```
    sudo umount /dev/sde1
```


## FAT32

Set Label using :

```
sudo dosfslabel <device> label
```

https://unix.stackexchange.com/questions/94811/renaming-a-hard-drive

```
sudo dosfslabel /dev/sdc2 DATA-2018

sudo dosfslabel /dev/sdc2 DATA-2018
```

Currently, only 1 or 2 FATs are supported, not 0.

```
sudo mlabel -i /dev/sdc2 -s ::
```

## NTFS

```
ntfslabel /dev/sdc2 Seagate_Backup_Plus_Drive
```

