# FSCK

Filesystem check helps fix any problems that come up from unclean drives. Filesystems can get out of sync when the operating system shuts down unexpectedly. 


## NTFS

via:
https://askubuntu.com/questions/47700/fix-corrupt-ntfs-partition-without-windows

Install ntfs-3g with sudo apt-get install ntfs-3g. Then run the ntfsfix command on your NTFS partition.

For example:

```
ntfsfix /dev/hda6
```

```
ntfsfix v2.0.0 (libntfs 10:0:0)

Usage: ntfsfix [options] device

Attempt to fix an NTFS partition.

-h, --help             Display this help
-V, --version          Display version information
Developers' email address:

linux-ntfs-dev@lists.sf.net Linux NTFS homepage: http://www.linux-ntfs.org

For newer Ubuntus You can use -b and -d option together. -b tries to fix bad clusters and -d to fix dirty states. So the command can be

sudo ntfsfix -b -d /dev/sda6
--help shows them

ntfsfix v2015.3.14AR.1 (libntfs-3g)

Usage: ntfsfix [options] device
    Attempt to fix an NTFS partition.

    -b, --clear-bad-sectors Clear the bad sector list
    -d, --clear-dirty       Clear the volume dirty flag
    -h, --help              Display this help
    -n, --no-action         Do not write anything
    -V, --version           Display version information
```

## Testdisk

I've just fixed my USB drive using "testdisk", a Linux command line (yet friendly) utility. My drive was not even mounting in Windows and Windows 8 discovered like 6 partitions (when the drive had only one).

To use the utility, install it:

```
sudo apt-get install testdisk
```

Then run it:

```
sudo testdisk
```

and follow the instructions. You must search for partitions and then write the changes.

Hope this help anyone.