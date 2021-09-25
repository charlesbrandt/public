# HFS plus

It's been a long time since I formatted a drive to use a native Mac filesystem. 

On os x 10.8 (10.7+) they've eliminated formatting drive as "Mac OS Extended" (non-journaled).

Linux cannot write to HFS+ Journal, so must diable Journal:
To get around this:

You must partition it journaled; then select the partition in Disk Utility's sidebar, press the Option key before selecting File from the menubar, and you'll see a Disable Journaling option.

https://discussions.apple.com/thread/3232454?start=0&tstart=0


other options:

sudo parted -a optimal /dev/sdb mklabel mac
sudo parted -a optimal /dev/sdb mklabel msdos  #is this Master Boot Record?

sudo parted -a optimal /dev/sdb mkpart primary NTFS 20.5kB 2000GB
Warning: The resulting partition is not properly aligned for best performance.
Ignore/Cancel? C


examples:

formatted with mac os X: (GUID Partition Table)
---------------------------------------------------
sudo parted -a optimal /dev/sdb print
Model: SAMSUNG HD204UI (scsi)
Disk /dev/sdb: 2000GB
Sector size (logical/physical): 512B/512B
Partition Table: gpt

Number  Start   End     Size    File system  Name                  Flags
 1      20.5kB  210MB   210MB   fat32        EFI System Partition  boot
 2      210MB   2000GB  2000GB  hfs+         Untitled

 <
