via:
https://www.cnx-software.com/2012/08/26/how-to-allow-apps-to-write-files-to-usb-mass-storage-devices-in-android/
https://stackoverflow.com/questions/43529588/termux-write-access-to-usb-pen-drive



    Can’t install on SD card or USB device

The solution below is adapted from a solution on XDA Developers Forums. Some instructions tells you to use an app such as Root Explorer to edit the files in Android (ES File Manager will also do, after allowing “Up to Root” option), but I personally prefer to run Dropbear SSH server, connect via SSH and edit text files with vi. After login to the system via SSH or adb, remount the system partition in read/write mode:
Shell
mount -o remount,rw /system
1
	
mount -o remount,rw /system


Go to /system/etc/permissions/, make a backup of platform.xml and edit it:
Shell
cd /system/etc/permissions/
busybox cp platform.xml platform-old.xml
busybox vi platform.xml
1
2
3
	
cd /system/etc/permissions/
busybox cp platform.xml platform-old.xml
busybox vi platform.xml


Add a line with <group gid=”media_rw” /> to WRITE_EXTERNALS_STORAGE, so that the section looks like:
Shell
<permission name="android.permission.WRITE_EXTERNAL_STORAGE" >
<group gid="sdcard_rw" />
<group gid="media_rw" />
</permission>
1
2
3
4
	
<permission name="android.permission.WRITE_EXTERNAL_STORAGE" >
<group gid="sdcard_rw" />
<group gid="media_rw" />
</permission>


Save the file, and reboot your device.

After following these steps, I was able to complete the installation of aTorrent, and download some files via Bit Torrent to my USB drive (in /mnt/usbhost1).
