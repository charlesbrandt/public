# Virtual Box

*2017.01.11
If you're installing a new OS on a virtual machine, be sure Virtual Box is up to date... versions change frequently and it's good to use the latest one before setting up new systems.

When creating a virtual machine in virtual box, I specify the OS type, version and bit type. I start with 2048MB of memory... this is easy to adjust later on. For the hard disk file type, choose VMDK (Virtual Machine Disk). This is VMWare's format, but it sounds like it's the most cross-platform. Also may help with backups, and split the disk image into smaller sizes... all great features! Go with dynamically allocated, and then you'll also have the option to split files. 16GB is a good place to start. (6.4GB minimum needed for Xubuntu). Trying out encrypting home folder for virtual... may not apply to anything in /c.

If installing to a virtual machine, avoid allowing the installation to download and apply updates automatically. Disconnect from the internet. To be sure: Virtual Box -> Devices -> Network -> Disconnect Cable. That way you can apply the updates whenever you want to clone and re-run the setup scripts.

Consider implications of using LVM with disks

For VirtualBox, you'll need a "Host-only Adapter" in order to access the machine. When the VM is powered off, add another network interface:

    Virtual Box -> Devices -> Network -> Network Settings...
    Adapter 2 -> Enable Network Adapter
    Attached to: Host-only Adapter
    Name: vboxnet0

Then, when the machine is powered on, do:

    #find all adapters... should be a new one that is unconfigured
    ip addr
    #enp0s8 for me
    sudo vi /etc/network/interfaces
    #add new section for interface, adapting from lines already there

    # The local network interface
    auto enp0s8
    iface enp0s8 inet dhcp

    sudo /etc/init.d/networking restart
    #check for new ip with:
    ifconfig


If you want to connect to the machine from outside of your local machine, you'll want to set up port forwarding. Back on "Adapter 1", toggle "Advanced" and click the "Port Forwarding" button. Add a new rule:

    - Host IP: blank
    - Host Port: 8822
    - Guest IP: 192.168.56.101
    - Guest Port: 22

Also be sure to open up 8822 (or whatever you choose) in the firewall:

     sudo ufw allow 8822

Start the system back up.




# on ubuntu server 16.04.1 needed this:

    sudo apt install build-essential

# may need to do this if running before ansible:

    sudo apt update
    sudo apt upgrade


#Virtual Box -> Devices -> Insert Guest Additions CD image... -> Download
sudo mount /dev/sr0 /media/cdrom
sudo /media/cdrom/VBoxLinuxAdditions.run


Eject Guest Additions form guest OS file browser, or:

    sudo umount /media/cdrom

#restart Guest OS
#can now resize screen too

#can do this sooner, but it won't work until now:
enable clipboard sharing, bidirectional.
Devices -> Shared Clipboard -> Bidirectional

#this requires proprietary oracle guest additions:
Virtual Box -> Devices -> Shared Folders -> Shared Folders Settings... -> Add icon
Choose "Folder Path": /c
Check "Make Permanent"

It's best not to configure any external media until you are working on a cloned system.
e.g. if you might want access to DATA drives, set that up later

see also:

    ansible/shares.yml


Decide if you want to clean up any downloaded package files:

    ls /var/cache/apt/archives/*.deb

    sudo rm /var/cache/apt/archives/*.deb








Under Machine -> Settings -> User Interface
un-check the bottom status bar to hide it... only takes up space in my opinion (can always enable it later if needed)





#(ansible will help with this step)
#consider adding this to fstab!
sudo mount -t vboxsf -o uid=1000,gid=1000 c /c
#http://askubuntu.com/questions/481559/how-to-automatically-mount-a-folder-and-change-ownership-from-root-in-virtualbox
# http://askubuntu.com/questions/252853/how-to-mount-a-virtualbox-shared-folder-at-startup#252865







*2015.11.07 15:37:14
when cloning a VM using VMDK with multiple files, the clone does not preserve that setting (lumps everything back in to one big file)

research command line options... maybe there is a way to clone and preserve there:

VBoxManage createhd --filename /space/vdis/ubuntu9.10.vmdk --size 10000 --format VMDK --variant Split2G -remember

*2015.11.05 23:52:35
discovered that VMDK is a better format for disk images... allows image file to be broken in to smaller (2GB) pieces... this allows the image to be copied to a FAT32 system:
http://superuser.com/questions/360517/what-disk-image-should-i-use-with-virtualbox-vdi-vmdk-vhd-or-hdd
http://www.vmware.com/pdf/esx3_backup_wp.pdf

When it comes time to migrate between platforms (VMWare vs Virtual Box):
http://kb.vmware.com/selfservice/microsites/search.do?language=en_US&cmd=displayKC&externalId=2053864


*2016.05.01 08:38:00
#virtualbox
----------------------------------------------
mkdir -p /c/virtual_box/installs
cd /c/virtual_box/installs


[snip virtualization-hypervisor.txt]




#In preferences:
File->Preferences...->Network->Host-only Networks->Add icon (right)->vboxnet0

#Add Extension Pack
File->Preferences...->Extensions

#change Default Machine Folder to be:
/c/virtual_box/

#or external drive:
/media/charles/DATA/virtual_box

It worked to boot directly from external drive (may be slower than host)...
If you use an image often, it would be good to keep it local










*2015.11.07 18:32:32
difficulty getting USB to work on guest VMs... tried the version from Oracle even... hmmm... punting and mounting the device as a shared file for now (if needed)

*2015.10.25 08:27:32
Still using VirtualBox... good enough!

For sharing files, it should be possible to have a shared folder between host and guest... have done this on OS X... maybe requires extensions?

However, it is also possible to just use rsync.
This requires SSH to be installed on guest, which is probably a good thing to enable anyway for Ansible

*2015.10.28 07:33:07 links
http://xubuntu.org/getxubuntu/
Get Xubuntu « Xubuntu
https://www.google.com/search?client=ubuntu&channel=fs&q=virtualbox+share+files&ie=utf-8&oe=utf-8#channel=fs&q=virtualbox+network+connect+to+guest
virtualbox network connect to guest - Google Search
http://askubuntu.com/questions/52147/how-can-i-access-apache-on-virtualbox-guest-from-host
apache2 - How can I access Apache (on VirtualBox guest) from host? - Ask Ubuntu
http://www.slideshare.net/powerhan96/networking-between-host-and-guest-v-ms-in-virtual-box
Networking between host and guest VMs in VirtualBox
https://www.virtualbox.org/manual/ch06.html
Chapter 6. Virtual networking
http://askubuntu.com/questions/366742/how-to-share-the-files-from-host-to-guest-in-virtual-box-host-ubunutu-guest-ubu
12.04 - How to share the files from Host to Guest in virtual box? Host-ubunutu/Guest-ubuntu - Ask Ubuntu
https://www.google.com/search?client=ubuntu&channel=fs&q=ubuntu+allow+ssh&ie=utf-8&oe=utf-8
ubuntu allow ssh - Google Search
https://help.ubuntu.com/community/SSH/OpenSSH/Configuring
SSH/OpenSSH/Configuring - Community Help Wiki

*2015.11.07 18:04:37 links
https://duckduckgo.com/?q=virtual+box+vboxsf+fstab+permissions&t=canonical
virtual box vboxsf fstab permissions at DuckDuckGo
https://www.virtualbox.org/ticket/2634
#2634 (Cannot change Shared Folder permissions) – Oracle VM VirtualBox
https://duckduckgo.com/?q=ansible+mount&t=canonical
ansible mount at DuckDuckGo
http://docs.ansible.com/ansible/mount_module.html
mount - Control active and configured mount points — Ansible Documentation
https://duckduckgo.com/?q=virtualbox+clone+multiple+vmdk&t=canonical
virtualbox clone multiple vmdk at DuckDuckGo
http://www.virtualbox.org/manual/ch05.html
Chapter 5. Virtual storage
https://duckduckgo.com/?q=virtualbox+vmdk+multiple+files&t=canonical
virtualbox vmdk multiple files at DuckDuckGo
https://blogs.oracle.com/ravee/entry/split_virtualbox_vdi_images
Split VirtualBox VM disk images (Ravi Chandra Nallan)
https://duckduckgo.com/?q=vboxmanage+clone&t=canonical
vboxmanage clone at DuckDuckGo
http://www.virtualbox.org/manual/ch08.html
Chapter 8. VBoxManage
http://www.virtualbox.org/manual/ch08.html#vboxmanage-clonevm
Chapter 8. VBoxManage
https://www.virtualbox.org/ticket/14396
#14396 (All VM only run if in detachable start) – Oracle VM VirtualBox
http://abdeldjalil-fellah.weebly.com/home/virtualbox-50-separate-mode-detachable-start
VirtualBox 5.0 Separate Mode (Detachable Start) - Abdeldjalil FELLAH
https://duckduckgo.com/?q=virtual+box+use+usb+drive&t=canonical
virtual box use usb drive at DuckDuckGo
http://dedoimedo.com/computers/virtualbox-usb.html
How to use USB devices in VirtualBox - Linux tutorial
https://duckduckgo.com/?q=virtual+box+no+usb+device+available+usb3&t=canonical
virtual box no usb device available usb3 at DuckDuckGo
http://superuser.com/questions/956622/no-usb-devices-available-in-virtualbox
linux - No USB devices available in VirtualBox - Super User
https://help.ubuntu.com/community/VirtualBox/USB
VirtualBox/USB - Community Help Wiki
https://www.virtualbox.org/ticket/3033
#3033 (USB devices not available to guest) – Oracle VM VirtualBox
http://www.sysprobs.com/guide-download-install-oracle-vm-virtualbox-extension-pack-windows-host
How to Download and Install Oracle VM VirtualBox Extension Pack 4.3.14 in Windows Host
https://www.virtualbox.org/wiki/Downloads
Downloads – Oracle VM VirtualBox
https://www.virtualbox.org/wiki/Download_Old_Builds_5_0
Download_Old_Builds_5_0 – Oracle VM VirtualBox
http://www.oracle.com/technetwork/server-storage/virtualbox/downloads/index.html#extpack
Oracle VM VirtualBox - Downloads | Oracle Technology Network | Oracle
https://duckduckgo.com/?q=adduser%3A+The+group+%60vboxusers%27+does+not+exist.&t=canonical
adduser: The group `vboxusers' does not exist. at DuckDuckGo
http://ubuntuforums.org/showthread.php?t=1583405
[ubuntu] vboxusers group doesn't exist?
https://duckduckgo.com/?q=firefox+.deb+package+download+disappears&t=canonical
firefox .deb package download disappears at DuckDuckGo
