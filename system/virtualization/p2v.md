# Physical To Virtual (P2V)

Take a physical computer and back up the operating system so that you can run it in a [virtual machine](virtual-machine.md).

## libguestfs

https://www.libguestfs.org/  
libguestfs, library for accessing and modifying VM disk images  


```
sudo apt-get install libguestfs-tools
```

Includes tools like: `virt-p2v`

Use `virt-p2v` to convert a physical machine to a virtual one.

Requires:

 - A bootable image of virt-p2v
 - A destination machine on the same network that can receive the new image data
 
### Create the image

```
virt-p2v-make-disk -o /dev/sdX [os-version]
```

However, when I did:

```
sudo virt-p2v-make-disk -o /dev/sdc
```

I received:

```
virt-p2v-make-disk: cannot find dependencies file (/share/virt-p2v/dependencies.debian)
```

You can use

```
virt-builder -l
```

To see available os versions. 

TODO: how to download dependencies? 


https://www.systutorials.com/docs/linux/man/1-virt-p2v-make-disk/  
virt-p2v-make-disk: Build the virt-p2v disk using virt-builder - Linux Man Pages (1)  

https://duckduckgo.com/?q=virt-p2v-make-disk&hps=1&atb=v343-5ja&ia=web  
virt-p2v-make-disk at DuckDuckGo  
https://duckduckgo.com/?q=ubuntu+virt-p2v&t=ffab&atb=v343-5ja&ia=web  
ubuntu virt-p2v at DuckDuckGo  
https://manpages.ubuntu.com/manpages/bionic/man1/virt-p2v.1.html  
Ubuntu Manpage: virt-p2v - Convert a physical machine to use KVM  



https://www.systutorials.com/docs/linux/man/1-virt-p2v/  
virt-p2v: Convert a physical machine to use KVM - Linux Man Pages (1)  
https://duckduckgo.com/?q=linux+p2v&hps=1&atb=v343-5ja&ia=web  
linux p2v at DuckDuckGo  
https://www.storix.com/asr/linux-p2v/  
Linux P2V (Physical to Virtual) Migrations Made Easy | Storix Software  
https://duckduckgo.com/?t=ffab&q=linux+p2v+kvm&atb=v343-5ja&ia=web  
linux p2v kvm at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=virt-v2v&atb=v343-5ja&ia=web  
virt-v2v at DuckDuckGo  
https://access.redhat.com/articles/1351473  
Converting virtual machines from other hypervisors to KVM with virt-v2v in RHEL 7, RHEL 8, and RHEL 9 - Red Hat Customer Portal  
https://duckduckgo.com/?t=ffab&q=proxmox&atb=v343-5ja&ia=web  
proxmox at DuckDuckGo  
https://www.proxmox.com/en/  
Proxmox - Powerful open-source server solutions  
https://duckduckgo.com/?t=ffab&q=proxmox+vs+virt-manager&atb=v343-5ja&ia=web  
proxmox vs virt-manager at DuckDuckGo  
https://www.reddit.com/r/homelab/comments/rfnuxl/advice_proxmox_vs_virtmanager/  
Advice: proxmox vs virt-manager : homelab  
https://duckduckgo.com/?t=ffab&q=kvm+qemu&atb=v343-5ja&ia=web  
kvm qemu at DuckDuckGo  
https://wiki.qemu.org/Features/KVM  
Features/KVM - QEMU  
https://www.qemu.org/  
QEMU  
https://libvirt.org/  
libvirt: The virtualization API  
https://sumit-ghosh.com/articles/virtualization-hypervisors-explaining-qemu-kvm-libvirt/  
Virtualization and Hypervisors :: Explaining QEMU, KVM, and Libvirt | Sumit’s Dreams of Electric Sheeps  
https://duckduckgo.com/?t=ffab&q=LXD&atb=v343-5ja&ia=web  
LXD at DuckDuckGo  
https://linuxcontainers.org/lxd/  
Linux Containers - LXD - Introduction  
https://duckduckgo.com/?t=ffab&q=virt-p2v+iso&atb=v343-5ja&ia=web  
virt-p2v iso at DuckDuckGo  
https://www.humblec.com/how-to-build-a-virt-p2v-iso-in-fedora-f18f17/  
How to build a virt-p2v ISO in fedora ( F18/F17) using virt-p2v-image-builder ? – My Humble Abode  
https://libguestfs.org/virt-p2v.1.html  
virt-p2v  



## Clonezilla

Download Clonezilla Live from http://clonezilla.org/downloads.php 

Verify the [checksums](../checksums.md).

[Create bootable media with the image.](../storage/bootable-usb.md)

https://clonezilla.org/clonezilla-live.php

Boot the physical system to be virtualized with Clonezilla.

Once booted, perform a backup as follows:

https://clonezilla.org/clonezilla-usage/clonezilla-live-usage.php

At the menu, select “Clonezilla Live”  
Choose your language when prompted  
Select your desired keymap as needed  
At the menu, select “Start_Clonezilla”  
Specify “device-image”  
Select “local_dev” to save the image to a local device (NOTE: if you need to save it somewhere else, make the proper selection)  
When asked to connect USB devices, do so and then press “Enter”  
Choose the desired drive or partition to save the image  
A directory structure will be shown for the selected drive or partition. Select the appropriate folder in which to place the backup image  
A listing of disk space usage for the selected location will be displayed. Press “Enter”  
Choose “Expert” for the backup mode  
Select savedisk (when backing up a whole disk), or savepart (when backing up a single partition) - for my example, I am using savepart so some prompts may be a little different  
A prompt for the name of the backup is required. Remove the existing information and give it a simple name like “LinuxBack”. The name given is the folder name in which the backup files are placed. Remember the name so you know the location of the backup files  
A listing of existing drives and/or partitions will be listed. Highlight the one to backup, press “spacebar” and then “enter”  
At the next menu, select -q1 for a dd backup  
The next menu shows various parameter options to select. By default, the -c and -j2 are checked. The defaults are fine unless you require others  
When prompted for the compression type, select -z0  
At the next menu when prompted about splitting the file, make sure the size is larger than the drive or partition being backed up  
Select the option to “Skip checking/repairing source file system”, unless you believe it is necessary  
At the next menu select to skip checking the saved image file  
For the action to perform when the backup is completed, select what you prefer  
At this point, the options are set and the command-line should be listed which is being executed to perform the backup. Press “enter”  
You are prompted if you want to continue and perform the backup, so press “y” and then “enter”  
The backup should begin  
Exit Clonezilla when the backup is completed  


## VirtualBox

this looks like a good guide -- may give this a try:

https://www.linux.org/threads/physical-to-virtual-p2v-using-virtualbox.10928/

Imported below for customization / review. 

Some people may want to run their existing system on another. Others may want to test an existing environment with specific apps or even updates. There may be many reasons to move an existing environment from one machine to another, or even the same, under VirtualBox.

The process may take a little time and plenty of disk space. For this procedure to work you will need:

  - VirtualBox from Oracle at https://www.virtualbox.org/wiki/Downloads

  - Super GRUB 2 ISO at http://www.supergrubdisk.org/category/download/supergrub2diskdownload/super-grub2-disk-stable/ (needed only if your disk or partition does not contain GRUB or some boot loader; leave as an ISO)
  
  - Plenty of drive space
  
VirtualBox should be installed on the system to which you want to run the virtualized Operating System (OS). Super Grub should also be copied to the system with VirtualBox to be used for booting the virtual image if the partition or disk does not contain the boot files.

The drive space needed for the backup must be accessible to Clonezilla for a backup to be performed.



Copy the directory specified in option 14 to the system with VirtualBox, if the virtual system will be run on another system, and be sure to include the directory itself (in my example it would be called LinuxBack)

Once the backup folder and files are moved to the system with VirtualBox installed, perform the following command:

VboxManage convertfromraw --format VDI source.img target.vdi
The source is the file in the backup folder which should end with .aa. In my example it should be in a folder call LinuxBack. The partition I backed up was partition sda5. The filename was “sda5.dd-img.aa”. The file can be renamed to “sda5.dd-img.img” or any other name, or leave it as it is. The targe.vdi file can be named whatever you like, but try to keep it simple. For example, the command could be “VboxManage convertfromraw --format VDI sda5.dd-img.aa sda5.vdi” to create a VDI file from the backup image.

The conversion from the image to a VDI file can take quite some time. The larger the image file, the longer the conversion can take.

Once done, you can open VirtualBox and create a new session. Set it up according to the physical system you are virtualizing. Set the hard drive to point to the VDI file created. Set the CD/DVD to point to the Super GRUB 2 ISO file. When the system boots select “Everything” and then, on the next menu, select the option for your kernel (should be the second line in the list). The system should start as normal.

If an error occurs similar to “The disk drive for UUID=### is not ready yet or not present.”, press “s” to skip the error until the OS loads. Once loaded, open an editor with root privileges and edit /etc/fstab to comment out with '#' any drive which is not accessible to the OS.


### Virtual Box Alternative

This one has the most promise:
https://askubuntu.com/questions/34802/convert-my-physical-operating-system-to-a-virtualbox-disk

I would use gparted before running dd. With dd if you have a 200GB Linux OS partition your going to get a 200GB file. Even if you are only using 8GB of it. Then when you convert your going to need 400GB total. So use gparted, shrink to 9GB (give it a little space) then run DD. To note it does take a long time to run gparted, but in the long run its a lot faster then doing a VBoxManage on a 200GB bin DD backup. :) – Psytek7 Dec 1 '12 at 2:26

Yes, however it will most likely take a very long time. I haven't actually tried the steps below, but they should work.

First, you need to make an image of your entire partition. You need to know the partition path of the Ubuntu partition, for example /dev/sda1 would be the first partition on the first attached hard drive.

    sudo dd if=PART_PATH of=OUTPUT_PATH/ubuntu.bin

note that you should do the above command from a liveCD with the partition you are trying to copy unmounted -- i.e. you need to have another hard drive or something to copy all the data to.

You can shut the liveCD down and boot back into Ubuntu to perform the last step:

Convert the binary into a vbox drive:

    VBoxManage convertdd PATH_TO_ubuntu.bin ubuntu.vdi --format VDI

you can then use the resulting ubuntu.vdi as a Virtualbox drive. Just make sure you have plenty of time and hard drive space for this operation. A little tip: commandline tasks can be suspended by hitting Ctrl+Z. The operation will go to sleep, and you can use you computer again. When you are interested in resuming the process, type fg in the terminal, and the operation will resume. Quite handy for long operations like this.

## Links

some research in to p2v  
not a real need to actually running the old system  
so in this case a backup will suffice  

but wanted to jot down resources that looked promising

https://www.sysprobs.com/physical-virtual-virtualbox-virtualbox-p2v  
Physical to Virtual in VirtualBox - VirtualBox P2V  
https://duckduckgo.com/?q=linux+p2v+virtualbox&t=canonical&ia=qa  
linux p2v virtualbox at DuckDuckGo  
https://askubuntu.com/questions/34802/convert-my-physical-operating-system-to-a-virtualbox-disk  
virtualization - Convert my physical Operating System to a VirtualBox Disk - Ask Ubuntu  
  
https://www.google.com/search?client=ubuntu&hs=6VY&ei=LnR4XLLtF8retQW5wof4Cw&q=p2v+linux+virtualbox&oq=p2v+linux+virtualbox&gs_l=psy-ab.3..0j0i22i30.128052.132329..132757...0.0..0.99.997.11......0....1..gws-wiz.......0i71j0i67j0i22i10i30.tCZ9-5wcBB8  
p2v linux virtualbox - Google Search  
https://askubuntu.com/questions/34802/convert-my-physical-operating-system-to-a-virtualbox-disk  
virtualization - Convert my physical Operating System to a VirtualBox Disk - Ask Ubuntu  
https://blogs.technet.microsoft.com/enterprise_admin/2010/05/13/linux-p2v-with-dd-and-vhdtool-easy-and-cheap/  
Linux P2V With DD and VHDTool – EASY and CHEAP! – John Kelbley's real life enterprise interop and administration  
https://www.linux.org/threads/physical-to-virtual-p2v-using-virtualbox.10928/  
Physical To Virtual (P2V) using VirtualBox | Linux.org  
https://askubuntu.com/questions/308897/convert-ubuntu-physical-machine-to-virtual-machine  
virtualbox - Convert Ubuntu Physical machine to Virtual machine - Ask Ubuntu  
  
http://www.localizingjapan.com/blog/2011/03/05/virtualizing-a-linux-system-creating-a-linux-vm-p2v/  
  
https://www.google.com/search?client=ubuntu&hs=CRM&channel=fs&ei=P6NSWrvlB8r8jwT34ZSYCg&q=linux+p2v+virtualbox&oq=linux+p2v+virtualbox&gs_l=psy-ab.3..0.177980043.177981315.0.177981563.6.6.0.0.0.0.173.436.0j3.3.0....0...1c.1.64.psy-ab..3.3.431...0i7i30k1.0.5iBFlSVVGOQ  
linux p2v virtualbox - Google Search  
https://www.virtualbox.org/wiki/User_HOWTOS  
User_HOWTOS – Oracle VM VirtualBox  
https://www.virtualbox.org/wiki/Migrate_Windows  
Migrate_Windows – Oracle VM VirtualBox  
http://www.sysprobs.com/physical-virtual-virtualbox-virtualbox-p2v  
Physical to Virtual in VirtualBox - VirtualBox P2V  
https://www.vmware.com/products/converter.html  
VMware vCenter Converter: P2V Virtual Machine Converter  
