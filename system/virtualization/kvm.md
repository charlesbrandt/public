# KVM / QEMU

KVM and Qemu provide Virtual Machines on Linux. Lighter weight and easier to manage remotely than virtualbox.

### KVM Installation and Deployment

Guide: https://help.ubuntu.com/community/KVM/Installation

```
sudo apt-get install qemu-kvm libvirt-daemon-system libvirt-clients bridge-utils
```

Add user to correct group:

Cosmic (18.10)

The group name is changed to libvirt:

```
sudo adduser `id -un` libvirt
```

Adding user `<username>` to group `libvirt` ...

After the installation, you need to relogin so that your user becomes an effective member of kvm and libvirtd user groups. The members of this group can run virtual machines. 

## Virsh

Verify everything works as expected by running:

```
virsh list --all
```

## Virt-manager

Optional: Install virt-manager (graphical user interface)

If you are working on a desktop computer you might want to install a GUI tool to manage virtual machines.

```
sudo apt-get install virt-manager
```

To start and stop VMs:
    
```
sudo virt-manager
```

Under main virt-viewer window:

View -> Scale Display -> Always

Under main virt-manager, current machine settings ('i')

Video Virtio -> Video Model -> Virtio
(set while machine is off?)


spice-vdagent
copy-paste for qemu




For a list of osinfo strings to use with the `--os-variant` option:

```
sudo apt install libosinfo-bin
osinfo-query os
```

-s is the size, in GB, of the disk image:

```
sudo virt-install --connect qemu:///system -n xubuntu -r 2048 -f xubuntu.qcow2 -s 16 -c Downloads/xubuntu/xubuntu-18.04-desktop-amd64.iso --vnc --noautoconsole --os-type linux --os-variant ubuntu18.04 --accelerate --network=network:default
```

Connect to the new VM using virt-viewer:

```
virt-viewer -c qemu:///system xubuntu
```

To change the display size, just use the native OS settings to adjust the size. (No integration like VirtualBox Addons that changes the resolution dynamically when the window is resized)


## Prerequisites / Troubleshooting

Make sure that your system has the hardware virtualization extensions: For Intel-based hosts, verify the CPU virtualization extension [vmx] are available using following command.

    [root@server ~]# grep -e 'vmx' /proc/cpuinfo

For AMD-based hosts, verify the CPU virtualization extension [svm] are available.

    [root@server ~]# grep -e 'svm' /proc/cpuinfo

If there is no output make sure that virtualization extensions is enabled in BIOS. 

Verify that KVM modules are loaded in the kernel “it should be loaded by default”.

    [root@server ~]# lsmod | grep kvm

The output should contains kvm_intel for intel-based hosts or kvm_amd for amd-based hosts.


## See also

https://www.tecmint.com/install-and-configure-kvm-in-linux/

