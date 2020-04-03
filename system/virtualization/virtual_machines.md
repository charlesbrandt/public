# Virtual Machines

Virtualization software abstracts computer hardware so that you can run full operating systems on top of an already running (and potentially different) operating system. Instances of these encapsulated, running operating systems are called virtual machines. 

If you want to avoid duplicating full system resources and files, consider a container solution like [docker](docker.md).




## Solutions

There are a number of different solutions for virtualization. Popular options include:

 - KVM
 - Virtual Box
 - VMWare
 - Nutanix (AHV)

## 2013.05.24 10:54:04
Setting up a virtual machine is a nice way to run a guest operating system without needing to reboot into that system separately.  Rebooting can often be a hassle if the guest system is not needed as often.

So far, I'm trying VirtualBox for this task... it's cross platform and open source and free... a tough combination to beat.
https://www.virtualbox.org/wiki/Downloads

https://www.virtualbox.org/manual/ch01.html


VMWare and Parallels (for Mac) get good reviews and apparently perform better, but are pricier.  I'll wait and see if I notice any performance issues with VirtualBox and then go from there.

Here are some comparisons and links:

http://www.civicactions.com/node/1271
http://pcnss.co.uk/mac-os-x-virtual-machine-comparison/
http://www.parallels.com/products/desktop/
https://buy.parallels.com/329/purl-us-pd8f
https://www.google.com/search?q=virtualbox+vs+xen
http://virtualization.findthebest.com/saved_compare/Xen-vs-KVM-vs-VirtualBox-Comparison-of-Open-Source-Virtualization-Software





