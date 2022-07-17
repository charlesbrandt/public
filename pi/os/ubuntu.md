# Ubuntu

Ubuntu has a server distribution tailored to the pi. For newer Pi's (e.g. >= 4, 400) you can also download a desktop version of Ubuntu. 

https://ubuntu.com/download/raspberry-pi

Choosing Ubuntu allows easy installation of any other packages in the Ubuntu / Debian ecosystem (as long as the software provides ARM binaries). 


## Ubuntu

https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#2-prepare-the-sd-card

https://ubuntu.com/download/raspberry-pi  
Install Ubuntu on a Raspberry Pi | Ubuntu  

If you're not installing a pre-made image configured for a specific application (e.g. Plex, Home Assistant, Motion Eye, etc...), this is a good foundation to start with. Can add a gui later if you're not sure if it's for desktop or server use. 

A 64bit OS is required to use more than 4GB of memory. If you have a Pi with 8GB of memory, find a 64 bit OS. 

If running 32bit os, don't buy anything bigger than 4GB of memory.

64 bit OS does not work with Raspberry Pi Zero W.

32 Bit ubuntu (20.04) did not boot raspberry pi zero w -- seems to just hang. 

### Networking

These instructions cover setting up networking on the SD card before you boot the image:

https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#3-wifi-or-ethernet

In summary: Edit the `network-config` file on the system-boot partition with something like:

```
ethernets:
  eth0:
    dhcp4: false
    addresses: [192.168.0.100/24]
    gateway4: 192.168.0.1
    nameservers:
      addresses: [8.8.8.8,4.2.2.2]
```


With ethernet you only need to plug in the cable and you should be assigned an IP address via DHCP. Ideally do this after you change the password, just to be sure. 

[Once the system is running, networking is configured like any other Ubuntu server.](/system/network.md)

Alternative instructions for static IPs available:

[Static IP](../../system/network.md#interface-configuration)



### Change the default password

Once you boot up, you can log in with 

U: ubuntu
P: ubuntu

You will need to change this after logging in. Fire up your password manager to keep track. 

Note: It can take a few minutes after the initial boot before the ubuntu user is created / initialized. 
https://askubuntu.com/questions/1199589/ubuntu-server-default-password-using-raspberry-pi-image


### Variants

If you plan to use a Pi as a daily driver / desktop, you may want to customize the desktop / distribution:

https://ubuntu-pi-flavour-maker.org/download/


