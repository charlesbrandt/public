# Raspberry Pi OS

Provides a common ground when documenting your projects: 

https://www.raspberrypi.com/software/

## Standard vs Lite vs Full

Standard is the one to use for most cases. If it is a server only and you won't ever need a desktop environment, choose lite. If you want applications like an office suite, choose full.

https://peppe8o.com/raspberry-pi-os-lite-vs-desktop/

## 32bit vs 64bit

Use for raspberry pi zero W

Maybe 32 Bit Ubuntu does not work with Raspberry Pi Zero W? 

In that case, 32 bit Raspberry Pi OS is the best choice?

may be good for pis intended for device service (e.g. camera

Can use a spare raspberry pi 4 (4GB memory) to run the install and set things up more efficiently)
(probably good enough for most server purposes)

install / setup on pi zero is slooooow

## Configuration

remove overscan which results in a black border for pi display
1824x984
sudo raspi-config
Advanced -> Overscan

After a restart, the issue was fixed!

## Static IP

https://www.makeuseof.com/raspberry-pi-set-static-ip/

Find the current IP

```
hostname -I
```

Find the current default gateway:

```
ip r | grep default
```

Find the current DNS server

```
sudo nano /etc/resolv.conf
```

Configure the static IP via

```
sudo vi /etc/dhcpcd.conf
```

Add the following lines at the end

```
interface (eth0/wlan0)
static ip_address=STATIC_IP/24
static routers=ROUTER_IP 
static domain_name_servers=DNS_IP
```

Reboot the machine

## SSHD

Solution 1: create ssh file in the boot directory on the SD card

If you don’t have the ability to operate Raspberry directly via keyboard and screen, you can use a simple trick instead: Use an external computer to access the microSD card on which you have installed Raspbian and then create a file called ssh in the boot directory. It’s important that you do not use a file extension in this case and make sure that it is not automatically added (this often happens with Windows). If you then reboot the mini computer, SSH access will be enabled.

Solution 4: start the SSH service with systemctl

As an alternative to raspi-config, you can use the systemctl command line tool to set up SSH on your Raspberry Pi. Simply enter the following two commands into the terminal:

```
sudo systemctl enable ssh
sudo systemctl start ssh
```

While the first command causes the SSH server to start automatically as soon as the mini computer is booted, the second command starts the server in the current session.

adapted via:

https://www.ionos.com/digitalguide/server/configuration/raspberry-pi-how-to-set-up-an-ssh-server/


## Raspbian

Previously, Raspberry Pi OS was called Raspbian. They're the same thing. Just something to be aware of if you come across documentation referring to Rasbian, you'll know. 


## Default passwords

Newer versions of Raspberry Pi OS no longer include a default password, but if you have an older system around and need access:

https://duckduckgo.com/?t=ffab&q=raspberry+pi+os+default+login&ia=web  
raspberry pi os default login at DuckDuckGo  
https://tutorials-raspberrypi.com/raspberry-pi-default-login-password/  
List of all Raspberry Pi Default Logins and Passwords  
