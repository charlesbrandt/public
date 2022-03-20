# Pi Cameras

## Purchasing

Choose the right sensor for the job. 

If you're setting up a security camera and you want to take photos at night, you'll want the NOIR filter version... The version with No Infra-red Filter. That way it reacts to any IR lights you set up. 

If you're working inside with reliable lighting, choose a HQ Camera module for the best quality photos. 
12MP goodness. :)
The High Quality camera module is really nice!


## Kits

Components: 

  - Raspberry Pi Zero + Wireless 

Be sure to get the converter cable! (usually sold separately, sometimes available as part of the case package)


So far I really like the kit that comes with everything you need (and nothing you don't need) from Vilros. 

Seems like all of the pi cases block access to the sim card
maybe that makes them more weather proof?

also need some 1/4" nut attachments (epoxy plus wood?)


## Motion Eye OS

Setting up a raspberry pi as a home security camera. 

Motion Eye OS did a good job of getting up and running quickly. 

https://github.com/ccrisan/motioneyeos  
GitHub - ccrisan/motioneyeos: A Video Surveillance OS For Single-board Computers  

Motion Eye is the web interface for interacting with the motion client. 

https://github.com/ccrisan/motioneye/  
GitHub - ccrisan/motioneye: A web frontend for the motion daemon.  
https://github.com/ccrisan/motioneye/wiki  
Home · ccrisan/motioneye Wiki · GitHub  
https://github.com/ccrisan/motioneye/  
GitHub - ccrisan/motioneye: A web frontend for the motion daemon.  

### Create Image

Using an SD card with more memory (e.g. 64 GB) is a good idea here. Sometimes, on windy / partly cloudy days motion gets triggered frequently and records a lot of otherwise empty videos. These can consume a lot of space locally before they get archived and deleted. 

Find an image for your board. Download. Flash. 

https://github.com/ccrisan/motioneyeos/wiki/Supported-Devices  
Supported Devices · ccrisan/motioneyeos Wiki · GitHub  

github.com/ccrisan/motioneyeos/releases/download/20200606/motioneyeos-raspberrypi-20200606.img.xz

Extract and burn to a micro SD with balenaEtcher

### Device Configuration

It's possible to pre-configure networking, but then the default password is set and it's available on the network. Not ideal. 

TODO: 
possible to change default passwords before first boot? 


#### Configure network interface

Create a new file on the 31 MB 'boot' volume named `wpa_supplicant.conf`
(NOTE: `supplicant.conf` did **not** work for me!)

```
country=US
update_config=1
ctrl_interface=/var/run/wpa_supplicant
network={
    scan_ssid=1
    ssid="your network's SSID"
    psk="your network's password"
}
```

/media/account/6F4C-4508/wpa_supplicant.conf


#### Static IP

possible to set a static IP via wireless?

https://github.com/ccrisan/thingos/wiki/static_ip.conf

Create a new file on the 31 MB 'boot' volume named `static_ip.conf`

/media/account/6F4C-4508/static_ip.conf

```
STATIC_IP=192.168.1.100/24
STATIC_GW=192.168.1.1
STATIC_DNS=8.8.8.8
```

You can use `route -n` to find your current gateway if unsure (May require `sudo apt-get install net-tools`)

This worked for the first boot, but second boot obtained a dynamic (different) IP.

After first boot, connect via SSH and apply the same configuration to:

vi /data/etc/static_ip.conf

#### Password

May be possible to set the password as an environment variable in `/boot/environment`
This would mean that the password is stored in plaintext on the card, but that seems better than an empty root/admin password. 

Hoping this allows remote access via SSH. 

```
PASSWORD=12345678
```

This did not work for me. Wait until the machine boots, then change the password via the web interface. This should set the password to be the same on both the web admin interface and via SSH. 

### First Boot

Unmount the SD card and insert it into the camera. 

Connect your Pi camera via the CSI connector or plug in a USB webcam using a Micro USB OTG adapter, then apply power. If your setup is correct, the Pi will boot up into MotionEyeOS.

If you did not set up a static IP, you can use a network scanner (e.g. router utility) to find its dynamically assigned IP address.

The device will have a default name, such as MEYE-12345678. Once you find the device name, navigate to the IP address on a web browser connected to the same network. A local website will appear, giving you a nice graphic interface on which to access your camera and settings.

To log in as a user, input user as the username with a blank password, or admin with a blank password as well. If you're setting up the system for the first time, use the admin username.

https://www.arrow.com/en/research-and-events/articles/motioneyeos-camera-setup-on-raspberry-pi-zero-w  
MotionEyeOS Raspberry Pi Zero W Tutorial: Setup & Uses | Arrow.com | Arrow.com  

## Preferences

After logging in to the device via the web interface, there are some configurations to set up. 

Timezone to eastern time? Then the timestamps will make more sense

Enable creating movie files
Enable file storage

## Archiving Recordings

Centralized Media Server

If you want to archive video longer than the device can hold, transfer to a different server


### Run Commands

```
export PIIP=192.168.1.2
export SERVERIP=192.168.1.3

ssh root@PIIP
ssh-keygen
ssh-copy-id account@SERVERIP
ssh account@SERVERIP
```

```
rsync -av /data/output/Camera1 account@SERVERIP:~/out-data/front-porch
rm -r /data/output/Camera1/*
```


## Gotchas

Keeping a high resolution video feed active in a web browser can consume a lot of bandwidth, even on a home network. 

Don't keep the stream active in a browser unless you need to see what is happening. 

## Focus

I was unable to focus the lens with the provided lens extender / adapter. 
But without that, it feels pretty loosely connected. 

Just need to play with it to see. 

https://duckduckgo.com/?q=how+to+focus+raspberry+pi+camera+high+quality&t=canonical&ia=web  
how to focus raspberry pi camera high quality at DuckDuckGo  
https://www.arducam.com/raspberry-pi-high-quality-camera-lens/  
Raspberry Pi High Quality Camera Lens Guide: The Key Topics  

## Links

https://duckduckgo.com/?t=canonical&q=use+pi+cam+as+a+security+camera&ia=web  
use pi cam as a security camera at DuckDuckGo  
https://makezine.com/projects/beginner-project-a-remote-viewing-camera-with-raspberry-pi/  
Beginner Project: A Remote Viewing Camera With Raspberry Pi | Make:  

https://duckduckgo.com/?t=canonical&q=raspberry+pi+camera+high+quality&ia=web  
raspberry pi camera high quality at DuckDuckGo  
https://www.raspberrypi.org/products/raspberry-pi-high-quality-camera/?resellerType=home  
Buy a Raspberry Pi High Quality Camera – Raspberry Pi  
https://static.raspberrypi.org/files/product-guides/Raspberry_Pi_High_Quality_Camera_Getting_Started.pdf  
Raspberry_Pi_High_Quality_Camera_Getting_Started.pdf  
https://hackernoon.com/polising-raspberry-pi-high-quality-camera-3z113u18  
How to Configure a Raspberry Pi High Quality Camera | Hacker Noon  
https://www.raspberrypi.org/products/camera-guide/?resellerType=home  
Buy a The Official Raspberry Pi Camera Guide – Raspberry Pi  
https://duckduckgo.com/?t=canonical&q=raspberry+pi+with+camer+connector&ia=web  
raspberry pi with camer connector at DuckDuckGo  
https://www.raspberrypi.org/blog/zero-grows-camera-connector/  
Zero grows a camera connector - Raspberry Pi  



https://learn.sparkfun.com/tutorials/getting-started-with-the-raspberry-pi-zero-wireless/all  
Getting Started with the Raspberry Pi Zero Wireless - learn.sparkfun.com  

https://www.raspberrypi.org/  
Teach, Learn, and Make with Raspberry Pi  

https://duckduckgo.com/?t=canonical&q=raspberry+pi+camera+plus+pi+zero+wifi+setup+instructions&ia=web  
raspberry pi camera plus pi zero wifi setup instructions at DuckDuckGo  
  
https://www.arrow.com/en/research-and-events/articles/motioneyeos-camera-setup-on-raspberry-pi-zero-w  
MotionEyeOS Raspberry Pi Zero W Tutorial: Setup & Uses | Arrow.com | Arrow.com  
https://duckduckgo.com/?t=canonical&q=motioneyes+wifi&ia=web  
motioneyes wifi at DuckDuckGo  
https://github.com/ccrisan/motioneyeos/issues/42  
Setting up WiFi with MotionEye · Issue #42 · ccrisan/motioneyeos · GitHub  
https://github.com/ccrisan/motioneyeos/wiki  
Home · ccrisan/motioneyeos Wiki · GitHub  
https://github.com/ccrisan/thingos/wiki/wpa_supplicant.conf  
wpa_supplicant.conf · ccrisan/thingos Wiki · GitHub  
https://github.com/ccrisan/motioneyeos/issues/1850  
Can't acces MotionEyeOS IP when using WiFi · Issue #1850 · ccrisan/motioneyeos · GitHub  


## OS native features

https://www.raspberrypi.com/documentation/computers/os.html#using-a-usb-webcam

## Web cam setup

Try Jeff Geerling's webcam setup

https://github.com/geerlingguy/pi-webcam  
GitHub - geerlingguy/pi-webcam: Automation to configure a Raspberry Pi as a USB OTG webcam  
https://gist.github.com/justinschuldt/36469e2a89d95ef158a8c4df091e9cb4  
Directions for setting up a RaspberryPi to act as a generic USB webcam · GitHub  
https://newatlas.com/telecommunications/jeff-geerling-raspberry-pi-hq-camera-webcam/  
Pi Zero W and HQ camera module used as cheap webcam  
https://github.com/geerlingguy  
geerlingguy (Jeff Geerling) · GitHub  


https://duckduckgo.com/?t=ffab&q=geerlingguy+github&ia=web  
geerlingguy github at DuckDuckGo  

https://duckduckgo.com/?t=ffab&q=raspberry+pi+hq+camera+maximum+video+rate&ia=web  
raspberry pi hq camera maximum video rate at DuckDuckGo  
https://www.raspberrypi.org/documentation/raspbian/applications/camera.md  
Raspberry Pi Camera Module - Raspberry Pi Documentation  
https://duckduckgo.com/?t=canonical&q=use+raspberry+pi+hq+camera+as+web+camera&ia=web  
use raspberry pi hq camera as web camera at DuckDuckGo  


