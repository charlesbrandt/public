# VirtualBox 

I have found the version from Oracle to be easier to use. They are also more up to date. Although some aspects are not open, I feel comfortable enough having the open version available. 

Download both platform package and Extension Pack. (2 separate pages for this):

    https://www.virtualbox.org/wiki/Downloads

```
cd ~/Downloads
sudo dpkg -i virtualbox-5.
sudo dpkg -i virtualbox-5.1_5.1.12-112440~Ubuntu~xenial_amd64.deb
```

Lauch VirtualBox
Next manually install Extension Pack from within the application
 
File->Preferences->Extensions

File->Preferences->Network
Add NatNetwork

Use the main UI toolbar for the "Global Tools" button on the right.
Add Host Only Networks


if common build utilities have not been added already, add them now

```
sudo apt-get install -y gcc make perl
sudo /sbin/vboxconfig
```

Add any machines.




