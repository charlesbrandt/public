# Samba

When you want a to make a drive available on the network, Samba is a good solution that works with most other operating systems. 

```
sudo apt-get update

sudo apt-get install samba smbclient
```

Check if the service is running with 

```
sudo systemctl status smbd
```

Update firewall to allow Samba connections

```
sudo ufw allow 'Samba'
```

## Shares

Edit `/etc/samba/smb.conf`

```
sudo vi /etc/samba/smb.conf 
```

Add a section for each share

```
[Home]
   comment = Camera Feeds
   path = /media/account/CAMERAS
   guest ok = no
   browseable = no
   create mask = 0600
   directory mask = 0700
```

Then restart `samba`

```
sudo service smbd restart && sudo service smbd status
```

https://www.techrepublic.com/article/how-to-set-up-quick-and-easy-file-sharing-with-samba/

Allowing "Guest" will not require a password. Anyone on the local network can connect to the share. 

If you want to use a standard user account to connect to them, you'll need to assign a samba password to the account:

```
sudo smbpasswd -a account
```

It's possible to configure the shares via finder. Right click on the folder / drive and go to either "Properties -> Sharing tab" or "Local Network Share". This is useful when testing a setup. However, these settings will not persist across reboots. 

### See Also

 https://linuxhint.com/share-folder-on-local-network-with-ubuntu/


## Backup Configs

```
sudo cp /etc/samba/smb.conf /etc/samba/smb.conf.orig
sudo cp ~/previous/system/smb.conf /etc/samba/smb.conf 

sudo /etc/init.d/samba reload
```


## Mounting

```
sudo mount -t cifs //192.168.1.234/data ~/shared-drive
```

Or, in finder / files browser use:

Other Locations -> Connect to Server -> smb://192.168.1.234
