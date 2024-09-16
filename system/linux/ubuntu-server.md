# Ubuntu Server

Focusing on a server, the requirements are a bit different. 
It doesn't matter if the server is physical or virtual.
Ubuntu even makes a server distribution tailored to Raspberry Pi's, so this is a good place to start for those. 

After installing Ubuntu Server, log in to the machine's console.

## See also

/home/account/combined/public/system/linux/ubuntu-server.md
/home/account/combined/public/system/linux/ubuntu.md
/home/account/combined/public/system/network.md
/home/account/combined/someplace/repositories.md


## SSHD

Best to enable SSH service so you can connect remotely. This is a nice, up-to-date write up of the different considerations for ssh configuration.

https://help.ubuntu.com/community/SSH/OpenSSH/Configuring

```
sudo apt-get install openssh-server
```

Then, SSH access should be on and available remotely

It might be a better idea to use SSH Keys and disable password authentication, especially on a publicly visible server

```
sudo vi /etc/ssh/sshd_config
```

From here, it probably makes sense to use configuration management. Documentation is okay too!


## Desktop

If you want a desktop GUI, you can add one:

    sudo apt-get install ubuntu-desktop

(This worked for me with a Raspberry Pi 4 with 4GB of memory and Ubuntu Server 20.04)
