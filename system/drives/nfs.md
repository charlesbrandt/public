# NFS (Network File System) 

Share & Mount Linux Drive with NFS

## Server

### Install

Install NFS Ensure that NFS is installed on both the server (the system with the drive to be shared) and the client (the system that will mount the shared drive). If NFS is not installed, you can install it using the package manager for your Linux distribution. For example, on Ubuntu, you can run the following command:

```sh
sudo apt-get update
sudo apt-get install nfs-kernel-server   # for server
``` 

### Configure

Configure NFS on the Server Edit the NFS server configuration file on the server. The configuration file is usually located at `/etc/exports`. Open the file with a text editor and add an entry for the drive or directory that you want to share. The entry should specify the client(s) that are allowed to access the shared drive or directory, along with the permissions. Here's an example entry:

```sh
/path/to/drive  <client-IP>(options)
``` 

Replace `/path/to/drive` with the actual path to the drive or directory you want to share, `<client-IP>` with the IP address of the client system, and `options` with the desired options for NFS. For example:

```sh
/path/to/drive  192.168.1.100(rw,sync,no_subtree_check)
```

This example allows the client with IP address 192.168.1.100 to have read and write (rw) access to the shared drive, with synchronization (sync) and no subtree check (no_subtree_check) options enabled. Save the configuration file after making changes.

### Restart

Restart NFS Server Restart the NFS server to apply the changes by running the following command:

```sh
sudo service nfs-kernel-server restart
```

### Firewall 

```
rpcinfo -p | grep nfs
```

https://serverfault.com/questions/377170/which-ports-do-i-need-to-open-in-the-firewall-to-use-nfs

```
sudo ufw allow 2049
sudo ufw allow 111
```

## Client

```
sudo apt-get update
sudo apt-get install nfs-common          # for client
````

### Mount

Mount the Shared Drive on the Client On the client system, create a directory where you want to mount the shared drive. For example:

```sh
sudo mkdir /mnt/shared_drive
``` 

Then, mount the shared drive from the server using the `mount` command. For example:

```sh
sudo mount <server-IP>:/path/to/drive /mnt/shared_drive
``` 

Replace `<server-IP>` with the IP address of the NFS server, and `/path/to/drive` with the path to the shared drive or directory on the server. The shared drive should now be mounted on the client system.

## fstab

Configure Auto-mount (Optional) If you want the shared drive to be automatically mounted at boot on the client system, you can configure it in the `/etc/fstab` file. Open the file with a text editor and add an entry for the shared drive in the following format:

```
<server-IP>:/path/to/drive /mnt/shared_drive nfs defaults 0 0
``` 

Save the `/etc/fstab` file after making changes. The shared drive will now be automatically mounted at boot on the client system.



That's it! You have successfully shared and mounted a drive on Linux using NFS.

Started with ChatGPT. Edited for structure and accuracy. 
