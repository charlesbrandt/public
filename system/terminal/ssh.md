# SSH

SSH allows accessing a remote computer via a "secure shell". This provides a command line interface to a remote system that is accessible via your network. 

    ssh username@machine_address
    
It's usually best to configure public key authentication (and even disable password authentication).


    Key pair is created (typically by the user). Typically:
    ssh-keygen

    Private key stays with the user (and only there), while the public key is sent to the server. Typically with the ssh-copy-id utility.
    Server stores the public key (and "marks" it as authorized).
    Server will now allow access to anyone who can prove they have the corresponding private key.

via:
https://www.ssh.com/ssh/public-key-authentication

Use ssh-copy-id to transfer the public key to the machine you want to be able to log in to:

    ssh-copy-id user@hostname.example.com

Or, if you're adding the id to a remote service like [Github](https://github.com/settings/keys), show the public key:

    cat .ssh/id_rsa.pub 


## SSH Server (SSHD)

If the machine doesn't have an ssh server running, you can install one:

```
sudo apt-get update
sudo apt-get -y install openssh-server
```


To find out ip

```
ip a
```
    
On older systems it was:

```
ifconfig
```

To enable sshd on raspberry pi os:

Enter `sudo raspi-config` in a terminal window

Select Interfacing Options

Navigate to and select SSH

Choose Yes

Select Ok

Choose Finish

via:  
https://www.raspberrypi.com/documentation/computers/remote-access.html


## Known Hosts

    ssh-keygen -R hostname
    
via https://askubuntu.com/questions/20865/is-it-possible-to-remove-a-particular-host-key-from-sshs-known-hosts-file


## Migrating keys

If you migrate keys from one machine to another, be sure to update the ~/.ssh directory to have the correct permissions:

```
mkdir ~/.ssh
chmod 700 ~/.ssh
chmod 600 ~/.ssh/*
```

https://stackoverflow.com/questions/44250002/how-to-solve-sign-and-send-pubkey-signing-failed-agent-refused-operation


## SSH Agent Forwarding

It's possible to use your local keys on machines that you connect to with

    ssh -A
    
Make sure you're running sshagent

    ps -aux | grep sshagent

https://developer.github.com/v3/guides/using-ssh-agent-forwarding/


## SSH Port Forwarding

aka simple VPN.

Encrypts only the ports that you request to forward. 

local port : remote address : remote port (??? TODO: confirm)

ssh account@address -L 5900:192.168.2.81:5900

For example, VNC often uses 5900


## SSH Key Conversion

If a user sends a public key in an SSH2 text format, convert it to the form expected in `authorized_keys`:

```
ssh-keygen -i -f publickey 
```

https://serverfault.com/questions/380712/ssh-public-key-format
