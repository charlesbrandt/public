# Remote Desktops

When [SSH](./terminal/ssh.md) is not enough, different protocols are available to connect to a desktop interface remotely. 

## Clients

### Remmina / Ubuntu 

Available by default on Ubuntu

A client that connects to most. Then choose the protocol that works best for your use case:

https://remmina.org/


If you need to install

```
sudo apt-get install remmina
[sudo] password for account: 
Reading package lists... Done
Building dependency tree       
Reading state information... Done
remmina is already the newest version (1.4.2+dfsg-1ubuntu1).
```

to pull configs from another machine:

```
cd 
rsync -av account@office:/home/account/.local/share/remmina/ .local/share/remmina/
```

### OS X

If connecting from OS X, use the built in VNC viewer / client: Screen Sharing.





## Servers / Protocols

### SSH Tunnels

Unless you're already working over an encrypted connection like VPN, it's a good idea to establish an SSH tunnel first before connecting to a remote desktop. 

If the remote desktop protocol is not encrypted by default (e.g. VNC), it's a good idea to establish an SSH tunnel to encrypt the connection (even on a local network):

```
ssh <user>@<destination_ip> -L 5900:<vnc_server_ip>:5900 -L 5901:<another_vnc_ip>:5901
```

Note: the `vnc_server_ip` is proxied via the `destination_ip` machine, so it's relative to that. 

Sometimes it's helpful to use `localhost` -- this proxies to the localhost of the destination_ip, which may get around firewall issues if the destination_ip proxy traffic gets routed through firewalls:

```
ssh <user>@<destination_ip> -L 5900:localhost:5900 
```


Remmia allows configuring an SSH tunnel in the client. This may work for you. 

If it doesn't it's also possible to configure Remmia to launch and manage the ssh tunnel via a command on your behalf:

https://kgibran.wordpress.com/2019/03/13/remmina-rdp-ssh-tunnel-with-pre-and-post-scripts/

### Rule of thumb

If you have a monitor and keyboard and mouse attached to the system, use built in screen sharing option. 

If no local desktop session is active via a monitor, use either RDP or tigervnc to create a new desktop session for you. In my experiments so far, RDP did not like having an active session running for a long time. It became very sluggish. Logging out and logging back in fixed the issue. 

### RDP

Remote Desktop Protocol

RDP may be a more efficient protocol. Does not appear to send the whole screen as an image? It may not require the user to be logged in already. 

Originally from Windows, there is a server version that can be installed on Ubuntu


https://websiteforstudents.com/how-to-connect-via-remote-desktop-rdp-to-ubuntu-20-04-18-04/

```
sudo apt update

sudo apt install -y xrdp
sudo systemctl enable xrdp
```

then to see if it's running

```
sudo systemctl status xrdp
```

Logout of any current Ubuntu desktop sessions. You can’t be logged in to Ubuntu while connecting via Xrdp. This is not a good option for active desktops (e.g. keyboard and mouse attached running active processes. For that use Quick Share.

To get passed the color issue, may need to provide authorization via password up to 4 times (!).

create a file (called 02-allow-colord.conf) in /etc/polkit-1/localauthority.conf.d/ and populated with the following content 

     
```
polkit.addRule(function(action, subject) {
  if ((action.id == "org.freedesktop.color-manager.create-device" ||
  action.id == "org.freedesktop.color-manager.create-profile" ||
  action.id == "org.freedesktop.color-manager.delete-device" ||
  action.id == "org.freedesktop.color-manager.delete-profile" ||
  action.id == "org.freedesktop.color-manager.modify-device" ||
  action.id == "org.freedesktop.color-manager.modify-profile") &&
  subject.isInGroup("{users}")) {
    return polkit.Result.YES;
  }
});
```

Summary: If the user belong to the group “users” then the create,modify profile and color device can then be performed without authentication prompt.

About the issue:

https://c-nergy.be/blog/?p=12073

The desktop environment is slightly different
having difficulty getting the dock to show. Maybe it doesn't matter? Sending all keys via keyboard allows most functionality

try restarting the machine after peripherals unplugged. Still able to access? Yes! Nice.



### VNC

VNC is free and open source and available by default on Ubuntu.

#### Quick Share (Ubuntu)

Quick share is available, but requires logging in every time. 

If you don't have a monitor and keyboard attached to the machine, this is a problem. 

Take a second to set up tightvncserver if you plan to use this machine headless.

However, if it is a machine that will always have a keyboard and mouse attached, Quick Share is perfect!

Go to settings –  click on sharing on the left panel – enable sharing using the slide switch

Click on Screen sharing (off by-default) – enable “Allow connections to control the screen“.

Select “Require a password” if you don’t want to manually approve connection on the remote computer. Set password as per your preference, you need to give it during remote authentication.

[via](https://cloudlinuxtech.com/enable-remote-desktop-ubuntu/)

Don't enable it on the network. This way an SSH tunnel must be established first. That is, the vpn service is only available via localhost. If you can get on there, you're in. 

!! NOTE !! 
This will only be enabled after the host has logged in to the desktop
Not a good option if you don't have a local keyboard / monitor to log in with

#### Main Desktop (Login Server)

TODO: This looks like the same scenario as the native Ubuntu screen sharing option (above) where remote connections are only available after local login. 

To attach to the existing (main, console) desktop, you'll want to use "x11vnc":

    sudo apt-get install x11vnc

    x11vnc -storepasswd

    x11vnc -forever -bg -usepw -display :0

To start x11vnc automatically at login, create a script:

    echo "/usr/bin/x11vnc -forever -bg -usepw -display :0" > ~/x11vnc.sh
    chmod +x ~/x11vnc.sh

Then use the desktop's GUI to add the automatic startup:

    Start -> Settings -> Session and Startup -> Application Autostart -> Add

Be sure to specify the full path to the script:

    /home/username/x11vnc.sh

via:
http://www.olij.co.uk/whitenoise/set-up-vnc-with-a-ubuntu-server-and-mac-client-simple-ssh-tunnel/


#### Separate Virtual Desktop

When running headless, RDP may be a more efficient option. Otherwise, if you really want VNC... 

An alternative approach is to launch a new virtual desktop that is not available on the main display when a monitor is plugged in. For this you'll want to use something like "tightvncserver".

    tightvncserver
    sudo ufw allow 5901

Consider if you need a full blown GUI in this case... it may be easier to just work at the command line over [SSH](./terminal/ssh.md).

To start tightvncserver at boot, you'll need an init script

```
#!/bin/sh -e
### BEGIN INIT INFO
# Provides:          vncserver
# Required-Start:    networking
# Default-Start:     3 4 5
# Default-Stop:      0 6
### END INIT INFO

PATH="$PATH:/usr/X11R6/bin/"

# The Username:Group that will run VNC
export USER="mythtv"
#${RUNAS}

# The display that VNC will use
DISPLAY="1"

# Color depth (between 8 and 32)
DEPTH="16"

# The Desktop geometry to use.
#GEOMETRY="<WIDTH>x<HEIGHT>"
#GEOMETRY="800x600"
GEOMETRY="1024x768"
#GEOMETRY="1280x1024"

# The name that the VNC Desktop will have.
NAME="my-vnc-server"

OPTIONS="-name ${NAME} -depth ${DEPTH} -geometry ${GEOMETRY} :${DISPLAY}"

. /lib/lsb/init-functions

case "$1" in
start)
log_action_begin_msg "Starting vncserver for user '${USER}' on localhost:${DISPLAY}"
su ${USER} -c "/usr/bin/vncserver ${OPTIONS}"
;;

stop)
log_action_begin_msg "Stoping vncserver for user '${USER}' on localhost:${DISPLAY}"
su ${USER} -c "/usr/bin/vncserver -kill :${DISPLAY}"
;;

restart)
$0 stop
$0 start
;;
esac

exit 0
```

It is possible to launch the VNC server when logging in to the desktop (see below), but this gets confusing if you decide to run multiple desktop instances. 



