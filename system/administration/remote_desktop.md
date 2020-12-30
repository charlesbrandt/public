# Remote Desktops

When [SSH](ssh.md) is not enough, there are different protocols that are used to connect to a desktop interface remotely. 


## Clients

### Remmina / Ubuntu 

A client that connects to most. Then choose the protocol that works best for your use case:

https://remmina.org/

sudo apt-get install remmina
[sudo] password for account: 
Reading package lists... Done
Building dependency tree       
Reading state information... Done
remmina is already the newest version (1.4.2+dfsg-1ubuntu1).

Installed by default on Ubuntu

### OS X

If connecting from OS X, use the built in VNC viewer / client: Screen Sharing.

### SSH Tunnels

If the remote desktop protocol is not encrypted by default (e.g. VNC), it's a good idea to establish an SSH tunnel to encrypt the connection (even on a local network):

    ssh <user>@<destination_ip> -L 5900:<destination_ip>:5900 -L 5901:<destination_ip>:5901
    

## Servers / Protocols

### VNC

VNC is free and open source and available by default on Ubuntu.

#### Quick Share (Ubuntu)

Go to settings –  click on sharing on the left panel – enable sharing using the slide switch

Click on Screen sharing (off by-default) – enable “Allow connections to control the screen“.

Select “Require a password” if you don’t want to manually approve connection on the remote computer. Set password as per your preference, you need to give it during remote authentication.

[via](https://cloudlinuxtech.com/enable-remote-desktop-ubuntu/)

!! NOTE !! 
This will only be enabled after the host has logged in to the desktop
Not a good option if you don't have a local keyboard / monitor to log in with

Two different ways to go about server configuration...

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

#### Separate Desktop

An alternative approach is to launch a new virtual desktop that is not available on the console. For this you'll want to use something like "tightvncserver".

    tightvncserver
    sudo ufw allow 5901

Consider if you need a full blown GUI in this case... it may be easier to just work at the command line over [SSH](ssh.md).

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

## RDP

Remote Desktop Protocol is another option. Originally from Windows, there is a server version that can be installed on Ubuntu


https://websiteforstudents.com/how-to-connect-via-remote-desktop-rdp-to-ubuntu-20-04-18-04/

