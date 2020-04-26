# Remote Desktops

There are different protocols that are used to connect to a desktop interface remotely. VNC is free and open source, but not very secure by default. 


## Setting up VNC 

Generally, it's a good idea to establish an SSH tunnel to encrypt the connection (even on a local network):

    ssh <user>@<destination_ip> -L 5900:<destination_ip>:5900 -L 5901:<destination_ip>:5901

If connecting from OS X, use the built in VNC viewer / client: Screen Sharing.

Two different ways to go about server configuration...


### Main Desktop (Shared)

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


### Separate Desktop

An alternative approach is to launch a new virtual desktop that is not available on the console. For this you'll want to use something like "tightvncserver".

    tightvncserver
    sudo ufw allow 5901

Consider if you need a full blown GUI in this case... it may be easier to just work at the command line.


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

To start this automatically, see below for init scripts to start on boot.

