# Streaming

With streaming there are 3 main parts:

  - source
    this includes things like OBS, Darkice, or other software that generates that stream to be served
  - server
    this is the machine / software that relays / serves the source material to clients
  - client
    applications that will play the stream


## Snapcast

https://github.com/badaix/snapcast/
https://home-assistant.io/blog/2016/02/18/multi-room-audio-with-snapcast/

### Installation

```
sudo apt-get install build-essential
sudo apt-get install libasound2-dev libvorbisidec-dev libvorbis-dev libflac-dev alsa-utils libavahi-client-dev avahi-daemon

cd ~/Downloads
git clone https://github.com/badaix/snapcast.git
cd snapcast
git submodule update --init --recursive

make

sudo make installserver
sudo make installclient
```

see also:
https://github.com/badaix/snapcast/blob/master/doc/build.md#linux-native

for configuration, see `/etc/default/snapserver`

Be sure to allow traffic on port 1704 and 1705 to the server if you want to access the stream outside of the local network

follow the testing guide (it works!)

    sudo cat /dev/urandom > /tmp/snapfifo

Then choose your source and configure accordingly:

https://github.com/badaix/snapcast/blob/master/doc/player_setup.md#pulseaudio


 -- Having difficulty getting live recorded audio to work well
 
pacmd load-module module-pipe-sink file=/tmp/snapfifo sink_name=Snapcast rate=48000
pacmd update-sink-proplist Snapcast device.description=Snapcast

arecord -r 48000 -c 1 -f S16_LE > /tmp/snapfifo

parec >/tmp/snapfifo

some stuttering happening....
might be all I have on this topic for now




## OBS open_broadcasting_system

via https://en.wikipedia.org/wiki/Comparison_of_screencasting_software:
https://obsproject.com/download

    sudo add-apt-repository ppa:obsproject/obs-studio
    sudo apt-get update && sudo apt-get install obs-studio

for local streaming, set up a rtmp server
if nginx is already installed, may be good to go...

be sure to update sources.list and uncomment lines for source:
this fixes the error:
You must put some 'source' URIs in your sources.list

    sudo vi /etc/apt/sources.list

Always a good idea before you start doing apt commands:

    sudo apt-get update && sudo apt-get upgrade
    sudo apt-get autoremove

Grab any build dependencies, this will install all required packages automatically:

    sudo apt-get build-dep nginx
    
We'll need git to grab the RTMP module later, and fakeroot for the .deb build

    sudo apt-get install git fakeroot
    
Make a directory for our build of nginx:

    cd /c/downloads
    mkdir nginx && cd nginx

Get the source to the nginx package

    apt-get source nginx

Enter the modules directory for this build of nginx.  Since the build number will vary, use the wildcard:

    cd nginx*/debian/modules/

Clone the git repo for the rtmp module, this copies its source into the Ubuntu nginx sources:

    git clone https://github.com/arut/nginx-rtmp-module.git

Go back to the top of the nginx source code

    cd ../../

next edit debian/rules:

    vi debian/rules

look for extras_configure_flags in the file. Add the line:

    --add-module=$(MODULESDIR)/nginx-rtmp-module

change the version number
(prevents the “stock” nginx from overwriting your custom RTMP enabled version)
for example:

```
nginx (1.10.0-0ubuntu0.16.04.4-ccb) xenial-security; urgency=medium

  * ENABLED RTMP:
     https://www.withoutthesarcasm.com/installing-nginx-rtmp-module-ubuntu-dpkg/

 -- Charles Brandt <example@example.com>  Sat, 06 May 2017 18:35:14 -0500
```

then build it!:

    dpkg-buildpackage -b

install any previously installed versions:

    sudo apt-get remove nginx nginx-core


then install it:

    cd ../
    sudo dpkg -i nginx-common_*.deb
    sudo dpkg -i nginx-extras_*.deb

next add configurations (/etc/nginx/nginx.conf is main file)

via:
https://www.withoutthesarcasm.com/installing-nginx-rtmp-module-ubuntu-dpkg/

https://obsproject.com/forum/resources/how-to-set-up-your-own-private-rtmp-server-using-nginx.50/




## icecast darkice audio_only

Need *BOTH* icecast2 to serve, and darkice to record local audio

    sudo apt-get install icecast2

can skip config if you have local settings configured already
or config screen walks through some settings (but may not know the right ones yet)

    sudo apt-get install darkice


if you skipped configuration previously either edit it manually:

    sudo vi /etc/icecast2/icecast.xml
    #sometimes a password there... be careful with that

 or copy it in:

    sudo cp /c/charles/system/icecast.xml /etc/icecast2/icecast.xml


Make sure the user you run it as can access the log directory (currently):

    mkdir /c/out/icecast2

Then run as an unprivileged user (root a bad idea) (this is a good way to test):
    icecast2 -c icecast.xml

start and stop service with:
    sudo /etc/init.d/icecast2 start
    sudo /etc/init.d/icecast2 stop

many more settings for darkice necessary... ideally you have a saved copy of
the configurations you prefer

don't need to copy those in to a server location...
better to run darkice manually when you want to stream

try running with:

    sudo darkice -c /c/charles/system/darkice.cfg

The tricky part was configuring the sound settings in Volume Control (I think that is 'pavucontrol'). Be sure that darkice is set to "pulse" in the config, and then it should show up under the "Recording" tab. From there you'll probably need to change it to the right source.

    ipconfig

Then, using a streaming client (VLC works), connect to the address:
    http://192.168.2.107:8000/stream.mp3


be sure to update ip addresses in the config.
stopping screencast here to fix the issue. (feel free to document further)

# if you need to start fresh:

    #gunzip /usr/share/doc/darkice/examples/darkice.cfg.gz
    #cp /usr/share/doc/darkice/examples /c/charles/system/darkice.cfg


via:
http://askubuntu.com/questions/28496/how-do-i-setup-an-icecast-server-for-broadcasting-audio-in-my-network
