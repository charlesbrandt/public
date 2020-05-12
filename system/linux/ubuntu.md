# Ubuntu / Xubuntu / Ubuntu Studio

When running from an Ubuntu live session, I add some configurations with:
system/startup.sh
Be sure to update $NAME to be your name/username and $USBPATH to point to where this instance is.

For notes on downloading and creating an image, see:

    linux-distributions.txt

This guide picks up once a base operating system has been installed. (or live run)

If you're installing the system on a virtual machine, see here for notes on those configurations:

    (virtualization)[virtualization.txt]

If on a base / root / hypervisor type machine, install Virtual Box:

    virtualization-hypervisor.txt

If you encrypted your home drive, it's a good idea to note the key shown with "ecryptfs-unwrap-passphrase"

```
sudo apt-get update
sudo apt-get -y install openssh-server
sudo apt-get install -y python
ifconfig

ip address show
```

set up a shared directory with all machines. I use /c... use anything you like
```
export SHARED=/c
sudo mkdir -p /c
sudo chmod 770 /c
sudo chown -R charles: /c
```

Drag link to /c in File Manager

## Ansible

might be a good point to explore ansible here???

if you don't have ansible available elsewhere, install it now (ansible.txt)
if you do have ansible available, install the requirements to be a

https://github.com/city-of-bloomington/system-playbooks

if you have ansible installed on another virtual machine, can use that for the bootstrap. edit hosts.txt:

    cd ansible
    vi hosts.txt

then on control machine run:

    ansible-playbook system-2.yml -i hosts.txt --ask-pass --ask-become-pass



## Choose a Windows Manager

[Window Manager](window_managers/window_manager.md)

# Configs

TODO:
way to boot live with custom configs?

There are some general system settings that are good to take care of first:
(it may be possible to copy the ~/.configs and ~/.mozilla folder from a previous installation, but eventually those will grow out of sync with newer releases)

rm -rf ~/.config
cp -r /c/out-data/.config/ ~/.config

#this does not get startup or session items, but gets most other configurations

rm -rf ~/.mozilla
cp -r /c/out-data/.mozilla/ ~/.mozilla

#cp /media/charles/CHARLES/charles/.hgrc ~/
cp /c/charles/.hgrc ~/.hgrc

cp /c/charles/.gitconfig ~/.gitconfig

# to store passwords:
git config --global credential.helper store
# these are stored in plaintext, so it could be unsecure
# https://stackoverflow.com/questions/5343068/is-there-a-way-to-skip-password-typing-when-using-https-on-github



# This is a useful application for adjusting monitor brightness

sudo apt install ddccontrol gddccontrol ddccontrol-db i2c-tools

sudo gddccontrol

https://duckduckgo.com/?q=control+screen+brightness+desktop+monitor+linux&t=canonical&ia=qa
control screen brightness desktop monitor linux at DuckDuckGo
https://www.reddit.com/r/linux/comments/3gfa7z/desktop_monitor_brightness_control_script/
Desktop monitor brightness control script : linux
https://github.com/zeroping/ddccontrol-backlight
zeroping/ddccontrol-backlight: Bash script for linking ddccontrol to /sys/class/backlight for desktop monitors
https://duckduckgo.com/?q=ddcontrol&t=canonical&ia=web
ddcontrol at DuckDuckGo
https://github.com/ddccontrol/ddccontrol
ddccontrol/ddccontrol: DDC Control




### Application configurations:


set task manager default settings:
  - show all processes
  - sort by cpu utilization


*2015.11.22 12:29:31 terminal
only a subtle tweak has been required...
setting background to be the same color as emacs:
Change terminal profile settings"
"Edit"->"Profile Preferences..."
Under "Colors" tab,
For "Background color", use color picker to match emacs setting (#1D1F21)



#apply browser configurations sooner rather than later
#this way they'll be available for snapshots in virtual machines

#either from scratch (see browsers.txt)
#then copy browser settings for future installs:
cp -r ~/.mozilla $ARCHIVEPATH/.mozilla
cp -r ~/.mozilla /c/public/downloads/.mozilla

#or copy previous:
#cp -r /c/public/downloads/.mozilla ~/
#cp -r /media/charles/CHARLES/configs/.mozilla/ ~/.mozilla


launch Software Updater
Settings... -> Automatically check for updates: Every two weeks, (Never?)
(depends on the use case)
#16.04 adds new option to download and install security updates automatically
# going to try this out first to see how it impacts things


#GitHub:

  git config --global user.email "you@example.com"
  git config --global user.name "Your Name"

#screencasting

#via https://en.wikipedia.org/wiki/Comparison_of_screencasting_software:
#https://obsproject.com/download
sudo add-apt-repository ppa:obsproject/obs-studio
sudo apt-get update && sudo apt-get install obs-studio

#see also streaming.txt


# Install Javascript / Node / Vue
/c/charles/technical/javascript/node.txt

/c/charles/technical/javascript/vue/start-here.txt



# configure printer





Connect to a network. (Wireless, Wired, your choice!)
May need to press "Auto Etherenet" for wired connection

Add "Incomplete Language Support"


Install Audacity

sudo apt-get install audacity

sudo apt-get install mercurial

sudo apt-get install vlc

sudo apt-get install mixxx

sudo apt-get install inkscape

sudo apt-get install blender
sudo apt-get install gimp

cd /c/public
git clone https://github.com/charlesbrandt/templates
#previously:
git clone https://github.com/charlesbrandt/system
git clone https://github.com/charlesbrandt/moments

sudo apt-get install build-essential python3-dev

sudo apt-get install python3-pip
#or:
cd /c/public/moments
sudo python3 get-pip.py

sudo pip3 install uwsgi
sudo apt-get install nginx
#for notes on configuring, see sortable_list project
/c/public/sortable_list/nginx-sortable_list.conf

#to install local packages on python3, use:
sudo pip3 install -e .





# keyboard shortcut for timestamps (system wide!)

https://www.semicomplete.com/projects/xdotool/

   sudo apt-get install xdotool

https://unix.stackexchange.com/questions/36922/keyboard-shortcut-to-send-text-strings-to-program

   xdotool type 'text'

   xdotool type "$(date +'# %Y.%m.%d %H:%M')"

### 2018.11.04 00:25:29
see screenshots... this attempt doesn't work so far

https://duckduckgo.com/?q=xfce+keyboard+shortcut+output+text&t=canonical&ia=qa
xfce keyboard shortcut output text at DuckDuckGo
https://unix.stackexchange.com/questions/36922/keyboard-shortcut-to-send-text-strings-to-program
Keyboard Shortcut To Send Text Strings To Program - Unix & Linux Stack Exchange
https://www.semicomplete.com/projects/xdotool/xdotool.xhtml
404 Page not found - semicomplete
https://duckduckgo.com/?q=xdotool&t=canonical
xdotool at DuckDuckGo
https://www.semicomplete.com/projects/xdotool/
xdotool - fake keyboard/mouse input, window management, and more - semicomplete



TODO:
medley?

# go ahead with media application configurations next
ansible-playbook system-3.yml -i hosts.txt --ask-become-pass






















#don't forget these manual steps:

#back up downloaded files, if you want
export DATE=$(date +%Y%m%d)
rsync -av /home/charles/Downloads/ubuntu/ /c/downloads/ubuntu-$DATE

#to save space, you can clean up downloaded .deb files, however...
#keep downloaded directories around...
#they're used as checks by ansible to see if actions are required
#to save space and clean up downloaded files:

sudo find ~/Downloads/ubuntu/* -iname "*.deb" -exec rm \{\} \;


df -k
16.04x is ~ 4.1GB used at this point

#Snapshot here

Base Configuration

Includes:
 - manual configurations
 - ansible configurations (through system-2, up to media)
 - updates



df -k
16.04x is ~ 4.8GB used at this point

#No need for snapshots here....
#at this point you can start making clones for the common configurations




*2016.05.01 08:43:15
notes on installing virtual_box moved to virtualization.txt
(usually only need this for host machines)



#SAVE THIS FILE!

cd $USBPATH/technical
hg stat
hg add
hg ci -m "updates for new ubuntu system installation"
hg clone $USBPATH/technical $LOCALPATH/technical
cd $LOCALPATH/technical/system
python /c/mindstream/mindstream/launch.py -c /c/technical system




#If on a system with multiple operating systems, but you don't want
#ubuntu to be the default operating system, install startup manager:
sudo apt-get install -y -d startupmanager
mkdir startupmanager
sudo mv /var/cache/apt/archives/*.deb startupmanager
#remember to run startup manager after installation and choose the correct operating system


Archived
----------

Initially, it is a good idea to develop the following steps on a *live* or *virtual* instance in case you install something you didn't mean to.  Eventually things should stabilize enough that you can skip to the locally installed instance and go from there.

Try using 32bit os on virtual machines... sounds like that is more efficient from a memory stand point... shouldn't need to go over 4gb of ram in a VM anyway. However, docker requires 64bit to run in a VM, so that could be a good reason to stick with that.
