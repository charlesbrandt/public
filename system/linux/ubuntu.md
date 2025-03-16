# Ubuntu

```
which git
sudo apt-get install git curl
```

[Generate ssh-keys](../terminal/ssh.md)  
Install an ssh server if you plan to connect to the machine remotely  

Clone any repositories that you plan to make use of locally

```
mkdir combined
cd combined
git clone https://gitlab.com/charlesbrandt/public.git
```

If the system requires a static IP, configure now: [network](../network.md)

Install your preferred [editor](../editors/)  
  - [micro](../editors/micro/index.md)

Edit `/etc/hosts`

[Apply Gnome 3 settings](window-managers/gnome3.md) (for things like trackpad settings, screen brightness, etc)
Change the desktop background. (right-click on the desired image in a file explorer and choose "Set as Desktop Background.")

Change the Terminal background.   
  - Preferences -> Profiles -> Unnamed -> Colors"  
  - Uncheck "Use colors from system theme"  
  - Set the default terminal height to be the full height of your desktop  
    (Initial terminal size)

Update your `.bashrc`

```
cp public/system/linux/.bashrc .bashrc
```

Launch and configure your [browser](../browsers.md)  

Launch the System Monitor. Select to see all processes.  

Install a [password manager](../password-manager.md)

```
sudo apt install keepassxc
```

Search utilities
[search](../../search.md)

Install [Docker / Docker-compose](../virtualization/docker.md)  

[Install other applications, as needed.](../applications.md)


