# Ubuntu

```
which git
sudo apt-get install git
```

[Generate ssh-keys](../terminal/ssh.md)  
Install an ssh server if you plan to connect to the machine remotely  

Clone any repositories that you plan to make use of locally

```
mkdir combined
cd combined
git clone https://gitlab.com/charlesbrandt/public.git
```

Edit `/etc/hosts`

If the system requires a static IP, update that now. [network](../network.md)

[Apply Gnome 3 settings](window-managers/gnome3.md) (for things like trackpad settings, screen brightness, etc)

Change the Terminal background.   
  - Preferences -> Profiles -> Unnamed -> Colors"  
  - Uncheck "Use colors from system theme"  
  - Set the default terminal height to be the full height of your desktop  
    (Initial terminal size)

Change the desktop background. (right-click on the desired image in a file explorer and choose "Set as Desktop Background.")

Launch and configure your [browser](../browsers.md)  

Launch the System Monitor. Select to see all processes.  

Install a [password manager](../password-manager.md)

```
sudo apt install keepassxc
```

Install your preferred [editor](../editors/)  

Install and configure [tmux](../terminal/tmux.md)  



Install [Docker / Docker-compose](../virtualization/docker.md)  


[Install other applications, as needed.](../applications.md)

