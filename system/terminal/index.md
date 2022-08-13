# Terminal

```
cd
rm .bashrc
ln -s ~/public/system/.bashrc .bashrc
```


## Alias vs Functions

interesting discussion came up here:  
~/public/system/virtualization/docker.md

Which is better, an alias or a function? 

#### Function

```
dew() {
  docker-compose exec web $@
}
```

The $@ part handles passing along all of the function’s arguments. 

https://duckduckgo.com/?q=docker-compose+shortcut&t=ffab&ia=web
docker-compose shortcut at DuckDuckGo
https://stackoverflow.com/questions/40429508/shortcut-command-for-docker-compose-build-down-up
Shortcut command for docker-compose build + down + up - Stack Overflow
https://nickjanetakis.com/blog/docker-tip-26-alias-and-function-shortcuts-for-common-commands
Docker Tip #26: Alias and Function Shortcuts for Common Commands — Nick Janetakis

## See also

<a href=".bashrc">.bashrc</a>

[tmux](tmux.md)

## Timing a process

just prefix the command you want to time with `time`!

```
time ls
```

https://duckduckgo.com/?t=ffab&q=time+how+long+a+script+takes+bash&ia=web  
time how long a script takes bash at DuckDuckGo  
https://stackoverflow.com/questions/37695776/bash-calculate-how-long-it-takes-a-script-to-run  
Bash: Calculate how long it takes a script to run - Stack Overflow  

## Environment Variables

Often need to append to the `PATH` so a new executable is matched via the CLI. Should not put in `.bashrc` according to:

https://unix.stackexchange.com/questions/26047/how-to-correctly-add-a-path-to-path

I just put it in `.bashrc` :P

```
PATH=$PATH:~/.local/bin
```

No need for `export ` if the variable already exists

## history

The `history` command is a useful way to see what commands have been run recently. If you want to document the steps taken, it helps to remove line numbers first:

```
history | cut -c 8-
```

via:  
https://stackoverflow.com/questions/7110119/bash-history-without-line-numbers  


## Tools

https://github.com/webpro/awesome-dotfiles  
GitHub - webpro/awesome-dotfiles: A curated list of dotfiles resources.  
https://github.com/ibraheemdev/modern-unix  
GitHub - ibraheemdev/modern-unix: A collection of modern/faster/saner alternatives to common unix commands.  
https://github.com/topics/tools

## Emulators

https://itsfoss.com/tilix-terminal-emulator/  
Tilix: Advanced Tiling Terminal Emulator for Power Users - It's FOSS  
https://github.com/gnunn1/tilix/  
gnunn1/tilix: A tiling terminal emulator for Linux using GTK+ 3  
https://duckduckgo.com/?t=ffab&q=Tilix&ia=web  
Tilix at DuckDuckGo  
https://gnunn1.github.io/tilix-web/  
Tilix: A tiling terminal emulator  


https://duckduckgo.com/?t=ffab&q=best+tiling+terminal+console&ia=web  
best tiling terminal console at DuckDuckGo  
https://itsfoss.com/linux-terminal-emulators/  
14 Best Linux Terminal Emulators With Extra Features [2021]  
https://github.com/topics/terminal-emulators  
terminal-emulators · GitHub Topics  
https://github.com/topics/terminal  
terminal · GitHub Topics  
https://github.com/nicolargo/glances  
nicolargo/glances: Glances an Eye on your system. A top/htop alternative for GNU/Linux, BSD, Mac OS and Windows operating systems.  
https://github.com/railsware/upterm  
railsware/upterm: A terminal emulator for the 21st century.  
https://github.com/yudai/gotty  
yudai/gotty: Share your terminal as a web application  
https://github.com/Eugeny/tabby  
Eugeny/tabby: A terminal for a more modern age (formerly Terminus)  
https://conemu.github.io/  
ConEmu - Handy Windows Terminal  
https://github.com/GitSquared/edex-ui  
GitSquared/edex-ui: A cross-platform, customizable science fiction terminal emulator with advanced monitoring & touchscreen support.  
https://github.com/alacritty/alacritty  
alacritty/alacritty: A cross-platform, OpenGL terminal emulator.  
https://github.com/vercel/hyper  
vercel/hyper: A terminal built on web technologies  



## zsh

https://github.com/ohmyzsh/ohmyzsh  
ohmyzsh/ohmyzsh: 🙃 A delightful community-driven (with 1900+ contributors) framework for managing your zsh configuration. Includes 300+ optional plugins (rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up your morning, and an auto-update tool so that makes it easy to keep up with the latest updates from the community.  
https://www.zsh.org/  
Zsh  
https://ohmyz.sh/  
Oh My Zsh - a delightful & open source framework for Zsh  
https://duckduckgo.com/?t=ffab&q=why+zsh&ia=web  
why zsh at DuckDuckGo  
https://www.howtogeek.com/362409/what-is-zsh-and-why-should-you-use-it-instead-of-bash/  
What is ZSH, and Why Should You Use It Instead of Bash?  

https://github.com/romkatv/powerlevel10k  
GitHub - romkatv/powerlevel10k: A Zsh theme  
https://github.com/Powerlevel9k/powerlevel9k  
GitHub - Powerlevel9k/powerlevel9k: Powerlevel9k was a tool for building a beautiful and highly functional CLI, customized for you. P9k had a substantial impact on CLI UX, and its legacy is now continued by P10k.  
