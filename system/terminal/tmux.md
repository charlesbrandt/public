# TMUX

A session / window manager for the console. Allows tracking tabs all within the same console. This is useful when connecting to remote servers, especially when connecting with something flakey like a phone. 

Install tmux on the remote server:

    sudo apt install tmux

from there you can launch `tmux` with

```
$ tmux
```

This is a good guide

https://tmuxcheatsheet.com/

Alternatively, these notes are less complete.  
Once inside a tmux context, it helps to know some basic commands to navigate. 

to detach

```
ctrl-b d # (or ctrl-i d if remapped)
```

to list all sessions

```
tmux ls
```

ctrl-b s # (or ctrl-i s if remapped)


to re-attach to the previous session

```
tmux attach
```

if the previous one is not the one you want:

```
tmux attach-session -t 5
```

to cycle through sessions from within tmux

ctrl-b (     # or ctrl-i ( if remapped  
ctrl-b )     # or ctrl-i ) if remapped  

to rename a session  
ctrl-b $     # or ctrl-i $ if remapped  


## Sessions, widows and panes

See the previously mentioned cheat sheet. 

Each Session contains a group of windows.

Each window is made up of one or more panes. 

On a small screen like on Android, no room for more than one pane, but sessions and windows help a lot.


## Windows

Ctrl + b p  
Previous window  

Ctrl + b n  
Next window  

Ctrl + b 0 ... 9  
Switch/select window by number


Ctrl + b c  
Create window  

Ctrl + b ,  
Rename current window  

Ctrl + b &  
Close current window  



## Configuration

Put this at the bottom of ~/.tmux.conf ($XDG_CONFIG_HOME/tmux/tmux.conf works too):

```
# remap prefix to Control + p
set -g prefix C-p
unbind C-b
bind C-p send-prefix
```

Control + m was used previously, but it overlaps with sending 'enter' in tmux, which results in needing to press enter twice. 
Not using ctrl-m resolves the issue where pressing enter requires a double press. 

Is ctrl-i used for tab key on the CLI. Using it as the bind key results in needing to press tab twice for auto-complete. 
Running out of options -- maybe ctrl-p? Similar to what is used in visual studio code



Other options

```
# force a reload of the config file
unbind r
bind r source-file ~/.tmux.conf

# quick pane cycling
unbind ^A
bind ^A select-pane -t :.+
```

## Script Startup

tmux can help manage the state needed for workspaces. In this case, create a script to configure the startup.

```
#!/bin/bash

session="team"

tmux new-session -d -s $session

window=0
tmux rename-window -t $session:$window 'launcher'
tmux send-keys -t $session:$window '' C-m

window=1
tmux new-window -t $session:$window -n 'vpn'
tmux send-keys -t $session:$window 'cd team; ./vpn.sh' C-m

session="dev"

tmux new-session -d -s $session

window=0
tmux rename-window -t $session:$window 'launcher'
tmux send-keys -t $session:$window 'cd ~/alpha/code/api' C-m
tmux send-keys -t $session:$window 'emacs instances-api.md &'

session="project"

tmux new-session -d -s $session

window=0
tmux rename-window -t $session:$window 'launcher'
tmux send-keys -t $session:$window 'cd ~/projects/project' C-m
tmux send-keys -t $session:$window 'emacs instances-project.md &'

window=1
tmux new-window -t $session:$window -n 'servers'
tmux send-keys -t $session:$window 'cd ~/projects/project/project-code/ui' C-m
tmux send-keys -t $session:$window 'pnpm run dev' C-m
# this didn't work in my first attempt -- should be close
#tmux split-window -t $session:$window 'dcp' C-m
# no parameters works
tmux split-window -t $session:$window 
tmux send-keys -t $session:$window 'dcu' C-m
tmux send-keys -t $session:$window 'dcp' C-m

window=2
tmux new-window -t $session:$window -n 'git'
tmux send-keys -t $session:$window 'cd ~/projects/project/project-code/ui' C-m
tmux send-keys -t $session:$window 'git status' C-m

tmux attach-session -t team

```


A good reference that got me going

https://how-to.dev/how-to-create-tmux-session-with-a-script  
How to create tmux session with a script  

https://unix.stackexchange.com/questions/292137/tmux-script-to-launch-several-commands  
scripting - tmux script to launch several commands - Unix & Linux Stack Exchange  

https://duckduckgo.com/?t=ffab&q=tmux+script+to+open+multiple+sessions&ia=web  
tmux script to open multiple sessions at DuckDuckGo  
https://stackoverflow.com/questions/58453982/spinning-up-multiple-tmux-sessions-in-bash  
Spinning up multiple tmux sessions in bash - Stack Overflow  


## Scrolling

Using a terminal multiplexer, you lose the ability to use your system terminal's built in scrolling mechanism, if any. On something like a phone, this is not an issue, but on desktop contexts it may be limiting? 

## See Also

https://www.ocf.berkeley.edu/~ckuehl/tmux/

## Resources

* [tmux: Productive Mouse-Free Development](http://pragprog.com/book/bhtmux/tmux)
* [How to reorder windows](http://superuser.com/questions/343572/tmux-how-do-i-reorder-my-windows)

## tmux shortcuts & cheatsheet

start new:

    tmux

start new with session name:

    tmux new -s myname

attach:

    tmux a  #  (or at, or attach)

attach to named:

    tmux a -t myname

list sessions:

    tmux ls

kill session:

    tmux kill-session -t myname

Kill all the tmux sessions:

    tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | xargs kill



In tmux, hit the prefix `ctrl+b` (my modified prefix is ctrl+m) and then:

## Misc

    d  detach
    t  big clock
    ?  list shortcuts
    :  prompt

## Sessions

    :new`<CR>`  new session
    s  list sessions
    $  name session

## Windows (tabs)

    c  create window
    w  list windows
    n  next window
    p  previous window
    f  find window
    ,  name window
    &  kill window

## Panes (splits) 

    %  vertical split
    "  horizontal split
    
    o  swap panes
    q  show pane numbers
    x  kill pane
    +  break pane into window (e.g. to select text by mouse to copy)
    -  restore pane from window
    ⍽  space - toggle between layouts
    `<prefix>` q (Show pane numbers, when the numbers show up type the key to goto that pane)
    `<prefix>` { (Move the current pane left)
    `<prefix>` } (Move the current pane right)
    `<prefix>` z toggle pane zoom

## Sync Panes 

You can do this by switching to the appropriate window, typing your Tmux prefix (commonly Ctrl-B or Ctrl-A) and then a colon to bring up a Tmux command line, and typing:

```
:setw synchronize-panes
```

You can optionally add on or off to specify which state you want; otherwise the option is simply toggled. This option is specific to one window, so it won’t change the way your other sessions or windows operate. When you’re done, toggle it off again by repeating the command. [tip source](http://blog.sanctum.geek.nz/sync-tmux-panes/)


## Resizing Panes

You can also resize panes if you don’t like the layout defaults. I personally rarely need to do this, though it’s handy to know how. Here is the basic syntax to resize panes:

    PREFIX : resize-pane -D (Resizes the current pane down)
    PREFIX : resize-pane -U (Resizes the current pane upward)
    PREFIX : resize-pane -L (Resizes the current pane left)
    PREFIX : resize-pane -R (Resizes the current pane right)
    PREFIX : resize-pane -D 20 (Resizes the current pane down by 20 cells)
    PREFIX : resize-pane -U 20 (Resizes the current pane upward by 20 cells)
    PREFIX : resize-pane -L 20 (Resizes the current pane left by 20 cells)
    PREFIX : resize-pane -R 20 (Resizes the current pane right by 20 cells)
    PREFIX : resize-pane -t 2 20 (Resizes the pane with the id of 2 down by 20 cells)
    PREFIX : resize-pane -t -L 20 (Resizes the pane with the id of 2 left by 20 cells)
    
    
## Copy mode:

Pressing PREFIX [ places us in Copy mode. We can then use our movement keys to move our cursor around the screen. By default, the arrow keys work. we set our configuration file to use Vim keys for moving between windows and resizing panes so we wouldn’t have to take our hands off the home row. tmux has a vi mode for working with the buffer as well. To enable it, add this line to .tmux.conf:

    setw -g mode-keys vi

With this option set, we can use h, j, k, and l to move around our buffer.

To get out of Copy mode, we just press the ENTER key. Moving around one character at a time isn’t very efficient. Since we enabled vi mode, we can also use some other visible shortcuts to move around the buffer.

For example, we can use "w" to jump to the next word and "b" to jump back one word. And we can use "f", followed by any character, to jump to that character on the same line, and "F" to jump backwards on the line.

       Function                vi             emacs
       Back to indentation     ^              M-m
       Clear selection         Escape         C-g
       Copy selection          Enter          M-w
       Cursor down             j              Down
       Cursor left             h              Left
       Cursor right            l              Right
       Cursor to bottom line   L
       Cursor to middle line   M              M-r
       Cursor to top line      H              M-R
       Cursor up               k              Up
       Delete entire line      d              C-u
       Delete to end of line   D              C-k
       End of line             $              C-e
       Goto line               :              g
       Half page down          C-d            M-Down
       Half page up            C-u            M-Up
       Next page               C-f            Page down
       Next word               w              M-f
       Paste buffer            p              C-y
       Previous page           C-b            Page up
       Previous word           b              M-b
       Quit mode               q              Escape
       Scroll down             C-Down or J    C-Down
       Scroll up               C-Up or K      C-Up
       Search again            n              n
       Search backward         ?              C-r
       Search forward          /              C-s
       Start of line           0              C-a
       Start selection         Space          C-Space
       Transpose chars                        C-t

## Configurations Options:

    # Mouse support - set to on if you want to use the mouse
    * setw -g mode-mouse off
    * set -g mouse-select-pane off
    * set -g mouse-resize-pane off
    * set -g mouse-select-window off

    # Set the default terminal mode to 256color mode
    set -g default-terminal "screen-256color"

    # enable activity alerts
    setw -g monitor-activity on
    set -g visual-activity on

    # Center the window list
    set -g status-justify centre

    # Maximize and restore a pane
    unbind Up bind Up new-window -d -n tmp \; swap-pane -s tmp.1 \; select-window -t tmp
    unbind Down
    bind Down last-window \; swap-pane -s tmp.1 \; kill-window -t tmp


## Cheatsheet

From here, it helps to learn some basics. Here are a few good guides:

https://thoughtbot.com/blog/a-tmux-crash-course

```
For all keybindings, press ctrl-b first, then press the key you want.
key 	what it does
ctrl-b, % 	split the screen in half from left to right
ctrl-b, " 	split the screen in half from top to bottom
ctrl-b, x 	kill the current pane
ctrl-b, `<arrow key>` 	switch to the pane in whichever direction you press
ctrl-b, d 	detach from tmux, leaving everything running in the background
```

This is an incomplete list; a more exhaustive list is available [here](https://gist.github.com/MohamedAlaa/2961058)

https://www.ocf.berkeley.edu/~ckuehl/tmux/





## Tiling Window Managers

There is a lot of conceptual overlap with tmux and tiling windows managers

[Window Managers]()

where are these notes?
wayland.txt

https://www.slant.co/topics/1902/~best-tiling-window-managers-for-linux  
https://www.reddit.com/r/i3wm/comments/66ho9z/why_you_use_tmux_when_already_using_i3/  
https://www.slant.co/options/1288/~awesome-review  
https://www.slant.co/options/1287/~xmonad-review  


## Launchers / dmenu

dmenu is a fast and lightweight dynamic menu for X. It reads arbitrary text from stdin, and creates a menu with one item for each line. The user can then select an item, through the arrow keys or typing a part of the name, and the line is printed to stdout. dmenu_run is a wrapper that ships with the dmenu distribution that allows its use as an application launcher
 
https://wiki.archlinux.org/index.php/dmenu
https://tools.suckless.org/dmenu/




# 2019.08.26 17:31:24 
getting started with learning tmux for managing command line sessions

this may be able to help when termux on android gets closed and the state is lost. 
# 2019.08.27 15:04:50 
tmux seems similar in functionality to emacs... 
multiple buffers showing on one terminal. 
but it's probably better to abstract that out of the editor

