# Keyboard

## Characters

Virtual keyboards on phones have built in access for emojis and special character selection. On a desktop, these characters can be harder to find

The app "Characters" will allow you to choose non-standard characters, like emojis, via linux. Had had some difficulty finding a way to do this in the past. 

This article helped:

https://www.maketecheasier.com/quickly-type-special-characters-linux/  
How to Quickly Type Special Characters in Linux - Make Tech Easier  


https://duckduckgo.com/?t=canonical&q=linux+insert+emoji&ia=web  
linux insert emoji at DuckDuckGo  
https://duckduckgo.com/?t=canonical&q=type+other+characters+linux&ia=web  
type other characters linux at DuckDuckGo  


## Show key press

https://duckduckgo.com/?t=ffab&q=linux+show+keycode&ia=web  
linux show keycode at DuckDuckGo  
https://superuser.com/questions/248517/show-keys-pressed-in-linux  
keyboard - Show keys pressed in Linux - Super User  
https://gitlab.com/screenkey/screenkey  
screenkey / screenkey · GitLab  

```
screenkey -p top
pkill -9 screenkey
```

## Remap Keys

Sometimes keyboards have a key that gets in the way, or deviates from other keyboard layouts.

https://askubuntu.com/questions/296155/how-can-i-remap-keyboard-keys

Sounds like `xmodmap` is an old way of mapping keys. As noted in the above thread, those settings don't stick around after a reboot. `xbd` seems to be the way to go for these. 

On my Dell XPS13 laptop, I want to remap the 'pgup' to be 'left' and 'pgdn' to be 'right'. 

    sudo vi /usr/share/X11/xkb/symbols/pc

```
    key <PGUP> {        [  Prior                ]       };
    key <PGDN> {        [  Next                 ]       };
```

```
    key <PGUP> {        [  Left                 ]       };
    key <PGDN> {        [  Right                ]       };
```

For mapping the menu key to be a control key on the perixx keyboard:

```
    modifier_map Control{ Control_L, Control_R, Menu };
```

Apply the changes with

    setxkbmap us 

    setxkbmap us intl 
    
This sets it up globally. In my case, that's what I want.

Looks like it's also possible to do this on a per user basis:

https://unix.stackexchange.com/questions/65507/use-setxkbmap-to-swap-the-left-shift-and-left-control/65600#65600

Other references on the topic:

https://duckduckgo.com/?t=ffab&q=ubuntu+remap+key&ia=web  
ubuntu remap key at DuckDuckGo  

https://web.archive.org/web/20170825051821/http://madduck.net/docs/extending-xkb/  
Extending the X keyboard map with xkb  
https://duckduckgo.com/?t=ffab&q=xkb+remap+keys&ia=web  
xkb remap keys at DuckDuckGo  
https://unix.stackexchange.com/questions/572768/remapping-a-key-for-xkb  
keyboard - Remapping a key for XKB - Unix & Linux Stack Exchange  
http://www.pixelbeat.org/docs/xkb_remap/  
XKB keyboard remapping  




