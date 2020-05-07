# XFCE

XFCE is a lightweight windows manager that is available in a number of different linux distributions. 


## Window Actions Button Layout

All Settings -> Window Manager
move window actions ("minimize", "close", etc) to a different position:
Settings Manager->Windows Manager
under the Style tab, change "Button layout"
X + _ Title v


## Styles

Use "Greybird". (It's the default in Xubuntu.)

System Tools -> Window Manager -> Style -> Greybird

The colors on Pills get washed out in some contexts. 


## Shortcuts

Stay in Window Manager settings
Under the "Keyboard" tab, clear the window operations menu setting

Then, under All Settings -> Keyboard settings -> Application Shortcuts find:
xfce4-popup-whiskermenu
(xfce4-appfinder also works, but requires tab to select)
and set that to Alt+Space or Windows/Super key

Create a new shortcut ("+Add") with the command 'xflock4', and the shortcut "Shift+Ctrl+Delete"
This is to be more in line with the way lock screen works on Macs. Not my first choice in combinations, but unfortunately Macs are not as configurable


## Workspaces

Settings -> Workspaces
Add workspaces (start with 4 - 10)

Optionally, add workspace switcher to Panel
Settings -> Panel -> Items -> Add (right icon) -> Workspace Switcher (bottom of list)


## Display / Backgrounds

For multiple monitors, configure them in Settings -> Display

Change Desktop background / wallpaper.
Open Settings from menu -> Desktop
Start->Settings Manager->Desktop

    Style: None
    #7C7C7C ! (or lighter: #C7C7C7)

if you choose a picture here, it will show up on the login screen!


## Clock

Set clock, including time format. Enable Date and Month in menu bar.
%a *%Y.%m.%d %H:%M


## Terminal 

be sure a Terminal is already open, then:
Settings -> Session and Startup -> Session -> Save Session


## Startup 

to start Task Manager automatically requires a different tactic...
save session will not pick it up.

Settings -> Session and Startup -> Application Autostart
Add a new one:
Task Manager
xfce4-taskmanager


## Launcher

Clean up favorite applications in the launcher (xfce4-popup-whiskermenu):
  - remove old favorites
  - add new:

Can also consider adding favorites to the desktop


Add "Places" to panel so that recent items can be cleaned up / removed:

"I fiured this out.
Add "places" to your panel. It has the function to clear out all recent documents."

via:
https://duckduckgo.com/?q=xfce+clear+recent+files&t=canonical&ia=web
xfce clear recent files at DuckDuckGo
https://forum.xfce.org/viewtopic.php?id=7163
[Solved] How to delete recent documents in xfce? / General discussion / Xfce Forums
https://www.linuxquestions.org/questions/slackware-14/recent-files-in-xfce-711915/
Recent files in XFCE

