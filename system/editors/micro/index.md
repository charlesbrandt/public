# Micro

Lightweight cli terminal based editor. 

```
sudo apt-get install micro
cd ~/.config
ln -s ~/public/system/editors/micro/micro.config micro
```

A worthy alternative to emacs, vi, and nano.

## Wrap

What about word wrapping?
How to enable? 

run the command:

```
set softwrap true
```

`set` sets the option to value. See the options help topic for a list of options you can set. This will modify your `settings.json` with the new value.

Or, if you want to edit the file directly, in the settings file (`~/.config/micro/settings.json`), enable the softwrap option:

```
{
    "softwrap": true
}
```

There is also `wordwrap`, but it requires `softwrap` which seems to do what I'm after

https://github.com/zyedidia/micro/blob/master/runtime/help/options.md

## Line Numbers

Toggle line numbers on and off quickly with `ctrl-r`

This is available by default. The binding looks like:

```
    "Ctrl-r":          "ToggleRuler",
```

same as

```
set ruler true|false
```

## Navigation / Moving Cursor

Jump to next blank line
By default "ctrl-up" and "ctrl-down" jump to the beginning and the end of the document / buffer. 
I prefer when they jump to the next blank line instead. 
```
    "CtrlUp": "ParagraphPrevious",
    "CtrlDown": "ParagraphNext",
```

Built-in, Default: There is also emacs mode if you're willing to switch over to using Alt for navigation:
```
    "Alt-a": "StartOfLine",
    "Alt-e": "EndOfLine",
    "Alt-f": "WordRight",
    "Alt-b": "WordLeft",
```

This is difficult to let go of, especially since these bindings work on the shell. 
Custom: These have existing, default equivalents ("Alt-right-arrow" and "Alt-left-arrow"), but if command execute is mapped to ctrl-p, it's available:

```
    "Ctrl-e": "EndOfLine",
    "Ctrl-a": "StartOfTextToggle",
```

## Find / Search

Find is available via `ctrl-f`, but you must complete your first search by pressing `enter` before you can use `ctrl-n` to findNext.

## Buffers / Tabs

Buffer switching?

Doesn't seem to be the same as emacs here. There is only the concept of tabs.

Remember: `alt-,` and `alt-.` to move to different tabs. 

- `tab 'filename'`: opens the given file in a new tab.
    
- `tabmove '[-+]?n'`: Moves the active tab to another slot. `n` is an integer. If `n` is prefixed with `-` or `+`, then it represents a relative position (e.g. `tabmove +2` moves the tab to the right by `2`). If `n` has no prefix, it represents an absolute position (e.g. `tabmove 2` moves the tab to slot `2`).
    
- `tabswitch 'tab'`: This command will switch to the specified tab. The `tab` can either be a tab number, or a name of a tab.
    
- `textfilter 'sh-command'`: filters the current selection through a shell command as standard input and replaces the selection with the stdout of the shell command. For example, to sort a list of numbers, first select them, and then execute `> textfilter sort -n`.

[micro/commands.md at master · zyedidia/micro · GitHub](https://github.com/zyedidia/micro/blob/master/runtime/help/commands.md)

keybinding equivalents

```
    "Alt-,":           "PreviousTab",
    "Alt-.":           "NextTab",
    "CtrlPageUp":     "PreviousTab",
    "CtrlPageDown":   "NextTab",
```


## Bindings

Ctrl-o opens new files. 

Can also run this as a command with `ctrl-e`

* `open 'filename'`: Open a file in the current buffer.

```
open ~/.config/micro/bindings.json
```

TODO: how is the `ctrl-o` binding defined? 
would like to do something similar for replace and prompt for input
Todo: how to be able to pass parameters after triggering a command:

```
    "Alt-%": "command:replace"
```

## Deleting

I remember reading in the documentation about "ctrl-backspace" to delete a whole word. There is some issue with that. It would be nice to be able to enable. 

```
   "Alt-Backspace":  "DeleteWordLeft",
```

`ctrl-d` is duplicate line by default  
might want to keep it as delete?  
have been using `ctrl-g` for duplicate line  


## Commands

Interesting commands from `ctrl-e help commands`

* `cd 'path'`: Change the working directory to the given `path`.

* `pwd`: Print the current working directory.

* `raw`: micro will open a new tab and show the escape sequence for every event
   it receives from the terminal. This shows you what micro actually sees from
   the terminal and helps you see which bindings aren't possible and why. This
   is most useful for debugging keybindings.

* `showkey`: Show the action(s) bound to a given key. For example
   running `> showkey Ctrl-c` will display `Copy`.

## Other commands

* `reload`: reloads all runtime files.

* `reset 'option'`: resets the given option to its default value

* `retab`: Replaces all leading tabs with spaces or leading spaces with tabs
   depending on the value of `tabstospaces`.



## Tmux

* `term exec?`: Open a terminal emulator running the given executable. If no
   executable is given, this will open the default shell in the terminal
   emulator.

with the `term` command, `micro` acts a bit like `tmux`... may not need `tmux` then? 

Custom: Skip: `Ctrl-p` gets in the way of current tmux bindings for running commands. Move that elsewhere if emacs bindings open up? 

```
    "Ctrl-p": "CommandMode",
```

Maybe if the editor is so lightweight, it's easier to manage separate instances externally in something like tmux? 

tmux bindings may interfere with micro bindings? 

Tried replacing `ctrl-e` -- seems easier to let go of chord for moving to end of line. 


## Failed Custom Bindings

This didn't seem to have any effect when in a command. Only Escape seems to work. 

```
    "Ctrl-g": "Escape",
```


