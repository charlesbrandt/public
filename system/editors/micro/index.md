# Micro

Promising cli terminal based editor. So encouraging, I've been looking for the desktop native application version.

## Wrap

What about word wrapping?
How to enable? 

run the command:

```
set softwrap true
```

: sets the option to value. See the options help topic for a list of options you can set. This will modify your settings.json with the new value.


In settings file (`~/.config/micro/settings.json`), enable the softwrap option:

```
{
    "softwrap": true
}
```

There is also `wordwrap`, but it requires `softwrap` which seems to do what I'm after

https://github.com/zyedidia/micro/blob/master/runtime/help/options.md

## Line Numbers

Toggle line numbers on and off quickly with `ctrl-r`

    "Ctrl-r":          "ToggleRuler",

same as

```
set ruler true|false
```

## Bindings

open ~/.config/micro/bindings.json

I remember reading in the documentation about "ctrl-backspace" to delete a whole word. There is some issue with that. It would be nice to be able to enable. 

   "Alt-Backspace":  "DeleteWordLeft",


`ctrl-d` is duplicate line by default
might want to keep it as delete?

Todo: how to be able to pass parameters after triggering a command:

```
    "Ctrl-b": "command:tabswitch",
    "Alt-%": "command:replace"
```

## Navigation / Moving Cursor

Jump to next blank line
By default "ctrl-up" and "ctrl-down" jump to the beginning and the end of the document / buffer. 
They should jump to the next blank line instead. 

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

Custom: Skip: These have existing, default equivalents ("Alt-right-arrow" and "Alt-left-arrow").

```
    "Ctrl-e": "EndOfLine",
    "Ctrl-a": "StartOfTextToggle",
```


## Selection

Is there a way to bind "ctrl-space" to "start selection"?

On mobile devices with touch keyboards, shift is not avaliable in the same way. I was unable to find a way to perform text block selection on a mobile device. 


## Buffers

Buffer switching?

Doesn't seem to be the same as emacs here. There is only the concept of tabs:

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

Maybe if the editor is so lightweight, it's easier to manage separate instances externally in something like tmux? 

Plugin, Todo
open buffer from list
print open buffers as list


## Failed Custom Bindings

This didn't seem to have any effect when in a command. Only Escape seems to work. 

```
    "Ctrl-g": "Escape",
```


Tried replacing `ctrl-e` -- seems easier to let go of chord for moving to end of line. 
Custom: Skip: `Ctrl-p` gets in the way of current tmux bindings for running commands. Move that elsewhere if emacs bindings open up? 

```
    "Ctrl-p": "CommandMode",
```


