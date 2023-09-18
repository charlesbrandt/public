# Input events

When pressing a key on a keyboard or using a mouse, input events are sent to the terminal. When setting up keybindings, it can help to know what event is being received by the application.

I've seen many tools to run in a terminal to echo these back.

## Micro

Launch a command: `raw`

> micro will open a new tab and show the escape sequence for every event it receives from the terminal. This shows you what micro actually sees from the terminal and helps you see which bindings aren't possible and why. This is most useful for debugging keybindings.

https://github.com/zyedidia/micro/blob/master/runtime/help/commands.md
