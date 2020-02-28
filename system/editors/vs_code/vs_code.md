# Visual Studio Code

download and install from site:
https://code.visualstudio.com/download

add to favorites

launch


## Keybindings

There are a few keyboard shortcuts that I find useful. Some have carried over from emacs, but I don't need to remap the whole configuration to be just like emacs. More like ergo-emacs. 

https://code.visualstudio.com/docs/getstarted/keybindings

File > Preferences > Keyboard Shortcuts. 

All keyboard shortcuts in VS Code can be customized via the keybindings.json file.

To configure keyboard shortcuts through the JSON file, open the keybindings.json file from the Command Palette (Ctrl+Shift+P) with the Preferences: Open Keyboard Shortcuts (JSON) command.

You can also open Keyboard Shortcuts editor and select the Open Keyboard Shortcuts (JSON) button on the right of the editor title bar. [couldn't find this path]

### Block navigation

Space Block Jumper allows jumping to the next blank line. 

https://marketplace.visualstudio.com/items?itemName=jmfirth.vsc-space-block-jumper

Block travel looks like another viable option:

https://marketplace.visualstudio.com/items?itemName=sashaweiss.block-travel

https://stackoverflow.com/questions/45788119/is-there-a-vs-code-shortcut-to-move-select-up-down-to-the-next-empty-line

Still requires adding custom bindings (included below).

Adding multiple cursors is still available by default with alt-shift-down and alt-shift-up. 

### Custom keyboard shortcut bindings

```
// Place your key bindings in this file to override the defaultsauto[]
[
    {
        "key": "ctrl+b",
        "command": "-workbench.action.toggleSidebarVisibility"
    },
    {
        "key": "ctrl+b",
        "command": "workbench.action.quickOpen"
    },
    {
        "key": "ctrl+e",
        "command": "-workbench.action.quickOpen"
    },
    {
        "key": "ctrl+e",
        "command": "cursorLineEnd"
    },
    {
        "key": "ctrl+e",
        "command": "-workbench.action.quickOpenNavigateNextInFilePicker",
        "when": "inFilesPicker && inQuickOpen"
    },
    {
        "key": "ctrl+a",
        "command": "-editor.action.webvieweditor.selectAll",
        "when": "!editorFocus && !inputFocus && activeEditor == 'WebviewEditor'"
    },
    {
        "key": "ctrl+a",
        "command": "-editor.action.selectAll",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+a",
        "command": "cursorLineStart"
    },
    {
        "key": "shift+alt+,",
        "command": "cursorTop",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+home",
        "command": "-cursorTop",
        "when": "textInputFocus"
    },
    {
        "key": "shift+alt+.",
        "command": "-editor.action.autoFix",
        "when": "editorTextFocus && !editorReadonly && supportedCodeAction =~ /(\\s|^)quickfix\\b/"
    },
    {
        "key": "shift+alt+.",
        "command": "cursorBottom",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+end",
        "command": "-cursorBottom",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+down",
        "command": "spaceBlockJumper.moveDown",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+down",
        "command": "-scrollLineDown",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+up",
        "command": "spaceBlockJumper.moveUp",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+up",
        "command": "-scrollLineUp",
        "when": "textInputFocus"
    },
    {
        "key": "ctrl+shift+down",
        "command": "spaceBlockJumper.selectDown"
    },
    {
        "key": "ctrl+shift+down",
        "command": "-editor.action.insertCursorBelow",
        "when": "editorTextFocus"
    },
    {
        "key": "ctrl+shift+up",
        "command": "spaceBlockJumper.selectUp"
    },
    {
        "key": "ctrl+shift+up",
        "command": "-editor.action.insertCursorAbove",
        "when": "editorTextFocus"
    },
    {
        "key": "ctrl+g",
        "command": "editor.cancelOperation",
        "when": "cancellableOperation"
    },
    {
        "key": "escape",
        "command": "-editor.cancelOperation",
        "when": "cancellableOperation"
    },
    {
        "key": "ctrl+l",
        "command": "workbench.action.gotoLine"
    },
    {
        "key": "ctrl+g",
        "command": "-workbench.action.gotoLine"
    },
    {
        "key": "ctrl+l",
        "command": "-expandLineSelection",
        "when": "textInputFocus"
    },
    {
        "key": "shift+alt+5",
        "command": "editor.action.startFindReplaceAction"
    },
    {
        "key": "ctrl+h",
        "command": "-editor.action.startFindReplaceAction"
    }
]
```

TODO:

  'ctrl-tab': 'editor:auto-indent'
  using 'shift-ctrl-I' will format the whole document according to linter configurations

  'ctrl-j': 'moments-atom:journal'

  'ctrl-d': 'editor:delete'


## Extensions

https://www.google.com/search?q=vs+code+extensions+vue
vs code extensions vue - Google Search

https://medium.com/@deepaksisodiya/top-vs-code-extensions-for-vue-js-development-93cb548baa32
Top VS Code Extensions For Vue.js Development - deepak sisodiya - Medium

https://vuejs.github.io/vetur/

### Eslint

https://github.com/Microsoft/vscode-eslint
https://marketplace.visualstudio.com/items?itemName=dbaeumer.vscode-eslint

### Auto Fix

To improve the formatting of your HTML source code, you can use the Format Document command Ctrl+Shift+I to format the entire file or Format Selection Ctrl+K Ctrl+F to just format the selected text.

https://code.visualstudio.com/docs/languages/html

### Remote Development

The remote development extension allows editing code on a remote server over SSH. 

https://code.visualstudio.com/docs/remote/remote-overview

https://code.visualstudio.com/remote-tutorials/ssh/getting-started

https://code.visualstudio.com/remote-tutorials/ssh/connect-to-vm

However, I had difficulty getting syntax highlighting to work. 


## Default window size

Open Settings (Ctrl-Shift-P -> "Settings")

Choose either:

    "Preferences:Open Settings (UI)"

or

    "Preferences: Open Settings (JSON)"

{
    "workbench.startupEditor": "newUntitledFile",
    "window.newWindowDimensions": "inherit"
}

https://stackoverflow.com/questions/44412233/how-to-set-window-size-and-position-in-visual-studio-code


## How to disable telemetry reporting

Disable telemetry

https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting

VS Code collects usage data and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send usage data to Microsoft, you can set the telemetry.enableTelemetry setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableTelemetry and uncheck the setting. This will silence all telemetry events from VS Code going forward. Telemetry information may have been collected and sent up until the point when you disable the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableTelemetry": false
You can inspect telemetry events in the Output panel by setting the log level to Trace using Developer: Set Log Level from the Command Palette.

Important Notice: VS Code gives you the option to install Microsoft and third party extensions. These extensions may be collecting their own usage data and are not controlled by the telemetry.enableTelemetry setting. Consult the specific extension's documentation to learn about its telemetry reporting.

### How to disable crash reporting
VS Code collects data about any crashes that occur and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send crash data to Microsoft, you can set the telemetry.enableCrashReporter setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableCrashReporter and uncheck the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableCrashReporter": false
Important Notice: This option requires a restart of VS Code to take effect.

### Links
https://code.visualstudio.com/docs/setup/linux#_visual-studio-code-is-unable-to-watch-for-file-changes-in-this-large-workspace-error-enospc

https://www.google.com/search?q=vs+code&oq=vs+code
vs code - Google Search
https://code.visualstudio.com/download
Download Visual Studio Code - Mac, Linux, Windows
https://code.visualstudio.com/docs/?dv=linux64_deb
Documentation for Visual Studio Code
https://code.visualstudio.com/docs?start=true
Documentation for Visual Studio Code
https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting
Visual Studio Code Frequently Asked Questions
