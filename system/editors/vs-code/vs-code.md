# Visual Studio Code

https://code.visualstudio.com/docs/?dv=linux64_deb  
Documentation for Visual Studio Code  

## Install

Download and install from site:  
https://code.visualstudio.com/download

```
cd ~/Downloads
sudo dpkg -i code...
```

Add to favorites (optionally)

In a terminal, when you navigate to a directory you want to work on in vs-code, just type

```
code .
```

## Preferences

Open Settings (Ctrl-Shift-P -> "Settings")

```
"Preferences: Open User Settings (JSON)"
```

### Current settings

See below for a description about what the settings do

```json
{
  "window.newWindowDimensions": "inherit",
  "workbench.startupEditor": "newUntitledFile",
  "workbench.editor.wrapTabs": false,
  "workbench.sideBar.location": "right",
  "breadcrumbs.enabled": true,
  "editor.bracketPairColorization.enabled": true,
  "editor.guides.bracketPairs":"active",
  "editor.minimap.enabled": false,
  "update.mode": "none",
  "explorer.confirmDragAndDrop": false,
  "window.zoomLevel": 0,
  "editor.formatOnSave": true,
  "eslint.format.enable": true,
  "iconify.inplace": false,
  "python.showStartPage": false,
  "workbench.editorAssociations": [
    {
      "viewType": "jupyter.notebook.ipynb",
      "filenamePattern": "*.ipynb"
    }
  ],
  "telemetry.enableTelemetry": false,
  "telemetry.enableCrashReporter": false,
  "redhat.telemetry.enabled": false,
  "files.exclude": {
     "**/.git": true,
     "**/.DS_Store": true,
     "node_modules" : true,
     "**/_tmp_*": true,
  },
}
```


### Preference Details

Descriptions of what the above options are setting

### Ignore files

Some package managers add extra files that you don't need to access in VSCode. 

Configure VSCode to ignore them with the following settings

```
       "files.exclude": {
            "**/.git": true,
            "**/.DS_Store": true,
            "node_modules" : true,
            "**/_tmp_*": true,
        }
```

Strangely, this answers the question I had, even though it doesn't answer the question of the post:

https://stackoverflow.com/questions/30313805/how-to-ignore-node-modules-folder-during-typescript-build-in-vscode

### Default window size

{
"workbench.startupEditor": "newUntitledFile",
"window.newWindowDimensions": "inherit"
}

https://stackoverflow.com/questions/44412233/how-to-set-window-size-and-position-in-visual-studio-code

### Minimap

The miniature over view of the current file to the right.

    "editor.minimap.enabled": false

### Tabs & Breadcrumbs

I go back and forth on this. Hoping that Tab Group helper will minimize the desire to see all of the tabs wrapped. It does take up a bit of vertical real estate when enabled. 

```
  "workbench.editor.wrapTabs": true,
  "breadcrumbs.enabled": false,
```

### Updates

I prefer to handle updates when updating the OS.
https://code.visualstudio.com/docs/setup/linux
https://stackoverflow.com/questions/42496935/disabling-visual-studio-code-update-notification

### Bracket Matching

It's helpful to be able to jump to the corresponding matching bracket, especially if it's far away...

    Tip: You can jump to the matching bracket with Ctrl+Shift+\

https://code.visualstudio.com/docs/editor/editingevolved





## Keybindings

Keybindings are a personal preference. There are a few keyboard shortcuts that I find useful. Some have carried over from [emacs](../emacs/emacs.md), but I don't need to remap the whole configuration to be just like emacs. More like ergo-emacs.

https://code.visualstudio.com/docs/getstarted/keybindings

File > Preferences > Keyboard Shortcuts

All keyboard shortcuts in VS Code can be customized via the keybindings.json file.

To configure keyboard shortcuts through the JSON file, open the keybindings.json file from the Command Palette (Ctrl+Shift+P) with the

    Preferences: Open Keyboard Shortcuts (JSON) command.

You can also open Keyboard Shortcuts editor and select the Open Keyboard Shortcuts (JSON) button on the right of the editor title bar. [couldn't find this path]

### Custom keyboard shortcut bindings

```json
// Place your key bindings in this file to override the defaults
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
    "key": "ctrl+d",
    "command": "-editor.action.addSelectionToNextFindMatch",
    "when": "editorFocus"
  },
  {
    "key": "ctrl+d",
    "command": "deleteRight"
  },
  {
    "key": "ctrl+k",
    "command": "deleteAllRight"
  }
]
```

### Multiple Cursors

I disable the Ctrl-Up and Ctrl-Down feature for creating cursors. I prefer to use these key combos to [navigate the document](#space-block-jumper). It is still possible to create multiple cursors with the mouse using Alt-Click.

https://tahoeninjas.blog/2019/03/30/multi-cursor-editing-in-visual-studio-code/




## Workspace Extensions

Recommended extensions can be configured as part of a project. These settings are stored in `.vscode/extensions.json`

```json
{
  "recommendations": [
    "jmfirth.vsc-space-block-jumper",
    "vue.volar",
    "dbaeumer.vscode-eslint",
    "ms-python.python",
    "ms-vscode-remote.remote-ssh",
    "redhat.vscode-yaml",
    "streetsidesoftware.code-spell-checker",
    "csstools.postcss",
    "antfu.unocss",
    "wix.vscode-import-cost",
    "lokalise.i18n-ally",
  ]
}
```

https://code.visualstudio.com/docs/editor/extension-marketplace#_recommended-extensions

Use `@recommended` to filter in extension panel

See also: 

https://code.visualstudio.com/docs/getstarted/settings

### Space Block Jumper

jmfirth.vsc-space-block-jumper

Space Block Jumper allows jumping to the next blank line. (Block Navigation)

https://marketplace.visualstudio.com/items?itemName=jmfirth.vsc-space-block-jumper

Block travel looks like another viable option:

https://marketplace.visualstudio.com/items?itemName=sashaweiss.block-travel

https://stackoverflow.com/questions/45788119/is-there-a-vs-code-shortcut-to-move-select-up-down-to-the-next-empty-line

Still requires adding custom bindings (included below).

Adding multiple cursors is still available by default with alt-shift-down and alt-shift-up.


### Vue Language Features (Volar)

    "vue.volar",

By: Johnson Chu

Volar is coming up as a preferred alternative.

Seems to work. If you disable Vetur and install Volar, be sure to restart VS Code for changes to take effect. 

https://marketplace.visualstudio.com/items?itemName=johnsoncodehk.volar  
Vue Language Features (Volar) - Visual Studio Marketplace  

https://github.com/johnsoncodehk/volar  
GitHub - johnsoncodehk/volar: ⚡ Fast Vue Language Support Extension  

https://www.duckduckgo.com/search?q=vs+code+extensions+vue
vs code extensions vue - Google Search


### eslint

    "dbaeumer.vscode-eslint",



### Python

    "ms-python.python",


I like to enable AutoPEP8 to format my python to automatically meet formatting rules. (i.e. linting for Python) Occasionally I've had scenarios where this can break things. Example: adding a path to the import path in a script, then that gets moved and the to-be-imported module is no longer available.

Formatting can be disabled with:

"python.formatting.provider": "none",



### Remote Development

ms-vscode-remote.remote-ssh

I've had success configuring the 'Remote - SSH' extension

https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh

https://code.visualstudio.com/docs/remote/troubleshooting#_configuring-key-based-authentication

Ctrl-Shift-P -> Remote SSH: connection

When configuring the connection, only specify 'username@host'.

do **not** include 'ssh -A ' in front of the username@host.

You'll need to install any additional required extensions (e.g. Vetur) to the ssh server separately. Syntax highlighting works when I do that.

See also / Previously

The remote development extension allows editing code on a remote server over SSH.

https://code.visualstudio.com/docs/remote/remote-overview

https://code.visualstudio.com/remote-tutorials/ssh/getting-started

https://code.visualstudio.com/remote-tutorials/ssh/connect-to-vm

https://github.com/Microsoft/vscode-remote-release



### YAML

redhat.vscode-yaml

By: Red Hat

#### Docker
helps with formatting docker-compose.yml
or is that just the redhat yml parser?

By: Microsoft

Also:
ms-vscode-remote.remote-containers



### Spell Checker

streetsidesoftware.code-spell-checker

By Street Side Software

Seems to be the top pick these days

https://marketplace.visualstudio.com/items?itemName=streetsidesoftware.code-spell-checker  
Code Spell Checker - Visual Studio Marketplace  
https://github.com/streetsidesoftware/vscode-spell-checker  
streetsidesoftware/vscode-spell-checker: A simple source code spell checker for code  
https://duckduckgo.com/?t=canonical&q=vs+code+spell+checker+md&ia=web  
vs code spell checker md at DuckDuckGo  

The Microsoft version has been deprecated:  
https://github.com/Microsoft/vscode-spell-check/blob/master/README.md  


### CSS

    "csstools.postcss",


Plus one of these should suffice

    "antfu.unocss",


WindiCSS IntelliSense (recommended by Vitesse)


Tailwind CSS IntelliSense

https://blog.katherinempeterson.com/4-must-have-vscode-extensions-for-tailwindcss


### Import Cost

wix.vscode-import-cost


### Il8n-ally

    "lokalise.i18n-ally",





## Extension Icons

Some extensions add a lot of extra icons to the interface. This can get in the way of things like tabs. May be possible to change settings to remove them. May not need them installed at all. 

TODO:
How to remove all the extension shortcut icons?
end up with tons of these...
can't see the tabs any more!

Right click on extension->Extension Settings


