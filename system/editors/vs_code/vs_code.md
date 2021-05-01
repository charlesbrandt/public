# Visual Studio Code

## Install

Download and install from site:  
https://code.visualstudio.com/download

```
cd ~/Downloads
sudo dpkg -i code...
```

Add to favorites (optionally)

Launch!

## Preferences

Open Settings (Ctrl-Shift-P -> "Settings")

Choose either:

    "Preferences: Open Settings (JSON)"

or

    "Preferences:Open Settings (UI)"

### Current settings

See below for details

```json
{
  "window.newWindowDimensions": "inherit",
  "workbench.startupEditor": "newUntitledFile",
  "workbench.editor.wrapTabs": true,
  "breadcrumbs.enabled": false,
  "editor.minimap.enabled": false,
  "update.mode": "none",
  "explorer.confirmDragAndDrop": false,
  "window.zoomLevel": 0,
  "telemetry.enableTelemetry": false,
  "telemetry.enableCrashReporter": false,
  "[vue]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "[javascript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "[json]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "editor.formatOnSave": true,
  "vetur.ignoreProjectWarning": true,
  "eslint.format.enable": true,
  "prettier.prettierPath": "./bin/prettier",
  "python.showStartPage": false,
  "workbench.editorAssociations": [
    {
      "viewType": "jupyter.notebook.ipynb",
      "filenamePattern": "*.ipynb"
    }
  ],
  "gitlens.advanced.messages": {
    "suppressGitVersionWarning": true
  },
  "gitlens.menus": null
}
```

### Prettier

    "prettier.prettierPath": "./bin/prettier",

Hoping that this will prevent needing to install node_modules outside of the container. VS Code looks for the local version of prettier, but if one is not installed (hidden by container), then it doesn't do anything.

```
prettier.prettierPath

Supply a custom path to the prettier module. This path should be to the module folder, not the bin/script path. i.e. ./node_modules/prettier, not ./bin/prettier.
```

See https://marketplace.visualstudio.com/items?itemName=esbenp.prettier-vscode

### Default window size

{
"workbench.startupEditor": "newUntitledFile",
"window.newWindowDimensions": "inherit"
}

https://stackoverflow.com/questions/44412233/how-to-set-window-size-and-position-in-visual-studio-code

### Minimap

The miniature over view of the current file to the right.

    "editor.minimap.enabled": false
    
### Updates

I prefer to handle updates when updating the OS.
https://code.visualstudio.com/docs/setup/linux
https://stackoverflow.com/questions/42496935/disabling-visual-studio-code-update-notification


## Useful Features

### Bracket Matching

Sometimes it's helpful to be able to jump to the corresponding matching bracket, especially if it's far away...

    Tip: You can jump to the matching bracket with Ctrl+Shift+\

https://code.visualstudio.com/docs/editor/editingevolved

### Auto Fix

To improve the formatting of your source code, you can use the Format Document command Ctrl+Shift+I to format the entire file or Format Selection Ctrl+K Ctrl+F to just format the selected text.

https://code.visualstudio.com/docs/languages/html

### Multiple Cursors

I disable the Ctrl-Up and Ctrl-Down feature for creating cursors (see Keybindings below), but it is still possible to create multiple cursors with the mouse using Alt-Click.

https://tahoeninjas.blog/2019/03/30/multi-cursor-editing-in-visual-studio-code/

## Keybindings

There are a few keyboard shortcuts that I find useful. Some have carried over from emacs, but I don't need to remap the whole configuration to be just like emacs. More like ergo-emacs.

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

## Extensions

https://www.google.com/search?q=vs+code+extensions+vue
vs code extensions vue - Google Search

TODO:
How to remove all the extension shortcut icons?
end up with tons of these...
can't see the tabs any more!

Right click on extension->Extension Settings

### Block Navigation

Space Block Jumper allows jumping to the next blank line.

https://marketplace.visualstudio.com/items?itemName=jmfirth.vsc-space-block-jumper

Block travel looks like another viable option:

https://marketplace.visualstudio.com/items?itemName=sashaweiss.block-travel

https://stackoverflow.com/questions/45788119/is-there-a-vs-code-shortcut-to-move-select-up-down-to-the-next-empty-line

Still requires adding custom bindings (included below).

Adding multiple cursors is still available by default with alt-shift-down and alt-shift-up.

### Quick Open

By Zongmin Lei

Similar to Emacs method for opening files via keyboard only

Keybindings
CTRL+⌘+O - Quick Open File
CTRL+⌘+P - Quick Open Input Path

Does not appear to work with opening a new file? 

### Vue (Vetur)

By: Pine Wu

https://medium.com/@deepaksisodiya/top-vs-code-extensions-for-vue-js-development-93cb548baa32
Top VS Code Extensions For Vue.js Development - deepak sisodiya - Medium

https://vuejs.github.io/vetur/

Frequently don't have the configs where the project is being loaded:

"vetur.ignoreProjectWarning": true,

May cause some issues with linting...
https://vuejs.github.io/vetur/guide/FAQ.html#vetur-can-t-find-package-json-in-xxxx-xxxxxx

### Prettier

By: Prettier

Prettier - Code formatter

helps with code formatting

https://glebbahmutov.com/blog/configure-prettier-in-vscode/
{
"editor.defaultFormatter": "esbenp.prettier-vscode",
"editor.formatOnSave": true
}

may require the project to install prettier as a dev dependency so that vscode has it available to use.

### Python

I like to enable AutoPEP8 to format my python to automatically meet formatting rules. (i.e. linting for Python) Occasionally I've had scenarios where this can break things. Example: adding a path to the import path in a script, then that gets moved and the to-be-imported module is no longer available.

Formatting can be disabled with:

"python.formatting.provider": "none",

### YAML

By: Red Hat

### Bracket Pair Colorizer

By: CoenraadS

### Spell Checker

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

### Docker

By: Microsoft

Also:
ms-vscode-remote.remote-containers

### Kubernetes

### Remote Development

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

### Import Cost

## Other Extensions

### GitLens

By: Eric Amodio

I find the menu actions take away precious space for other file tabs:

    "gitlens.menus": null,

I want to like this extension, but I feel like the information it offers gets in the way more often than it helps me understand the code.

### XML

By: Red Hat

Helpful for SVG formatting?
Requires Java?

### HTML Preview

A few different options for this task. None of them receive high marks.

https://marketplace.visualstudio.com/items?itemName=tht13.html-preview-vscode&ssr=false#review-details

https://marketplace.visualstudio.com/items?itemName=SimonSiefke.html-preview&ssr=false#review-details

https://marketplace.visualstudio.com/items?itemName=hdg.live-html-previewer&ssr=false#review-details

### Eslint

https://github.com/Microsoft/vscode-eslint
https://marketplace.visualstudio.com/items?itemName=dbaeumer.vscode-eslint

### Live Server

Launch a development local Server with live reload feature.
I prefer using Docker containers for this.

### Github

https://marketplace.visualstudio.com/items?itemName=GitHub.vscode-pull-request-github

### Git History

### Color Picker

### Markdown

Do these really add anything? If you want to work with converting Markdown, see Nuxt/Content module (in the context of Node JS)

#### Markdown All in One

ctrl-shift-v keybinding may conflict with HTML Preview (is it from here?)

#### Useful Shortcuts

https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one

https://marketplace.visualstudio.com/items?itemName=mdickin.markdown-shortcuts

#### Linting

markdownlint -- checks for formatting issues.

#### Highlighting

TODO:

Looking for better syntax highlighting in .md files (editor view, not preview).

This one set the theme for the whole editor, not just markdown files:
https://marketplace.visualstudio.com/items?itemName=ms-vscode.Theme-MarkdownKit

Could try some custom definitions for missing ones
https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide

#### Preview

https://code.visualstudio.com/Docs/languages/markdown

To switch between views, press Ctrl+Shift+V in the editor. You can view the preview side-by-side (Ctrl+K V) with the file you are editing and see changes reflected in real-time as you edit.

Outline view can act as an automatic Table Of Contents. Below File Explorer view.

VS Code can even compile markdown to HTML!

### Auto Close Tag

I find this gets in the way more than it helps. YMMV.

### TODO

Previously installed... not sure if they are actively being used:

JavaScript (ES6) code snippets
HTML Snippets
HTML CSS Support

Evaluate:

npm
npm intellisense

### Journal

It's a pretty heavy application memory-wise to keep lots of open files / instances. There are better editors for keeping notes.

'ctrl-j': 'moments-atom:journal'

## Reporting

Up to you if you want to leave this enabled or not. Both ways make sense!

### Telemetry reporting

Disable telemetry

https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting

VS Code collects usage data and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send usage data to Microsoft, you can set the telemetry.enableTelemetry setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableTelemetry and uncheck the setting. This will silence all telemetry events from VS Code going forward. Telemetry information may have been collected and sent up until the point when you disable the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableTelemetry": false

You can inspect telemetry events in the Output panel by setting the log level to Trace using Developer: Set Log Level from the Command Palette.

Important Notice: VS Code gives you the option to install Microsoft and third party extensions. These extensions may be collecting their own usage data and are not controlled by the telemetry.enableTelemetry setting. Consult the specific extension's documentation to learn about its telemetry reporting.

### Crash reporting

VS Code collects data about any crashes that occur and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send crash data to Microsoft, you can set the telemetry.enableCrashReporter setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableCrashReporter and uncheck the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableCrashReporter": false

Important Notice: This option requires a restart of VS Code to take effect.

## Links

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

## Emacs Modes

Usually the keybindings I use are tailored enough. Plus the use of Ergo Emacs makes emacs more compatible with standard shortcuts.

Still looking for a good way to yank to the end of the line. (Ctrl-K)

https://duckduckgo.com/?q=visual+studio+code+yank+to+end+of+line+command&t=canonical&ia=web
visual studio code yank to end of line command at DuckDuckGo
https://marketplace.visualstudio.com/items?itemName=reignofwebber.emacsx
Emacs<>Vscode - Visual Studio Marketplace
https://code.visualstudio.com/docs/getstarted/tips-and-tricks
Visual Studio Code Tips and Tricks
https://marketplace.visualstudio.com/items?itemName=NotKyon.vscode-emacs-neon
Emacs Friendly Keybindings - Visual Studio Marketplace
https://github.com/NotKyon/vscode-emacs-neon
GitHub - NotKyon/vscode-emacs-neon: Visual Studio Code plugin emulating Emacs functionality
https://github.com/SebastianZaha/vscode-emacs-friendly
GitHub - SebastianZaha/vscode-emacs-friendly: Visual Studio Code plugin emulating Emacs functionality
