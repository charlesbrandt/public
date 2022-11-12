# Emacs

It's kind of old school. But it's still a great text editor. 

## Installation

Some systems come with emacs already installed. This is rare. Usually you'll need to install it. On linux systems, it's available through your package manager. For example:

```
sudo apt-get install emacs
```
    
Link to your configurations:
 
```
git clone --recurse-submodules git@github.com:charlesbrandt/public.git
cd
rm .emacs
rm .emacs.d
ln -s ~/public/system/editors/emacs/.emacs .emacs
ln -s ~/public/system/editors/emacs/.emacs.d .emacs.d
```

Sometimes emacs will create a configuration directory if it is run and one doesn't already exist. (dangerous! make sure you don't have any old configurations here!)

```
rm -rf .emacs.d/
```

## Running

```
emacs
```

to ensure the editor runs in the terminal and does not launch a gui version

```
emacs -nw
```

## Using

To close an unexpected frame in a window, it can be difficult via a terminal if `Alt-0` doesn't have an effect. 

```
M-x delete-window
```

## Configuration

Settings are stored in `.emacs` and `.emacs.d` directories. 

Settings and configurations accumulate over time. The ones I use are available in this directory. I'm aiming for something that works reasonably well with other editors.

I like Ergo Emacs to keep cut-copy-paste operations in line with other editors. 

I map the action key (typically ctrl-x) to alt-a 
Overlaps with Select-all on many applications, but for the rare times that I use select all, I don't mind calling it manually. 

## Lisp

Emacs is built on a language called lisp. 

Custom commands can be written in lisp.

I've written some of my own in this repository. See also `moments.el` / `.emacs.d/moments.el`

## Themes

```
;; themes and start up settings 
(load-file "~/.emacs.d/editor.el")
```

Not all themes are created equally. Considering all types of syntax highlighting is important. 
A good source of inspiration:

https://emacsthemes.com/popular/index.html

### Melpa

Milkypostman’s Emacs Lisp Package Archive

https://melpa.org/#/
MELPA
https://melpa.org/#/getting-started
Getting Started - MELPA

Interact with:

    m-X package-list-packages
    
" If you run into a problem installing or upgrading, you may need to go into your ~/.emacs.d/elpa/ directory and delete packages that are installed multiple times. This can happen when the install times out."


### Keymaps

One of the first things to do is to set up some sane key bindings. Yes, I know the default ones in emacs are optimized for efficient hand movement. I've had them burned into my muscle memory. But they're really non-standard, and they get in the way when switching to just about any other typical computing platform. 

In my configuration, custom keymaps definitions are in:
.emacs.d/keymaps.el

It looks like many of these may be configured as part of the ergoemacs package:
https://ergoemacs.github.io/

Make sure you have at least emacs version 24.1:
M-x emacs-version
via: https://stackoverflow.com/questions/19393538/get-emacs-version-from-within-emacs


Trying it out first:

    cd .emacs.d/
    git clone https://github.com/ergoemacs/ergoemacs-mode.git

If it works, be sure to include the module as a submodule in this repository:

    cd ~/public
    git submodule add https://github.com/ergoemacs/ergoemacs-mode.git editors/emacs/.emacs.d/modules/ergoemacs-mode
    
via: https://git-scm.com/book/en/v2/Git-Tools-Submodules

Then, when cloning this repository, you can get the latest version with:

    git clone --recurse-submodules -j8 git://github.com/foo/bar.git
    
via: https://stackoverflow.com/questions/3796927/how-to-git-clone-including-submodules


