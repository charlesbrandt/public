# Emacs

It's kind of old school. But it's still a great text editor. 

## Installation

Some systems come with emacs already installed. This is rare. Usually you'll need to install the editor. On linux systems, it's available through your package manager. For example:

    sudo apt-get install emacs emacs-goodies-el
    
(emacs-goodies-el is optional!)
    
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

    rm -rf .emacs.d/


## Configuration

Settings are stored in `.emacs` and `.emacs.d` directories. 

Settings and configurations accumulate over time. The ones I use are available in this directory. I'm aiming for something that works reasonably well with other editors.

I've moved over to Ergo Emacs to keep cut-copy-paste operations in line with other editors. 

I've mapped the action key to alt-a 
Overlaps with Select-all on many applications, but for the rare times that I use select all, I don't mind calling it manually. 

## Lisp

Emacs is built on a language called lisp. 

Custom commands can be written in lisp.

I've written some of my own in this repository. See also `moments.el` / `.emacs.d/moments.el`

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


## Python (TODO)

Would like to enable jedi for goto functionality:

http://lgfang.github.io/mynotes/emacs/emacs-python.html#sec-9-3
Python Coding in Emacs
https://duckduckgo.com/?q=python+jedi&t=canonical&ia=software
python jedi at DuckDuckGo
https://pypi.org/project/jedi/
jedi · PyPI

## File Tree (TODO)

This was a nice feature in Atom for quickly navigating to different files in a related project. 
I especially liked being able to group directories that were spread across different locations into a single project:

https://www.google.com/search?q=emacs+file+tree
emacs file tree - Google Search
https://www.emacswiki.org/emacs/NeoTree
EmacsWiki: Neo Tree
https://github.com/jaypei/emacs-neotree
GitHub - jaypei/emacs-neotree: A emacs tree plugin like NerdTree for Vim.
