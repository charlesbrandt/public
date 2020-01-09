# Emacs

What? No documentation for emacs? 

Well, it's kind of old school. But it's still a great text editor. 

Settings and configurations accumulate over time. My current ones are available under this repository. 

Feel free to revise and revisit these. Especially with any insight from other editors. 

One of the first things to do is to set up some sane key bindings. Yes, I know the default ones in emacs are optimized for hand efficient hand movement. I've had them burned into my muscle memory. But they're really non-standard, and they get in the way when switching to just about any other typical computing platform. 

In my configuration, I've moved all custom keymaps definitions to:
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

    git submodule add https://github.com/ergoemacs/ergoemacs-mode.git editors/emacs/.emacs.d/ergoemacs-mode
    
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
