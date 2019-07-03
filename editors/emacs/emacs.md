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

