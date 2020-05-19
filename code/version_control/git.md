# Git

Git is a distributed version control system. 


## CLI Cheat sheet

    git status
    
    git add .
    
    git commit -m "commit message"

Can be helpful to combine ```git add``` and ```git commit -m```

To see what the remote server is set to, use:

    git remote -v
    
    
## Credentials

To see the configured id for a given local repo:

    git config --list --show-origin

*** Please tell me who you are.

Run

  git config user.email "you@example.com"
  git config user.name "Your Name"

to set the identity to use when making changes to the current repository. Different repositories may have different accounts associated with them. 

If that's not something your work requires, you can also set a single account globally:

  git config --global user.email "you@example.com"
  git config --global user.name "Your Name"


https://git-scm.com/docs/gitcredentials

```
$ git config --global credential.helper cache
# Set git to use the credential memory cache

$ git config --global credential.helper 'cache --timeout=3600'
# Set the cache to timeout after one hour (setting is in seconds)
```

If you think you want to store your password, consider setting up an ssh key with your git server instead. 

(Don't do this) to store passwords:

    git config --global credential.helper store
    
These are stored in plaintext, so it is insecure.
via: https://stackoverflow.com/questions/5343068/is-there-a-way-to-skip-password-typing-when-using-https-on-github

### History

To see who has made commits to a repository.

    git blame filename

see also:

    git bisect
    
    git log --follow -p -- file


## Resoving a conflict

If you try to pull in changes to a file you've modified locally, Git does not try to do the merge. You can check in your changes, but that results in a branch merge. 

You can also use stash to move your local changes to the side while you pull in changes from remote. 

    git stash
    
Then to unstash:

    git stash pop

https://www.atlassian.com/git/tutorials/saving-changes/git-stash
https://dev.to/alediaferia/git-tips-for-trunk-based-development-1i1g


## Finding a deleted file in history

Sometimes it's easier to use an interface like gitlab (run it locally) or github to browse the history of the commits to the project. 

If you do not know the exact path you may use
  
    git log --all --full-history -- **/thefile.*

If you know the path the file was at, you can do this:

    git log --all --full-history -- <path-to-file>
    
An alternative:

    git rev-list -n 1 HEAD -- <file_path>


This should show a list of commits in all branches which touched that file. Then, you can find the version of the file you want, and display it with...

    git show <SHA> -- <path-to-file>
    
Or restore it into your working copy with:

    git checkout <SHA>^ -- <path-to-file>

Note the caret symbol (^), which gets the checkout prior to the one identified, because at the moment of <SHA> commit the file is deleted, we need to look at the previous commit to get the deleted file's contents

via:
https://stackoverflow.com/questions/7203515/git-how-to-find-a-deleted-file-in-the-project-commit-history/34681842

https://stackoverflow.com/questions/953481/find-and-restore-a-deleted-file-in-a-git-repository


## Running a server

It's pretty much as simple as having an SSH server running and being able to connect over that.

https://www.linux.com/learn/how-run-your-own-git-server
How to Run Your Own Git Server | Linux.com | The source for Linux information

https://www.google.com/search?q=linux+git+server
linux git server - Google Search

It is best to have a local master repo that different machines can clone from. You can create a copy of the master repository by running:

    cd /path/to/current/checked/out/repository
    git clone --bare . /path/to/master/repository

When you're not on the server (e.g. created the repo somewhere else), you need to create a blank repo on the server first:

    - Log into the server machine.
    - Create a bare repo using git init --bare
    - On the client machine you can push your repo to the server. git remote add origin ssh://user@server:/GitRepos/myproject.git followed by git push origin master

via:
https://stackoverflow.com/questions/6167905/git-clone-through-ssh

then checkout to any device with:

    git clone username@192.168.1.100:/srv/git/repo

If the repo on the server has local files checked out (not bare), when it's time to push changes up to the server, they'll be rejected.

Being able to push is the important option in this scenario. 

An alternative solution could use different branches on either the remote device and/or the server. This seems more cumbersome. 

https://stackoverflow.com/questions/2816369/git-push-error-remote-rejected-master-master-branch-is-currently-checked


## Submodules

Submodles allow for external dependencies to be noted and included. 

To get submodules on clone: 

    git clone --recurse-submodules -j8 git://github.com/foo/bar.git
    cd bar

To pull in submodules for an already checked out repository:

    git submodule update --init --recursive

hint: You've added another git repository inside your current repository.
hint: Clones of the outer repository will not contain the contents of
hint: the embedded repository and will not know how to obtain it.
hint: If you meant to add a submodule, use:
hint:
hint: 	git submodule add <url> path/to/mod
hint:
hint: If you added this path by mistake, you can remove it from the
hint: index with:
hint:
hint: 	git rm --cached path/to/mod
hint:
hint: See "git help submodule" for more information.

#### Remove a submodule

To remove a submodule you need to:

    Delete the relevant section from the .gitmodules file.
    Stage the .gitmodules changes git add .gitmodules
    Delete the relevant section from .git/config.
    Run git rm --cached path_to_submodule (no trailing slash).
    Run rm -rf .git/modules/path_to_submodule (no trailing slash).
    Commit git commit -m "Removed submodule "
    Delete the now untracked submodule files rm -rf path_to_submodule


https://gist.github.com/myusuf3/7f645819ded92bda6677


## Branches

When working on a branch, `git pull` will pull changes from the branch, but `git push` has some surprising behavior that tries to push changes to all matching branches:

```
error: failed to push some refs to 'https://github.com/remote/repo'
hint: Updates were rejected because a pushed branch tip is behind its remote
hint: counterpart. If you did not intend to push that branch, you may want to
hint: specify branches to push or set the 'push.default' configuration variable
hint: to 'simple', 'current' or 'upstream' to push only the current branch.
```

```
git config push.default simple # just for the current repository
git config --global push.default simple # globally for your account
```

https://longair.net/blog/2011/02/27/an-asymmetry-between-git-pull-and-git-push/


## Remove directory from history

If someone adds a directory with large binary files, it may be useful to remove that directory from the history to avoid having to download the large binary data with every `clone` of the repo. 

```
git filter-branch --tree-filter "rm -rf node_modules" --prune-empty HEAD
git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
git gc

echo node_modules/ >> .gitignore
git add .gitignore
git commit -m 'Removing node_modules from git history'

git push origin master --force
```

via:

https://stackoverflow.com/questions/10067848/remove-folder-and-its-contents-from-git-githubs-history



## Changing a commit message


## Merging two git repositories

If you want to merge project-a into project-b:

```
cd path/to/project-b
git remote add project-a path/to/project-a
git fetch project-a
git merge --allow-unrelated-histories project-a/master # or whichever branch you want to merge
git remote remove project-a
```

https://duckduckgo.com/?q=merge+separate+git+repositories&t=canonical&ia=qa
merge separate git repositories at DuckDuckGo
https://stackoverflow.com/questions/1425892/how-do-you-merge-two-git-repositories
How do you merge two Git repositories? - Stack Overflow

Before merging, be sure your incoming repository is in it's new subdirectory, otherwise everything will be merged in at the root level.

cd ~/Downloads/technical
mkdir technical

This fails with an error:

    git mv * technical

Instead, use the -k option:

    git mv -k * technical

git commit -m "moving all items to a sub-directory for easier merging"

```
cd /c/user
git remote add technical ~/Downloads/technical
git fetch technical
git merge --allow-unrelated-histories technical/master # or whichever branch you want to merge
```

Might have a merge message here, then:

   git remote remove technical


## GUI Clients

Knowing the command line interface is useful when you only have a terminal to work with (ssh, termux, etc).

However, there are plenty of cases where a full desktop environment is availabe. In that case, no need to limit yourself to the CLI. 

Github Desktop is a nice GUI client. Github Desktop is only available on Mac and Windows. 

Clients available for linux are listed here:

https://git-scm.com/download/gui/linux

Looking at:

https://git-cola.github.io/

    sudo pacman -Ss git-cola
    # not found
    
    sudo pacman -S gitg


