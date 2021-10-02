# Git

Git is a distributed [version control system](index.md), popular for keeping [code](../) repositories in sync. 

[git servers](git-server.md)

## CLI Cheat sheet

    git pull

    git status

    git add .

    git commit -m "commit message"

    git push

## What makes a good commit message

  - Separate subject from body with a blank line
  - Limit the subject line to 50 characters
  - Do not end the subject line with a period
  - Use the imperative mood in the subject line
  - Wrap the body at 72 characters
  - Use the body to explain what and why vs. how
  - Capitalize the subject line

Source: http://chris.beams.io/posts/git-commit/#seven-rules

https://gist.github.com/julienbourdeau/e605e4b8b47da97c249a0f72598529c8

## Configurations

```
git config --list
```

## Remotes

If you want to see - what repo something pushes back to - the configured id for a given local repo

Use

    git config --list --show-origin

To see what the remote server is set to, use:

    git remote -v


Sources that a repository is set up to track or follow.

The `origin` is the one that the repo will push to / pull from by default.

It is possible to have more than one remote !

Remotes can be called whatever you want.

### `origin` vs `upstream`

`upstream` can be useful for tracking a boilerplate source for a project.

```
git remote add upstream git@example.com:project/boilerplate.git
git remote -v
```

https://unfoxnews.com/keep-track-of-multiple-git-remote-repositories/

### Update origin

Sometimes projects get checked out using the public url, but then changes are made. 

```
git remote remove origin
git remote add origin git@gitlab.com:charlesbrandt/public.git
git branch --set-upstream-to=origin/main main
git pull
```

## User Details

\*\*\* Please tell me who you are.

Run

```
git config user.email "you@example.com"
git config user.name "Your Name"
```

to set the identity to use when making changes to the current repository. Different repositories may have different accounts associated with them.

If that's not something your work requires, you can also set a single account globally:

```
git config --global user.email "you@example.com"
git config --global user.name "Your Name"
```

### SSH

use `ssh -A` to share your local credentials with the machine you connect to.

Then if you register your local machine's public ssh key with a service like Gitlab or [Github](https://github.com/settings/keys), you won't have to type your username and password every time you push or pull.

### Passwords

If you think you want to store your password, consider setting up an ssh key with your git server instead.

(Don't do this) to store passwords:

    git config --global credential.helper store

These credentials are stored in plaintext. Plaintext is insecure, especially on shared systems.
[via](https://stackoverflow.com/questions/5343068/is-there-a-way-to-skip-password-typing-when-using-https-on-github)

https://git-scm.com/docs/gitcredentials

```
$ git config --global credential.helper cache
# Set git to use the credential memory cache

$ git config --global credential.helper 'cache --timeout=3600'
# Set the cache to timeout after one hour (setting is in seconds)
```

## Create New Repository

    git init

The default branch on `git init` is currently set to master.
If you want to change it to `main`, this is a good point to do so.

```
git checkout -b main
git branch -d master
git branch -a
```

Confirm the remote server is set to your own repo:

    git remote -v

From here you can add different remotes (origins and upstreams) as needed. 

(See also server section)


## Common Workflows

### History

To see the changes to the repository in reverse chronological order:

    git log
    
To see history for a specific file:

    git log --follow -p -- file

To see who has made commits to a repository.

    git blame filename

see also:

```
git bisect
```

https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History

### Moving files / directories

Tracking a move with git with:

    git mv [src] [destination]

** Important Note **

This may affect the ability to see history of files via `log`.

currently (2020.12), GitHub is not able to associate the history of a moved file with it's previous location's history:

https://github.community/t/renaming-folder-within-a-repo-loses-file-history/1752

### Resoving a conflict

If you try to pull in changes to a file you've modified locally, Git does not try to do the merge.

Use stash to move local changes to the side while pulling in changes from remote.

    git stash

Then to unstash:

    git stash pop

https://www.atlassian.com/git/tutorials/saving-changes/git-stash
https://dev.to/alediaferia/git-tips-for-trunk-based-development-1i1g

### Undo add files

Newer versions of git show reminders about these commands when running `git status`

```
git reset
```

https://stackoverflow.com/questions/348170/how-do-i-undo-git-add-before-commit

### Revert local changes

`git checkout path/to/file` will revert the local changes to `path/to/file`

### Collaborative Commits

```
git commit -m "Commit title
Commit body

Co-authored-by: First Person <example@example.com>
Co-authored-by:
"
```

### Changing a commit message

Ideally, do this before pushing up to a public repo, otherwise it results in a branch merge. This will change the most recent commit:

    git commit --amend

https://docs.github.com/en/github/committing-changes-to-your-project/creating-and-editing-commits/changing-a-commit-message
Changing a commit message - GitHub Docs

### Tag current position

A tag is a bit like a branch, but you don't need to check it out. 

```
git tag -a v1.4 -m 'version 1.4'
```


## Branches

### Show current branch

    git branch --show-current

### Show a list of all branches available

    git branch --all

### Switch to existing branch

```
git checkout name_of_branch
```

### Creating a branch

Assumes you have already checked out the repository locally. Then:

```
git checkout -b upstream
```

Which is shorthand for:

```
git branch upstream
git checkout upstream
```

### Delete local branch

Once you merge a feature branch back in to `main`, feel free to remove it. It's a good idea to keep your branch names somewhat clean. 

```
git branch -d <local-branch>
```

If the branch has been shared publicly, and deleted on the remote repository, but still shows up locally (in the list of remote branches), it can be removed with

```
git remote prune origin
```

To preview this action, use

    git remote prune origin --dry-run

### Setting tracking source

If a new local branch has not been pushed up to the origin, use

    git push origin [branch-name]

If you wish to set tracking information for this branch:

```
git branch --set-upstream-to=<remote>/<branch> main
```

### Merging

Allows you to pull in changes from master / different branch.

Use merge if your branch is already pushed.

To merge changes

    git merge origin/main

https://duckduckgo.com/?q=git+import+changes+on+master+to+branch&t=canonical&ia=web
git import changes on master to branch at DuckDuckGo

See also web-ui-api-db/README.md for a branching strategy on handling changes to a foundation that exists outside of the current repository.

### Compare branches

    git diff branch1..branch2

### Renaming a branch

```
git branch -m <oldname> <new_name>
```

If you want to rename the current branch

```
git branch -m <new_name>
```

If the current branch is available publicly, you'll need to rename it on the remote repo as well.

```
# Rename the local branch to the new name
git branch -m <old_name> <new_name>

# Delete the old branch on remote - where <remote> is, for example, origin
git push <remote> --delete <old_name>

# Or shorter way to delete remote branch
git push <remote> :<old_name>

git remote -v
# Push the new branch to remote
git push <remote> <new_name>
# e.g.
git push origin <new_name>

# Reset the upstream branch for the new_name local branch
git push <remote> -u <new_name>
```

https://stackoverflow.com/questions/30590083/how-do-i-rename-both-a-git-local-and-remote-branch-name/30590238#30590238

via:
https://stackoverflow.com/questions/6591213/how-do-i-rename-a-local-git-branch

### Rename master to main

Check out the repository, then

    git branch -m master main
    git checkout main
    git push -u origin main

    git push origin --delete master

This may require updating the HEAD reference on Github directly. For a locally hosted git repository, edit the `HEAD` file on the server. Then, running the following should work:

    git push origin --delete master

```
git branch -m master main
git push -u origin main
```

https://dev.to/afrodevgirl/replacing-master-with-main-in-github-2fjf

Be sure to update defaults in your git server host:

https://stackoverflow.com/questions/30987216/change-default-branch-in-gitlab

### Branch workflows

There are different ways to use branches.

https://guides.github.com/introduction/flow/

https://nvie.com/posts/a-successful-git-branching-model/

https://stackoverflow.com/questions/15072243/git-with-development-staging-and-production-branches

https://stackoverflow.com/questions/24582319/branching-and-merging-best-practices-in-git

#### Preparing a release

When it's time to merge a feature branch to the main branch, a few steps are necessary. Examples include diffing the branches, merging the branches, and creating a pull request in your issue tracker to document the change.  Frequently these step are done directly in a web service like Github. Better integration with issue tracker that way. Notification emails are automatically sent to collaborators. However, if you need to do it locally for some reason:

### Branch from previous commit

You can create the branch via a hash:

```
git branch branchname <sha1-of-commit>
```

Or by using a symbolic reference:

```
git branch branchname HEAD~3
```

https://stackoverflow.com/questions/2816715/branch-from-a-previous-commit-using-git

### Pick up branches from a remote repository

TODO: confirm

    git pull

should do the trick

### Rebasing

Similar to merge, but re-writes the history. Use rebase if your branch is local and hasn't been pushed to origin. However, generally, a merge is just fine!

Never rebase a public branch / master -- makes a mess for others who have it checked out already

    git checkout name_of_branch
    git rebase master

https://stackoverflow.com/questions/5340724/get-changes-from-master-into-branch-in-git
version control - Get changes from master into branch in Git - Stack Overflow

https://www.atlassian.com/git/tutorials/merging-vs-rebasing
Merging vs. Rebasing | Atlassian Git Tutorial

### Pushing Changes

https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging

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

### Fork

A fork is another term for a cloned copy of the repository.

https://stackoverflow.com/questions/3611256/forking-vs-branching-in-github

## Submodules

Submodules are remote repositories that are tracked separately, but needed for the local project to work.

Submodles allow for external dependencies to be noted and included.

### Fetching

To get submodules on clone:

    git clone --recurse-submodules -j8 git://github.com/foo/bar.git
    cd bar

To pull in submodules for an already checked out repository:

    git submodule update --init --recursive

```
hint: You've added another git repository inside your current repository.
hint: Clones of the outer repository will not contain the contents of
hint: the embedded repository and will not know how to obtain it.
hint: If you meant to add a submodule, use:
hint:
hint: git submodule add <url> path/to/mod
hint:
hint: If you added this path by mistake, you can remove it from the
hint: index with:
hint:
hint: git rm --cached path/to/mod
hint:
hint: See "git help submodule" for more information.
```

### Add a submodule

    git submodule add https://gitlab.com/charlesbrandt/public docs

https://git-scm.com/book/en/v2/Git-Tools-Submodules

### Remove a submodule

To remove a submodule you need to:

    Delete the relevant section from the .gitmodules file.
    Stage the .gitmodules changes git add .gitmodules
    Delete the relevant section from .git/config.
    Run git rm --cached path_to_submodule (no trailing slash).
    Run rm -rf .git/modules/path_to_submodule (no trailing slash).
    Commit git commit -m "Removed submodule "
    Delete the now untracked submodule files rm -rf path_to_submodule

https://gist.github.com/myusuf3/7f645819ded92bda6677

### Merging two git repositories

This is also the same scenario as working with an upstream or boilerplate.

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

## Removed Content

### Revert vs Reset

If you need to undo a change from a previous commit, a `revert` is generally preferred over a `reset`. Resets can cause problems with other shared instances of the repository. 

Reminder: It is possible to `revert` a previous `revert` to bring the code back in at a later date. 

https://stackoverflow.com/questions/1616957/how-do-you-roll-back-reset-a-git-repository-to-a-particular-commit
How do you roll back (reset) a Git repository to a particular commit? - Stack Overflow
https://stackoverflow.com/questions/4114095/how-do-i-revert-a-git-repository-to-a-previous-commit
git checkout - How do I revert a Git repository to a previous commit? - Stack Overflow
https://duckduckgo.com/?q=vuetify+autocomplete+rules&t=canonical&ia=web
git undo pull request merge at DuckDuckGo
https://stackoverflow.com/questions/6481575/undo-a-merge-by-pull-request
git - Undo a merge by pull request? - Stack Overflow
https://stackoverflow.com/questions/34638188/how-to-undo-merge-of-master-branch
git - How to undo merge of master branch? - Stack Overflow
https://www.datree.io/resources/git-undo-merge
"Git undo merge" - How to undo merge in git [Tutorial]


### Ignore trivial changes

Ignore changes to a file

    git update-index --assume-unchanged path/to/file

Resume tracking again:

    git update-index --no-assume-unchanged path/to/file

https://stackoverflow.com/questions/13442130/git-temporarily-ignore-trivial-changes-to-files
Git - Temporarily ignore trivial changes to files - Stack Overflow

### Finding a deleted file in history

Sometimes it's easier to use an interface like gitlab (run it locally) or github to browse the history of the commits to the project.

If you do not know the exact path you may use

    git log --all --full-history -- **/thefile.*

If you know the path the file was at, you can do this:

```
git log --all --full-history -- <path-to-file>
```

An alternative:

```
git rev-list -n 1 HEAD -- <file_path>
```

This should show a list of commits in all branches which touched that file. Then, you can find the version of the file you want, and display it with...

```
git show <SHA> -- <path-to-file>
```

Or restore it into your working copy with:

```
git checkout <SHA>^ -- <path-to-file>
```

Note the caret symbol (^), which gets the checkout prior to the one identified, because at the moment of `<SHA>` commit the file is deleted, we need to look at the previous commit to get the deleted file's contents

via:
https://stackoverflow.com/questions/7203515/git-how-to-find-a-deleted-file-in-the-project-commit-history/34681842

https://stackoverflow.com/questions/953481/find-and-restore-a-deleted-file-in-a-git-repository

### Remove directory from history

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

[via](https://stackoverflow.com/questions/10067848/remove-folder-and-its-contents-from-git-githubs-history)

### Changing a commit message


### Clearing a public repo's history

```
-- Remove the history from 
rm -rf .git

-- recreate the repos from the current content only
git init
git add .
git commit -m "Initial commit"

-- push to the github remote repos ensuring you overwrite history
git remote add origin git@github.com:<YOUR ACCOUNT>/<YOUR REPOS>.git
git push -u --force origin main
```

via
https://gist.github.com/stephenhardy/5470814

In Gitlab, you'll need to explicitly allow "Force push" in the project -> settings -> Repository -> Protected Branches. Set "Allowed to force push" to enabled. Once this action is complete, it's a good idea to set this back to disabled. 


## GUI Clients

Knowing the command line interface is useful when you only have a terminal to work with (ssh, termux, etc).

However, there are plenty of cases where a full desktop environment is availabe. In that case, no need to limit yourself to the CLI.

VS Code has a lot of useful git utilities built in.

Github Desktop is a nice GUI client. Github Desktop is only available on Mac and Windows.

Clients available for linux are listed here:

https://git-scm.com/download/gui/linux

Looking at:

https://git-cola.github.io/

    sudo pacman -Ss git-cola
    # not found

    sudo pacman -S gitg
