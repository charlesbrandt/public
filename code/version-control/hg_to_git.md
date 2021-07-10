# Convert Mercurial to Git

Mercurial is a distributed version control system written in python. It has a nice minimal command line interface.

Unfortunately it hasn't been ported to python3 yet. I haven't dug in to the details, but I'm guessing most folks are switching over to git.

So I'm trying to catch up with the times. [2018.08.25 19:03:04] I'm not sure when I started using mercurial for personal repositories, but I've been happy using git in other contexts (work).

According to:
https://stackoverflow.com/questions/16037787/convert-mercurial-project-to-git

You can try using fast-export:

```
cd ~
git clone https://github.com/frej/fast-export.git
git init git_repo
cd git_repo
~/fast-export/hg-fast-export.sh -r /path/to/old/mercurial_repo
git checkout HEAD
```

However, fast-export is not so simple to install these days. (un-maintained?)

I hit the following issue:
https://github.com/frej/fast-export/issues/132

Which I resolved based on this sequence:
```
~$ git clone https://github.com/frej/fast-export.git
~$ cd fast-export
~$ git checkout tags/v180317
~$ cd ..
~$ mkdir new_git_repo
~$ cd new_git_repo
~$ git init
~$ ../fast-export/hg-fast-export.sh -r /old_hg_repo/
```

#old draft:
cd ~/Downloads
mkdir technical
cd technical/
git init .
../fast-export/hg-fast-export.sh -r /c/charles-todo/technical/





## Full process (including merging in with another repo): 

// check for any uncommitted changes in the original mercurial repository first
// export CONVERT=yoga-mala
// export ROOT=/c/out-data/yoga-mala-hg
export CONVERT=brandt
export ROOT=/media/charles/DATA/c-all/brandt/
cd $ROOT
mkdir $CONVERT-git
cd $CONVERT-git/
git init .
~/Downloads/fast-export/hg-fast-export.sh -r $ROOT/$CONVERT/
git checkout HEAD
git rm .hgignore

// via git.md
mkdir $CONVERT
git mv -k * $CONVERT
git commit -m "moving all items to a sub-directory for easier merging"
cd /c/charles
git remote add $CONVERT /c/out-data/2018/hg_to_git-conversion/$CONVERT-git/
git fetch $CONVERT
git merge --allow-unrelated-histories $CONVERT/master

cd /c/charles-todo
mv $CONVERT /c/out-data/2018/hg_to_git-conversion/orignal_hgs/

See also:

//TODO:
//anchors in markdown... reference?
[Merging two git repositories](git.md#merging-two-git-repositories)

## Links

https://duckduckgo.com/?q=pip+python2+ubuntu+18.04&t=canonical&ia=web
pip python2 ubuntu 18.04 at DuckDuckGo
https://linuxconfig.org/how-to-install-pip-on-ubuntu-18-04-bionic-beaver
How to install PIP on Ubuntu 18.04 Bionic Beaver - LinuxConfig.org
https://duckduckgo.com/?q=from+mercurial.scmutil+import+revsymbolImportError%3A+cannot+import+name+revsymbol&t=canonical&ia=web
from mercurial.scmutil import revsymbolImportError: cannot import name revsymbol at DuckDuckGo
https://github.com/frej/fast-export/issues/11
fast-export fails if python3 is sytem-wide default · Issue #11 · frej/fast-export
https://github.com/frej/fast-export/issues/132
revsymbol not found · Issue #132 · frej/fast-export
https://www.viget.com/articles/tips-for-writing-better-bug-reports/
Tips for Writing Better Bug Reports | Viget



https://duckduckgo.com/?q=convert+mercurial+to+git&t=canonical&ia=qa
convert mercurial to git at DuckDuckGo
https://stackoverflow.com/questions/16037787/convert-mercurial-project-to-git
Convert Mercurial project to Git - Stack Overflow
http://hivelogic.com/articles/converting-from-mercurial-to-git/
Converting from Mercurial to Git
https://github.com/frej/fast-export
frej/fast-export: A mercurial to git converter using git-fast-import
