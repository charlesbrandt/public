# Userland

This process is very similar to getting going with termux, but there are some differences. 

A physcial keyboard makes things a lot easier!

Early on, before you have a text editor available, it can be handy to use the script command to log what you're doing:

    script -a test.txt


## Extras

So far I haven't seen anything like Termux extras for userland. This means the filesystem won't be able to access any shared spaces on android. 

## Core tools

    apt-get update

install git:

    sudo apt-get install git

install emacs:

    sudo apt-get install emacs
     

Check out repos locally. You'll need to use a git server to sychronize repositories. For ones that are public, put them in a public directory

    mkdir public

For now there aren't any other apps that are making use of these files. No need to bury them deep (from Termux's home perspective) in the shared/ folder.

    git clone --recurse-submodules https://github.com/charlesbrandt/system public/system


    ln -s public/system/editors/emacs/.emacs.d/ .emacs.d
    ln -s public/system/editors/emacs/.emacs .emacs


## SSH

This is a good chance to generate ssh keys to simplify connecting to primary servers:

    ssh-keygen -t rsa
    ssh-copy-id demo@198.51.100.0

## Rsync

Another good one to have

    sudo apt-get install rsync

## Sqlite3

    sudo apt-get install sqlite
    
Then can run it with command

    sqlite3
    
https://sqlite.org/cli.html

    .databases
    .tables
    
## Node

If you haven't done this earlier, 

    sudo apt install build-essential

### Nodesource

    # maybe it's a problem with version 12 of node? experienced the same behavior as NVM... going to try 10
    // curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
    
    curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
    
    sudo apt-get install -y nodejs

via:

https://linuxize.com/post/how-to-install-node-js-on-ubuntu-18.04/

### Native packages

Trying:

    sudo apt-get install nodejs
    
This has tons of dependencies... bailing on this approach:

    sudo apt-get install npm

### NVM


Still had difficulty (2019.08.23) installing node with nvm. Trying out base packages.

    sudo apt-get install curl

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash
 
    nvm install node

## Python

pkg install python
pip install --upgrade pip
python -V
Python 3.7.3
$ pip install ipython

pip install pipenv






## Upgrades

Don't forget to run (TODO: test this)

    apt list --upgradable
    
and
  
    sudo apt-get upgrade
    
every now and again

