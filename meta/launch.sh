#!/bin/bash

# launch.py is availble in:
# https://github.com/charlesbrandt/moments

# path to launch.py is defined in:
# ~/.bashrc
# example .bashrc is available in moments/editors/ directory

launch.py -c /c/public/system linux

echo "see also:

launch.py -c /c/public/system system

launch.py -c /c/public/system macosx

launch.py -c /c/public/system ansible

ansible-playbook system.yml -i hosts.txt --ask-become-pass
"

