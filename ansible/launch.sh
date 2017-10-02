#!/bin/bash

# launch.py is availble in:
# https://github.com/charlesbrandt/moments

# path to launch.py is defined in:
# ~/.bashrc
# example .bashrc is available in moments/editors/ directory

launch.py -c /c/public/system ansible

echo "

ansible-playbook system.yml -i hosts.txt --ask-become-pass
"

