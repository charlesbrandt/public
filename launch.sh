#!/bin/bash

# launch.py is availble in:
# https://github.com/charlesbrandt/mindstream

# path to launch.py is defined in:
# ~/.bashrc
# example .bashrc is available in:
# https://github.com/charlesbrandt/moments
# in editors directory

python /c/mindstream/mindstream/launch.py -c /c/public/system system

echo "see also:

python /c/mindstream/mindstream/launch.py -c /c/public/system macosx

python /c/mindstream/mindstream/launch.py -c /c/public/system ansible

ansible-playbook system.yml -i hosts.txt --ask-become-pass
"

