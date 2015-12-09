#!/bin/bash

python /c/mindstream/mindstream/launch.py -c /c/system system

echo "see also:

python /c/mindstream/mindstream/launch.py -c /c/system macosx

python /c/mindstream/mindstream/launch.py -c /c/system ansible

ansible-playbook system.yml -i hosts.txt --ask-become-pass
"

