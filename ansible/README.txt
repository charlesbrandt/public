sudo apt install ansible

ansible-playbook system.yml -i hosts.txt --ask-become-pass

ansible-playbook system-2.yml -i hosts.txt --ask-become-pass


#2018 installed
ansible-playbook python3.yml -i hosts.txt --ask-become-pass
# won't work if there are local changes
# this is a good chance to update and commit repos
#ansible-playbook repositories-python3.yml -i hosts.txt --ask-become-pass
cd /c/public/moments
#/c/public/moments$ sudo pip3 install -e .
cd ../medley/
#/c/public/medley$ sudo pip3 install -e .
ansible-playbook utilities.yml -i hosts.txt --ask-become-pass
ansible-playbook desktop-main.yml -i hosts.txt --ask-become-pass
