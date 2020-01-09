sudo apt install ansible

ansible-playbook system.yml -i hosts.txt --ask-become-pass

ansible-playbook system-2.yml -i hosts.txt --ask-become-pass

# 
looking for an easier way to run
don't want to have to update hosts
but eventually I know that is necessary

https://stackoverflow.com/questions/33222641/override-hosts-variable-of-ansible-playbook-from-the-command-line


#2018 installed
ansible-playbook python3.yml -i hosts.txt --ask-become-pass
# won't work if there are local changes
# this is a good chance to update and commit repos
#ansible-playbook repositories-python3.yml -i hosts.txt --ask-become-pass

# diff .bashrc /c/public/moments/editors/.bashrc
# if the repos script doesn't finish,
# be sure to link the .bashrc file (make changes in /c/public/moments/editors/.bashrc as needed)
rm ~/.bashrc
ln -s /c/public/moments/editors/.bashrc ~/.bashrc 


cd /c/public/moments
#/c/public/moments$ sudo pip3 install -e .
cd ../medley/
#/c/public/medley$ sudo pip3 install -e .
ansible-playbook utilities.yml -i hosts.txt --ask-become-pass
ansible-playbook desktop-main.yml -i hosts.txt --ask-become-pass
