# Networking

The way computers talk to one another. 

Every computer on a network has it's own address. [IP address -- Internet Protocol](https://en.wikipedia.org/wiki/IP_address)

https://www.google.com/search?q=ubuntu+gnome+3+change+IPs  
ubuntu gnome 3 change IP - Google Search  
https://www.lifewire.com/how-to-configure-networking-in-gnome-4682592  
How to Configure Networking in GNOME  

There are ip address ranges that are designated for internal (private) networks.

192.168.1.x is a common one. 

## Interface Configuration

Often it's pretty straightforward to use a GUI. If you want to configure an interface via a CLI, it's necessary to know where the OS stores the configuration settings. This varies from OS to OS. 

### Ubuntu

Find the interface in use

    ip address

enp0s25

The netplan configuration is located in /etc/netplan

    cd /etc/netplan
    
    sudo cp 01-network-manager-all.yaml 01-network-manager-all.yaml.bak
    
Modify the netplan configuration. 

    sudo vi 01-network-manager-all.yaml

add a section like: 

```
network:
  ethernets:
    eth0:
      dhcp4: no
      addresses: [192.168.1.200/24]
      gateway4: 192.168.1.1
      nameservers:
        addresses: [8.8.8.8,4.2.2.2]
```

Then

    sudo netplan apply 

to apply the configuration and changes to affect.

[adapted via](https://getlabsdone.com/static-ip-configuration-in-ubuntu-using-cli-gui/)


[Copy SSH keys to the new machine](/public/system/ssh.md)


## Remote Connections

Ngrok looks like a cool service that can expose a local service via a remotely accessible address

Following along with:

https://www.endtoend.ai/tutorial/ngrok-ssh-forwarding/

Sign in: https://dashboard.ngrok.com/signup

On the server with the service you want to access, 

```
wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-arm64.zip
unzip /path/to/ngrok.zip
chmod +x ngrok

```

or

```
sudo snap install ngrok
```

Start up the forwarding

```
ngrok tcp 22
```

Connect to the service from any remote system:

```
ssh <YOUR_USERNAME>@0.tcp.jp.ngrok.io -p 11111
```


## Open Ports

On a linux machine you can install `netstat` to see what ports are currently open:

    sudo apt install net-tools

    netstat -pan | egrep " LISTEN "
    
    netstat -tulnp
    
t – Show TCP
u – Show UDP
l – Show only listening processes (netstat can show both listening and all established connections, i.e. as a client too)
n – Do not resolve network IP address names or port numbers
p – Show the process name that is listening on the port

Similar to `netstat`, but the focus is on processes:

    ss -nutlp
    
    lsof -i

via: 
https://www.thegeekdiary.com/centos-rhel-how-to-find-if-a-network-port-is-open-or-not/

    
To scan open ports from another (external) machine that's on the same network

    nmap [ip of machine to scan]

### Common Ports

By convention, common services utilize specific ports to publish and connect to the service. Some examples include:

SSH            22
DNS servers    53	 tcp	potential trojan (probably dns)
ipps	       631	        Internet Printing Protocol over HTTPS


## Firewall

Firewalls block external traffic from entering internal networks and hosts. 

iptables

    iptables -xvn -L


## DNS

To resolve a name associated with an IP address, try nslookup:

    nslookup 129.79.5.100

https://kb.iu.edu/d/ackg


## Traceroute

    sudo apt install inetutils-traceroute 




## Traffic Analysis

### Wireshark

To see what is happening on a network, use wireshark

https://www.wireshark.org/
Wireshark · Go Deep.

    sudo apt install wireshark

https://jvns.ca/blog/2018/06/19/what-i-use-wireshark-for/

To see statistics on TCP connection duration:

'Statistics' -> 'Conversations'

### Other tools

https://www.reddit.com/r/networking/comments/78mtfj/looking_for_an_open_source_network_traffic/
Looking for an open source Network Traffic Analyzer : networking

https://github.com/robcowart/elastiflow
GitHub - robcowart/elastiflow: Network flow analytics (Netflow, sFlow and IPFIX) with the Elastic Stack
http://pmacct.net/
pmacct project: IP accounting iconoclasm
https://gitlab.com/thart/flowanalyzer
Manito Networks / flowanalyzer · GitLab
https://www.ntop.org/
ntop – High Performance Network Monitoring Solutions based on Open Source and Commodity Hardware.


