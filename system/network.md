# Networking

The way computers talk to one another. 

Every computer on a network has it's own address. [IP address -- Internet Protocol](https://en.wikipedia.org/wiki/IP_address)

There are ip address ranges that are designated for internal (private) networks.

192.168.1.x is a common one. 

## Documenting

What's a good layout for ip assignments?
Example documentation

Just use `/etc/hosts` and keep it around in your configurations repo (e.g. `~/alpha/system/hosts`)

## Interface Configuration

You need to know a few details about your local network before you configure a static ip. 

Network IP range (TODO: network class, CIDR notation)
Available IP
Gateway
DNS 


Often it's pretty straightforward to use a GUI. 

If you want to configure an interface via a CLI, it's necessary to know where the OS stores the configuration settings. This varies from OS to OS. 


## Prerequisites (Pi)

On a Raspberry Pi, disable cloud config:
To disable cloud-init's
network configuration capabilities, write a file
`/etc/cloud/cloud.cfg.d/99-disable-network-config.cfg`
with the following:

```
network: {config: disabled}
```

## NetworkManager

NetworkManager is the system... well... umm.. managing the network. 

https://networkmanager.dev/docs/admins/  
NetworkManager for administrators  
https://en.wikipedia.org/wiki/NetworkManager  
NetworkManager - Wikipedia  
https://www.networkmanager.dev/  
NetworkManager  
https://www.networkmanager.dev/docs/  
Documentation  


It has a cli: `nmcli` that can be used to modify the network configuration. 

This is what allows you to enable wifi on a machine with only a CLI. 
  
https://networkmanager.dev/docs/api/latest/nmcli.html  
nmcli: NetworkManager Reference Manual  

https://www.tecmint.com/nmcli-configure-network-connection/  
How to Configure Network Connection Using 'nmcli' Tool  
https://www.makeuseof.com/configure-static-ip-address-settings-ubuntu-22-04/  
How to Configure Static IP Address on Ubuntu 22.04 LTS  
https://www.makeuseof.com/connect-to-wifi-with-nmcli/  
How to Connect to Wi-Fi Through the Linux Terminal With Nmcli  


### Interfaces


`nmcli` is very convenient for seeing the status of the network. Dare I say better than `ip address`?

```
nmcli
```


An alternative way to find the interface in use:

```
ip address
```

Use the name that comes after the number, for example:

```
2: eno1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UP group default qlen 1000
```

`eno1` would be the adapter name. 


Don't assume `eth0` will just work.


### Netplan

https://netplan.io/  
Canonical Netplan  

Netplan configuration files are located in `/etc/netplan`

When editing `yaml` files, it's easy to make syntax errors. To detect them, you'll want a linter:

```
sudo apt install yamllint

yamllint /etc/netplan/01-netplan.yaml
```

https://unix.stackexchange.com/questions/681220/netplan-generate-gateway4-has-been-deprecated-use-default-routes-instead


Create a new netplan configuration. 

```
cd /etc/netplan
sudo vi 02-main-nic.yaml
```

add a section like: 

```
network:
  ethernets:
    eno1:
      dhcp4: false
      addresses: [192.168.1.234/24]
      routes:
        - to: default
          via: 192.168.1.1
      nameservers:
        addresses: [8.8.8.8, 4.2.2.2]

```

To apply the configuration and have changes take effect, run:

```
sudo netplan apply 
```

[adapted via](https://getlabsdone.com/static-ip-configuration-in-ubuntu-using-cli-gui/)

https://netplan.io/examples/  
Netplan | Backend-agnostic network configuration in YAML  


### GUI

https://www.google.com/search?q=ubuntu+gnome+3+change+IPs  
ubuntu gnome 3 change IP - Google Search  
https://www.lifewire.com/how-to-configure-networking-in-gnome-4682592  
How to Configure Networking in GNOME  




## Connections & Troubleshooting

Ping the gateway. If that doesn't work, the configuration is incorrect. Check your settings, your network addresses, etc (is it on a different subnet?)

```
ping 192.168.1.1
```


### netstat

On a linux machine you can install `netstat` to see what ports are currently open:

```
sudo apt install net-tools

netstat -plan 

netstat -pan | egrep " LISTEN "
    
netstat -tulnp
```

  - t – Show TCP
  - u – Show UDP
  - l – Show only listening processes (netstat can show both listening and all established connections, i.e. as a client too)
  - n – Do not resolve network IP address names or port numbers
  - p – Show the process name that is listening on the port

Similar to `netstat`, but the focus is on processes:

```
ss -nutlp
    
lsof -i
```

via:   
https://www.thegeekdiary.com/centos-rhel-how-to-find-if-a-network-port-is-open-or-not/  

### nmap
    
To scan open ports from another (external) machine that's on the same network

```
nmap [ip of machine to scan]
```

A few other tools could help 

```
sudo apt-get update
sudo apt-get install nmap netdiscover arp-scan
```

### DNS

To resolve a name associated with an IP address, try nslookup:

```
nslookup 129.79.5.100
```

To go the other way and find the IP of a configured domain name, `dig` can help:

```
dig +short unix.stackexchange.com
```

Don't forget!
You can always add the host & ip to your `/etc/hosts` file and then it will resolve and you can test the service before the dns entries propagate! :)


### Traceroute

```
sudo apt install inetutils-traceroute 
```


## Firewall, Security, Ports

See also [Configure a firewall](firewall.md)

### Common Ports

By convention, common services utilize specific ports to publish and connect to the service. Some examples include:

SSH            22
DNS servers    53	 tcp	potential trojan (probably dns)
ipps	       631	        Internet Printing Protocol over HTTPS


### ufw

Firewalls block external traffic from entering internal networks and hosts. 

Ubuntu uses `ufw`. See if it's running:

```
sudo ufw status
```

`ufw` is disabled by default. Enable this first on a new host machine! :)

```
sudo ufw enable
```

Then allow the ports that you want to be accessible on your local network 

```
sudo ufw allow 22
```

https://ubuntu.com/server/docs/security-firewall

### iptables

See what netfilter rules have been applied with `iptables` tool

```
iptables -xvn -L
```

### VPN

Wireguard is now built in to most modern linux kernels. Give that a try. On the server

```
curl -O https://raw.githubusercontent.com/angristan/wireguard-install/master/wireguard-install.sh
chmod +x wireguard-install.sh

sudo ./wireguard-install.sh
```

If your VPN server will be responding via a NAT'd IP address, use that when configuring the VPN's "IPv4 or IPv6 public address" so clients know where to go to contact the server. 

At the end of the process, the script will ask you for the name of the **clients** that will be connecting to the VPN. These names need to be less than 15 characters. It will also create a QR code in the terminal (cool!)

Note where the config is saved for transfer to the clients. From the client:

```
rsync -av account@server:/home/account/wg0-client-machine_name.conf wg0.conf
```

These need to go into `/etc/wireguard`

```
sudo cat /etc/wireguard/wg0.conf
```

Clients need a few things to be installed first:

```
sudo apt-get install wireguard
```

On Ubuntu, I needed the following symlink before `wg-quick` works

```
ln -s /usr/bin/resolvectl /usr/local/bin/resolvconf
```
[via](https://superuser.com/questions/1500691/usr-bin-wg-quick-line-31-resolvconf-command-not-found-wireguard-debian)


Then, on the client, you can start the connection with

```
sudo wg-quick up wg0
```




Back on the server, you can check the status of WireGuard with: 

```
systemctl status wg-quick@wg0
```

If you have a firewall enabled, note the selected port in the configuration and allow it in the firewall rules:

```
sudo ufw allow 59984
```

Note: I tried limiting the traffic so that only local IPs would be routed. This didn't work for me:

```
AllowedIPs = 192.168.0.0/24, 192.168.2.0/24
```

You may also need to forward the port in any upstream routers. 


https://github.com/angristan/wireguard-install

https://www.makeuseof.com/vpn-wireguard/

https://github.com/firezone/firezone  
GitHub - firezone/firezone: WireGuard®-based VPN server and firewall  



https://www.freecodecamp.org/news/subnet-cheat-sheet-24-subnet-mask-30-26-27-29-and-other-ip-address-cidr-network-references/


#### IP Forwarding

```
sudo vi /etc/sysctl.conf
```

Uncomment line:

```
net.ipv4.ip_forward=1
```

Save and apply with: 

```
sudo sysctl -p
```

#### VPN Client

On Android, download the wireguard client and use QR codes to configure (nice!)

On linux, install wireguard:

```
sudo apt-get update && sudo apt-get upgrade
sudo apt-get install wireguard
```

and transfer the config over from your server:

```
cd /etc/wireguard/
sudo rsync account@server:/home/account/wg0-client-name.conf wg0.conf
```

Bring up the client connection

```
sudo wg-quick up wg0
```

Note: On ubuntu, I needed to create a symlink for `resolvectl`

```
ln -s /usr/bin/resolvectl /usr/local/bin/resolvconf
```

https://superuser.com/questions/1500691/usr-bin-wg-quick-line-31-resolvconf-command-not-found-wireguard-debian

See the connection status:

```
sudo wg show
```

Once finished, close the client connection

```
sudo wg-quick down wg0
```

How to Install WireGuard VPN Client on Ubuntu Linux | Serverspace  
https://serverspace.io/support/help/how-to-install-wireguard-vpn-client-on-ubuntu-linux/  



## Traffic Analysis

### Wireshark

To see what is happening on a network, use wireshark

https://www.wireshark.org/  
Wireshark · Go Deep.  

```
sudo apt install wireshark
```

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


### Related Tasks

[Copy SSH keys to the new machine](terminal/ssh.md)




