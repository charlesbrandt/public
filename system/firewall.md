# Firewall

Additional firewall notes are summarized in [network notes](network.md). 

For linux, iptables is a tried-and-true solution for a system firewall. Included by default in debian / ubuntu. 

https://www.digitalocean.com/community/tutorials/iptables-essentials-common-firewall-rules-and-commands

See currently configured rules

```
iptables -nvL | less
```

Netfilter is the official name for the project that maintains `iptables` and its successor, `nftables`

https://www.netfilter.org/

https://github.com/trimstray/iptables-essentials  
trimstray/iptables-essentials: Iptables Essentials: Common Firewall Rules and Commands.  
https://major.io/2010/04/12/best-practices-iptables/  
Best practices: iptables | Major Hayden  
https://duckduckgo.com/?t=ffab&q=iptables+init+&ia=web  
iptables init at DuckDuckGo  
https://github.com/BorX/iptables-init  
BorX/iptables-init: Easy initialization iptables rules  
https://github.com/topics/iptables  
iptables · GitHub Topics  

https://github.com/topics/firewall  
firewall · GitHub Topics  


https://docs.opnsense.org/development/architecture.html
Architecture — OPNsense documentation
https://github.com/opnsense/core
GitHub - opnsense/core: OPNsense GUI, API and systems backend

https://ubuntu.com/server/docs/security-firewall
Security - Firewall | Ubuntu

