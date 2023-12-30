# Password Managers

It's a good idea to use a different password for different services. That way, if one of those services is compromised, it only affects that service. 

If you use the same password everywhere, then you should change it everywhere after a security breach. Probably not going to happen. 

If you use different passwords for different services, it becomes impossible to memorize all of them. Password managers help track all of the different passwords in a secure fashion. 

Encrypting a collection of passwords is necessary if you're going to store them digitally. Good solutions exist for helping you meet this goal. 


## KeePass

https://keepass.info/  
KeePass Password Safe  

KeePass is one of the older open-source solutions for tracking passwords. There are clients available for most platforms. 

KeePass stores the encrypted password database locally. The tricky part is keeping your key database in sync across multiple devices. Keepass doesn't have that functionality built in. I prefer to keep the database file stored locally rather than in a cloud based service. I use git to synchronize it across devices. 

https://github.com/lgg/awesome-keepass

### Desktop

KeepassXC is an updated fork of KeepassX. KeepassX is a good cross platform option. 

https://keepassxc.org/

```
sudo apt install keepassxc
```

To copy a password to the system clipboard, do not open the entry details. Ctrl-C is enabled on the find / filter list. It's not available in the details. 

### Merging databases

It is possible to open one copy of a database and then merge in another one via File -> Synchronize -> Synchronize with File

Very useful in the event of a git merge conflict.

https://www.ghacks.net/2022/03/25/how-to-merge-two-keepass-databases/

### Disable expiration

https://keepass.info/help/v2_dev/customize.html  
0x8	8 	Disable controls to specify after how many days the master key should/must be changed.  


### Android

KeePass DX is good and open source.

KeePassXC looks like another good client  
https://keepassxc.org/  

### Web

KeeWeb may be a good option to try. Found it via the Open Collective site. 

https://opencollective.com/keeweb

### Older versions

https://www.keepassx.org/
 
```
sudo apt-get install keepassx
```

KeePass.info seems to be the official desktop client

https://keepass.info/  
KeePass Password Safe  


### Others

Don't like this one as much. 

```
sudo apt-get install keepass2
```




## Bitwarden

Open Source password manager

https://bitwarden.com/

https://github.com/bitwarden

Uses a dedicated server that you can run locally to synchronize the password database. 

Written in C#
Docker images are provided for the server, and there are also lighter weight clones in Ruby that are available  
https://sgoel.org/posts/switching-from-keepassxc-to-bitwarden/  

Seems like a great solution that may be easier to get started with for most people compared to KeePass. 

TechRadar: Popular password manager could have a critical vulnerability.  
https://www.techradar.com/news/popular-password-manager-could-have-a-critical-vulnerability  


## Lockwise

Lockwise is a newer open source solution from Mozilla that is built in to Firefox:

https://www.mozilla.org/en-US/firefox/lockwise/  
Firefox Lockwise — password manager — take your passwords everywhere  

It looks like there may be some issues with prompting for the password to unlock the database.

https://medium.com/@JoeKreydt/how-secure-is-firefox-lockwise-password-manager-51d44dcf4dbc  
How secure is Firefox Lockwise password manager? | by Joe Kreydt | Medium  

Here is the open issue to track the fix:

https://github.com/mozilla-lockwise/lockwise-ios/issues/1105

https://duckduckgo.com/?q=Lockwise&t=canonical&ia=web  
Lockwise at DuckDuckGo  
https://duckduckgo.com/?q=lockwise+vs+lastpass&t=canonical&ia=web  
lockwise vs lastpass at DuckDuckGo  


## Others

open source password manager - searches

LastPass is a popular, well regarded closed / commercial / free option.  
https://thenextweb.com/basics/2019/08/25/dont-be-an-idiot-heres-how-to-store-and-remember-all-your-passwords/  

https://padlock.io/  
Padlock - A Minimalist Password Manager  
https://github.com/padlock/padlock  
padlock/padlock: A minimalist open source password manager.  

https://www.passbolt.com/  
Passbolt | Open source password manager for teams  

https://hackernoon.com/the-best-password-manager-for-you-747b92c43d18  
The Best Password Manager for You - HackerNoon.com  

https://www.passwordstore.org/  
Pass: The Standard Unix Password Manager  

