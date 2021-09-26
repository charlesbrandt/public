# Local Development

Running an application stack locally for development is a convenient way to work. With container solutions, it's possible to describe the environment in a way that makes it easy to replicate on other systems. 

## Localhost

One gotcha that comes up is what address to use to serve the applications. `localhost` and `127.0.0.1` are obvious choices. Ultimately this is where the local traffic gets routed anyway.

**However**, these are not as easy as they first seem. 

The first complication is with self-signed certificates. You can [generate them](/web/certificates.md), but some browsers won't accept them as valid (e.g. Chrome). 

The second (an potentially bigger) complication is when testing is added. If the testing process is run in its own container, it will need to know how to access the service. If `localhost` is the name in use, then the testing container will refer to the local testing container, which won't actually be running the service. 

## Aliases

If your development machine has a publicly accessible DNS name, it already has an alias (the domain name). 

Even when developing locally, using a local unique host alias is preferred. 
(TODO: link to github issue in the cypress project on this topic). 

The format in `/etc/hosts` is

```
[IP address]  [URL]
```

*However*, it seems that browsers (Firefox, Chromium) are not seeing names defined in `/etc/hosts`. Maybe they skip straight to dns resolution??? 

Seems to require using the local host as dns server. If the dns server is configured manually to be external, then browsers will bypass local name resolution. 



Docker aliases seems like a good choice within the container network. 

```
services:
        
  nginx:
    ...
    networks:
      default:
        aliases:
          - myapi.docker
          - docker_my_network
```


As long as the alias used on the host is the same as the one used in the docker context, everything should line up. 

https://docs.docker.com/compose/networking/

https://stackoverflow.com/questions/47577490/resolve-container-name-in-extra-hosts-option-in-docker-compose


A local DNS server is another option, but it may be overkill for this purpose. 


### Ports

Even if aliases are the same at the host and container contexts, using different ports requires the service to be available on that port both inside and outside of the container context. Using one port on the host localhost and a different port within the container context is a solution I was fond of. It allows different services to be available on a dedicated port, and potentially running more than one at a time.  May not be viable once a testing solution is in the mix. 

## Troubleshooting

Name aliases are resolved in a number of different ways. 

Browsers generally use DNS directly

```
cat /etc/resolv.conf 
```

In Ubuntu, this is controlled by a link. By default it points to:

```
sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
```

In some environments it's necessary to update this link (only on remote servers?):
~/public/system/virtualization/docker.md

https://serverfault.com/questions/118923/difference-between-etc-hosts-and-etc-resolv-conf

If `/etc/resolv.conf` only references an external DNS server, browsers may not be able to locate hosts defined in `/etc/hosts`. 

If `/etc/resolv.conf` references the default local DNS cache, browsers should resolve the local aliases as expected:

```
nameserver 127.0.0.53
options edns0 trust-ad
```

`/etc/nsswitch.conf` specifies the order of sources that applications should use to resolve a given hostname. The default order should work as expected. 

The default for host name resolution is:

```
hosts:          files mdns4_minimal [NOTFOUND=return] dns
```

`files` should mean that `/etc/hosts` is referenced first. 


## Automatic Startup

decide what services to start at boot
containers are good

restart - always should be used with care though
use with all or none-- doesn't make sense to only keep a db running (from a now inactive project)

decide what ports to use for which services

## See also

[docker](/system/virtualization/docker.md)  
[docker-compose](/system/virtualization/docker-compose.md)  


