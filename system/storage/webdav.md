# WebDAV

Web Distributed Authoring and Versioning (WebDAV), an extension to the HTTP-protocol, allows authoring of resources on a remote web server. 

via
https://savannah.nongnu.org/projects/davfs2

## Nautilus File Browser Linux

It's possible to test out the connection to a WebDAV server in the file browser in linux. It's a bit tucked away, but this approach works for other types of network drives too.

(For 22.04) Locate `+ Other Locations` in the left nav panel. At the bottom of the window is an option to `Connect to Server` with an input to `Enter server address...`. The carrot toggle to the right will allow choosing previously used connections. 

The connection string is of the form:

```
davs://user@host.name/path
```

https://askubuntu.com/questions/233242/how-do-i-establish-a-webdav-connection-in-gnome-3

## Mounting

It is possible to mount a WebDAV endpoint as a native filesystem in Linux:

```
sudo apt-get install davfs2
```

Create a directory that you want to use as your mount point, for example myserver:

```
sudo mkdir /media/myserver
```

### Manual mount

Cannot pass in username via the `server_url`, but the command will prompt for credentials (if no matching ones are found in a secrets file) and is a good utility for testing connections

```
sudo mount -t davfs 'https://<server_url>' /mnt/<mount_directory>/
```

https://stackoverflow.com/questions/9871270/mounting-webdav-shares-using-davfs2

### Certificates

If you're using a local NAS and the above command prompts to accept a self sign certificate like this:

```
/sbin/mount.davfs: the server certificate does not match the server name
/sbin/mount.davfs: the server certificate is not trusted
  issuer:      Company Example
  subject:     Example
  identity:    Support
  fingerprint: 
You only should accept this certificate, if you can
verify the fingerprint! The server might be faked
or there might be a man-in-the-middle-attack.
Accept certificate for this session? [y,N] y
```

- Download the certificate (thanks to [elec3647](https://superuser.com/users/245449/elec3647) [on SuperUser](https://superuser.com/a/641396/30127))

```    
openssl s_client -connect HOSTNAME:443 -showcerts </dev/null 2>/dev/null | openssl x509 -outform PEM > certificate.pem
```        
    
- Copy the certificate to `/etc/davfs2/certs/`

```    
sudo cp certificate.pem /etc/davfs2/certs/
```

use the [trust\_server\_cert](http://manpages.ubuntu.com/manpages/utopic/man5/davfs2.conf.5.html) option in the dav2fs config.

> Usefull when the server's certificate can't be verified or is even invalid, but you know that you can trust this certificate.

So edit `/etc/davfs2/davfs2.conf` and add a line that looks like the following:

```
trust_server_cert        /etc/davfs2/certs/my.selfsigned.cert.pem
```    

Allows mounting a self-signed OwnCloud webdav even when the certificate host didn't match.


https://askubuntu.com/questions/488812/automating-a-mount-operation-that-requires-user-input


### Authentication

Open `~/.davfs2/secrets` (per user) or `/etc/davfs2/secrets` (system wide)

Find the credential line section

Add a line with the server ip, the username you need to login to the webDAV share and the password in the following order:

```
myserverip webdavusername webdavpassword
```

Confirm the correct permissions on `~/.davfs2/secrets` or `/etc/davfs2/secrets`:

```
chmod 600 ~/.davfs2/secrets
```

or

```
sudo chmod 600 /etc/davfs2/secrets
```


### Automatic mount

Add the following line to /etc/fstab:

```
myserverip /media/myserver/ davfs noauto,user,rw 0 0
```

Change myserverip to the IP address or domain name and port of your webDAV server.

TODO: Needs troubleshooting


### Non-root users

run sudo dpkg-reconfigure davfs2 and select yes to allow mounting by non-root users.

check that in `/etc/davfs2/davfs2.conf` you have:

```
dav_user        davfs2           # system wide config file only
dav_group       davfs2           # system wide config file only
```

add your user (my username in this example) to the davfs2 group:

```
sudo usermod -aG davfs2 myusername
```

You can check that it you have been added to the group by running groups myusername:

myusername : myusername davfs2

Reboot or log out to let the group changes come in effect.

You should now be able to mount your share by clicking on it in your file browser.

via:
https://askubuntu.com/questions/498526/mounting-a-webdav-share-by-users
