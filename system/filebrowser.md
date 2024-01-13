# File Browser

A good solution to quickly and easily share locally available files via the network that the local server has access to. 

Includes local user accounts. 

https://filebrowser.org/
Welcome - File Browser

## Installation

https://filebrowser.org/installation

### Docker

In project directory:

```
mkdir filebrowser
touch filebrowser/filebrowser.db
touch filebrowser/.filebrowser.json
```

Make sure `.filebrowser.json` is valid (configure as needed): 

```
{
  "port": 80,
  "baseURL": "",
  "address": "",
  "log": "stdout",
  "database": "/database.db",
  "root": "/srv"
}
```

Ignore the database in your `.gitignore` file (don't want credentials in version control):

```
filebrowser/filebrowser.db
```


Then add the following block to your project's `docker-compose.yml` file:

```
  filebrowser:
    image: filebrowser/filebrowser
    container_name: boilerplate_filebrowser_1
    restart: unless-stopped
    volumes:
      - /media/account:/srv
      - ./filebrowser/filebrowser.db:/database.db
      - ./filebrowser/.filebrowser.json:/.filebrowser.json
    #--user $(id -u):$(id -g)
    ports:
      - 9999:80

```

### Local

This approach requires a way to start automatically at boot:

```
curl -fsSL https://raw.githubusercontent.com/filebrowser/get/master/get.sh | bash
filebrowser -r /path/to/your/files
```

### Local Service

To run a local instance outside of Docker, create a custom service:

cat /etc/systemd/system/filebrowser.service

```
[Unit]
Description=Filebrowser
After=network-online.target

[Service]
User=account
Group=account

ExecStart=/usr/local/bin/filebrowser -r /media/account/ -a 192.168.0.100 -p 9999 -d /home/account/filebrowser.db

[Install]
WantedBy=multi-user.target
```



## Running

https://filebrowser.org/cli/filebrowser

address
-a
address to listen on

port
-p
port to listen on

filebrowser -r /path/to/your/files -a 192.168.0.100 -p 9999

## Credentials

Track credentials in keepass -> Filebrowser entry

By default the service starts with the following.

   - Username: admin
   - Password: admin

Change this!!

May be possible to script the setup as needed:

https://filebrowser.org/cli/filebrowser-users

`filebrowser users`

Create new admin account per your requirements. 

Good commands:

```
git unzip
```

Update password for admin (or disable / delete account)

https://filebrowser.org/configuration/authentication-method

It is also possible to disable authentication with

    filebrowser config set --auth.method=noauth



## Links

https://github.com/filebrowser/filebrowser
GitHub - filebrowser/filebrowser: 📂 Web File Browser

https://filebrowser.org/installation
Installation - File Browser
https://filebrowser.org/configuration/authentication-method
Authentication Method - File Browser
https://filebrowser.org/cli/filebrowser-config-set
filebrowser config set - File Browser
https://github.com/filebrowser/filebrowser/blob/master/frontend/src/views/Files.vue
filebrowser/Files.vue at master · filebrowser/filebrowser · GitHub
https://github.com/filebrowser/filebrowser
GitHub - filebrowser/filebrowser: 📂 Web File Browser

https://filebrowser.org/
Welcome - File Browser

https://github.com/filebrowser
File Browser · GitHub


https://github.com/filebrowser/filebrowser/blob/master/.docker.json
filebrowser/.docker.json at master · filebrowser/filebrowser · GitHub
https://hub.docker.com/r/filebrowser/filebrowser
filebrowser/filebrowser - Docker Image | Docker Hub

https://github.com/filebrowser
File Browser · GitHub
https://filebrowser.org/cli/filebrowser-users
filebrowser users - File Browser



