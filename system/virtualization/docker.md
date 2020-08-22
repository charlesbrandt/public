# Docker

Docker implements a container solution. Containers are a lighter weight alternative to a full virtual machine. They are run on the host operating system, but they are encapsulated to provide isolation, security, and compartmentalization. 

Docker has great documentation:
https://docs.docker.com/

Engine is the core of it all:
https://docs.docker.com/engine/

and this is a great overview:
https://docs.docker.com/engine/understanding-docker/

https://docs.docker.com/engine/userguide/

This cheat sheet is a great overview, and closely resembles what these notes cover:
https://github.com/wsargent/docker-cheat-sheet


## See Also

[Docker Compose](docker-compose.md)


## Installation

https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository

```
sudo apt-get update

sudo apt-get install apt-transport-https ca-certificates curl gnupg-agent software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo apt-key fingerprint 0EBFCD88
```

This command will look up the version of your os:

```
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"

```

update again

```
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io

```

At this point Docker should be installed and you can verify with:

    sudo systemctl status docker


Make sure docker is installed:

    docker ps


This guide also has a nice introduction to what containers are:

https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-18-04

### Add user to docker group

This allows you to execute docker without using sudo
that's a good thing!



```
sudo groupadd docker

sudo usermod -aG docker ${USER}
```

Log out and log back in, or:

    su - ${USER}

### Snap / Distro (not recommended)

Command 'docker' not found, but can be installed with:

sudo snap install docker     # version 18.09.9, or
sudo apt  install docker.io

(those tend to be pretty old!)


## Status

To see a list of *currently running* docker containers:

    docker ps
    
To see the name of the container (and the size of disk in use):

    docker ps -s 
    
https://docs.docker.com/engine/reference/commandline/ps/


## Images

See a list of available docker images (see what is currently available):

    docker image ls -a

Is equivalent to:

    docker images

docker images are stored in:

    /var/lib/docker
    
(don't forget, docker sometimes run in a VM, so this location is on that VM)
via:
http://stackoverflow.com/questions/19234831/where-are-docker-images-stored-on-the-host-machine

Clear everything out (!!! dangerous !!!)

    docker image rm $(docker image ls -a -q)
    docker image rm -f $(docker image ls -a -q)

### Official Images

https://hub.docker.com/search?q=&type=image&image_filter=official

Different dependencies will result in different sized containers. Smaller is generally better, everything else being the same:

https://www.brianchristner.io/docker-image-base-os-size-comparison/

https://docs.docker.com/docker-hub/official_images/

### Dockerfile

A Dockerfile determines how your image is configured (and ultimately what is run in your container). These can be tracked as part of the project's source code. 

https://docs.docker.com/get-started/part2/

```
# Use the official image as a parent image.
FROM node:current-slim

# Set the working directory.
WORKDIR /usr/src/app

# Copy the file from your host to your current location.
COPY package.json .

# Run the command inside your image filesystem.
RUN npm install

# Inform Docker that the container is listening on the specified port at runtime.
EXPOSE 8080

# Run the specified command within the container.
CMD [ "npm", "start" ]

# Copy the rest of your app's source code from your host to your image filesystem.
COPY . .
```

https://docs.docker.com/develop/develop-images/dockerfile_best-practices/

### Building

    docker build -t simple-node .
    docker run -p 3000:3000 simple-node

Now you should be able to connect to localhost without specifying a VM host. Without the explicit forward for the port, the port won't be available:

    http://localhost:3000/
    
To specify a Dockerfile, use -f:

    docker build -t simple-node -f Dockerfile.debug .
    
https://docs.docker.com/engine/reference/commandline/build/

When it comes time to build a docker image (so you can ultimately deploy a running docker container), there are a few different ways to create the image:

 - use a dockerfile to create a new image (recommended process)
 - start a container, make some changes, and then commit that to a new image
 - use ansible to build the image
    - could install ansible using the docker file and then run the ansible playbook via the docker file
    - could use docker to connect to a running container (either using docker connection, or by instantiating a ssh connection to the container) and then commit those changes

Using (one of the) ansible concept seems like the ideal approach, but so far I've hit some walls making this work.

start off configuring with just docker to make sure everything works that way first. after that it should be easier to just focus on the ansible integration. 

(does not appear that docker on mac still utilizes a separate docker-machine running on VirtualBox 2017.09.27 16:15:17)


## Running a Container

When you 'run' a command with docker, you specify the docker image to use to run it. The run command will download the image, build the container (if it doesn't exist already), and then run the command in the container.

    docker run mhart/alpine-node node --version
    
### Connecting to a Container 

start and connect to a docker container:

    docker run -i -t --entrypoint /bin/bash <imageID>
    docker run -i -t --entrypoint /bin/bash docker_web_run_1

start a new shell in an already running container:

    docker exec -it <containerIdOrName> bash
    docker exec -it 393b12a61839 /bin/sh
    docker exec -it docker_web_run_1 bash

connect to a (already running) docker container (Note: this will share the same shell if another instance is already connected interactively)

    docker attach loving_heisenberg 

via:
http://askubuntu.com/questions/505506/how-to-get-bash-or-ssh-into-a-running-container-in-background-mode

### Stopping a Container

    docker container stop devtest

To stop everything:

    docker stop $(docker ps -q)


## Shares & Storage

It is possible to share storage between the host and containers. For a general overview:

https://docs.docker.com/storage/

Bind Mounts are shares data between the container and the host:

https://docs.docker.com/storage/bind-mounts/

Volumes are encapsulated in the container engine itself (managed separately from the host):

https://docs.docker.com/storage/volumes/

For development, a bind mount may work well. For deployments, a volume is a better choice. 

These can be specified when running a container, or as part of a compose setup:

```
docker run -d \
  -it \
  --name devtest \
  --mount type=bind,src="$(pwd)"/target,dst=/app \
  nginx:latest
```

## Networking

See all networks currently configured:

    docker network ls
    
See details for a specific network:

    docker network inspect bridge

Docker containers can be referenced from other containers using the container name. Be sure to use the full container name, not the abbreviated service name that is used in docker-compose files. 

`ping` is not always available. On debian based containers, install it with:

    apt-get update
    apt-get install iputils-ping

From there, can testing pinging containers by name:

    ping nginx

see a list of all IP addresses for all containers:
sudo docker ps | tail -n +2 | while read cid b; do echo -n "$cid\t"; sudo docker inspect $cid | grep IPAddress | cut -d \" -f 4; done

via:
http://stackoverflow.com/questions/17157721/getting-a-docker-containers-ip-address-from-the-host


wasn't sure about how to get one container to talk to another...
they've documented that well:

https://docs.docker.com/engine/userguide/containers/networkingcontainers/

(on macs) set up a terminal to know how to interact with docker by running:

    eval "$(docker-machine env default)"


way to generalize reference to containers in configuration files?
what if the IP for the api server changes?
would require manually updating nginx.conf file
just use docker name

### Troubleshooting connections in docker

https://www.docker.com/blog/why-you-dont-need-to-run-sshd-in-docker

A successful approach was to launch the server, connect to the container using another shell

    apk update
    
this provides the "ss" command for "socket statistics"
RUN apk add --no-cache iproute2
e.g. to see if a server is running on expected port:

    ss -lntp

verify server was on correct ports using above command

    apk add lynx

lynx 127.0.0.1:8080

curl is another good option!

### Troubleshooting nginx

If you go inside the container `docker exec -it <container-id> /bin/bash` and check the log location `ls -la /var/log/nginx/` you will see the following output:

```
lrwxrwxrwx 1 root root   11 Apr 30 23:05 access.log -> /dev/stdout
lrwxrwxrwx 1 root root   11 Apr 30 23:05 error.log -> /dev/stderr
```

Clearly, the logs are written in stdout. You can also try doing cat access.log inside the container and it still doesn't show anything.

The right way to get your logs is going outside the container and doing `docker logs <container-id>` Then, you would see your logs. 

https://stackoverflow.com/questions/30269672/unable-to-use-lt-when-running-nginx-docker-or-cat-logs



## Context Specific Applications

### Node

If you're using a container that does not have Node installed (e.g. Centos, installing from nodesource.com seems like the best option

RUN curl -sL https://rpm.nodesource.com/setup_10.x | bash # for node version 10.x
RUN yum -y install nodejs
RUN node --version # optional to check that it worked
RUN npm --version # optional to check that it worked


tricky to use NVM in a container:

```
# nvm environment variables
ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 12.16.2

# Install NVM for installing node
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash

RUN source $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

```

### NPM Packages

May be possible to minimize the number of npm packages pulled down during an image build:

https://itnext.io/npm-install-with-cache-in-docker-4bb85283fa12

Looks like Seth has another tactic for this here:

https://github.com/City-of-Bloomington/myBloomington/blob/master/Dockerfile

