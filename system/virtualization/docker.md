# Docker

Docker implements a container solution. Containers are a lighter weight alternative to a full virtual machine. They are run on the host operating system, but they are encapsulated to provide isolation, security, and compartmentalization. 

Docker documentation:  
https://docs.docker.com/

Overview:  
https://docs.docker.com/engine/understanding-docker/

https://docs.docker.com/engine/userguide/

Cheat sheet with an overview (similar intent as this doc)  
https://github.com/wsargent/docker-cheat-sheet

Introduction to what containers are:  
https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-18-04

## Tools 

TODO:
https://github.com/jesseduffield/lazydocker

## Aliases

Create bash aliases

Add the following to your `.bashrc` file (or equivalent)

```
alias dcu='docker compose up -d'
alias dcd='docker compose down --remove-orphans'
alias dcp='docker compose ps'
alias dce='docker compose exec'
alias dcl='docker compose logs'
```


## Installation

When it comes to installing Docker on Ubuntu, you have two main options: using the packages distributed via the official Ubuntu repositories or using the packages provided directly by Docker.


### Packages distributed via Ubuntu

Install Docker:

```bash
sudo apt update
sudo apt install docker.io
```
    
Add Your User to the Docker Group (Optional):
While this step is not strictly necessary for rootless mode, it allows you to run Docker commands without using `sudo`. You can add your user to the `docker` group with the following command:
    
```bash
sudo usermod -aG docker $USER
```
    
Log out and log back in for this change to take effect.

This version does not appear to support the newer `docker compose` syntax of the `docker-ce` version. Install it separately:

newer `docker compose` functionality [as discussed here](https://stackoverflow.com/a/79555851)

```
sudo apt install docker-compose-v2
```

older `docker-compose` functionality

```
sudo apt install docker-compose 
```

Be sure to set `DOCKER_HOST` in `.bashrc`:

```
# this setting works with docker.io
export DOCKER_HOST=unix:///var/run/docker.sock
```


### Packages provided directly by Docker

Just follow along with Docker's instructions. These stay more up to date

https://docs.docker.com/engine/install/ubuntu/

https://docs.docker.com/engine/install/


### Verify

At this point Docker should be installed and you can verify with:

```
sudo systemctl status docker
```

## GPU Support

Using GPU via docker 

https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/install-guide.html

```
curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
  && curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | \
    sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
    sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list
```

```
sudo apt-get update
```

```
sudo apt-get install -y nvidia-container-toolkit
```

Alternative, concise version of the first command (untested)

```
distribution=$(. /etc/os-release;echo  $ID$VERSION_ID)  
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -  
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list

```

https://saturncloud.io/blog/how-to-install-pytorch-on-the-gpu-with-docker/


### Verify


```
services:
  inference:
    build: .
    container_name: inference
    volumes:
      - ~/.cache/huggingface:/root/.cache/huggingface
      - .:/app
    command: sh -c "while true; do sleep 1; done"
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: 1
              capabilities: [gpu]
    stop_signal: SIGINT
```


```
sudo docker-compose up -d
sudo docker-compose exec inference bash
nvidia-smi
```



Or an ollama example:

```
services:
  ollama:
    volumes:
      - ollama:/root/.ollama
    container_name: ollama
    pull_policy: always
    tty: true
    restart: unless-stopped
    image: ollama/ollama:${OLLAMA_DOCKER_TAG-latest}
    ports:
      - ${OPEN_WEBUI_PORT-11434}:11434
    # GPU support
    deploy:
      resources:
        reservations:
          devices:
            - driver: ${OLLAMA_GPU_DRIVER-nvidia}
              count: ${OLLAMA_GPU_COUNT-1}
              capabilities:
                - gpu

```



### Rootless & Permissions

Visit https://docs.docker.com/go/rootless/ to learn about rootless mode.

Seems necessary to still install docker as usual. 

```
sudo apt-get install -y uidmap
```

With Ubuntu, I already had:

```
sudo apt-get install -y dbus-user-session
```

If the system-wide Docker daemon is already running, consider disabling it: 

```
sudo systemctl disable --now docker.service docker.socket
```

To run Docker as a non-privileged user, consider setting up the Docker daemon in rootless mode for your user. `dockerd-rootless-setuptool.sh` should be in `/usr/bin` if Docker was installed with packages.

```
dockerd-rootless-setuptool.sh install
```

Automatically start up when user logs in:

```
systemctl --user enable docker
sudo loginctl enable-linger $(whoami)
```

Make sure the following environment variables are set by adding them to `~/.bashrc`

```
export PATH=/usr/bin:$PATH
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
```


Alternate notes

**Install the Rootless Kit:** To set up Docker to run in rootless mode, you need to install the `docker-rootless-extras` package. This can be done with the following command:
    
```bash
sudo apt install docker-rootless-extras
```

yields
```
sudo apt install docker-rootless-extras
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
E: Unable to locate package docker-rootless-extras
```
    
**Start the Rootless Docker Daemon:** Once the `docker-rootless-extras` package is installed, you can start the rootless Docker daemon using the following command:
    
```bash
dockerd-rootless-setuptool.sh install
```
    
This script will set up the necessary environment and start the Docker daemon in rootless mode.
    
**Configure Systemd (Optional):** If you want the rootless Docker daemon to start automatically on system boot, you can enable the systemd service:
    
```bash
systemctl --user enable docker
systemctl --user start docker
```











## System configurations

To expose privileged ports (< 1024), 

```
sudo micro /etc/sysctl.conf 
```

Add the line: 
```
net.ipv4.ip_unprivileged_port_start=80
```

Then apply changes to the system
```
sudo sysctl -p
```

Alternatively, set CAP_NET_BIND_SERVICE on rootlesskit binary and restart the daemon.

```
sudo setcap cap_net_bind_service=ep $(which rootlesskit)
```





### Add user to docker group

This allows you to execute docker without using `sudo` for every command.  
`docker-compose` is a frequent command. 

```
sudo groupadd docker

sudo usermod -aG docker ${USER}
```

Log out and log back in.

This usually doesn't work: 

```
su - ${USER}
```
    
Test that you have permissions to run docker commands without sudo:

```
docker ps
```

If docker isn't running, start it up: 

```
systemctl --user restart docker
```



To run the Docker daemon as a fully privileged service, but granting non-root
users access, refer to https://docs.docker.com/go/daemon-access/

WARNING: Access to the remote API on a privileged Docker daemon is equivalent
         to root access on the host. Refer to the 'Docker daemon attack surface'
         documentation for details: https://docs.docker.com/go/attack-surface/




## Docker Desktop for Linux

Looking forward to giving this a try. KVM/QEMU -- sounds promising!

```
sudo usermod -aG kvm $USER
```

Dependencies:

Install docker as usual -- ends up expecting `docker-ce-cli` package anyway

Install KVM/QEMU ahead of time

~/public/system/virtualization/kvm.md

```
sudo apt-get install pass tree
```

download package for system

https://docs.docker.com/desktop/linux/install/

```
sudo dpkg -i docker-desktop-4.8.0-amd64.deb 
```



## Status

```
systemctl status docker
```

To see a list of *currently running* docker containers:

```
docker ps
```

To see the name of the container (and the size of disk in use):

```
docker ps -s 
```

https://docs.docker.com/engine/reference/commandline/ps/



















## Dockerfile

How you set up the image that gets run in a container

https://docs.docker.com/engine/reference/builder/

A Dockerfile determines how your image is configured (and ultimately what is run in your container). These can be tracked as part of the project's source code. 

https://docs.docker.com/get-started/part2/

to keep a container running, choose a process that won't exit:

```
CMD [ "tail", "-f", "/dev/null" ]
```

See also [dockerfiles](dockerfiles.md)

```
FROM node:lts

# Set the working directory.
WORKDIR /srv
```



## Images

See a list of available docker images (see what is currently available):

```
docker image ls -a
```

Is equivalent to:

```
docker images
```

docker images are stored in:

```
/var/lib/docker
```

(don't forget, docker sometimes run in a VM, so this location is on that VM)
via:
http://stackoverflow.com/questions/19234831/where-are-docker-images-stored-on-the-host-machine

### Official Images

https://hub.docker.com/search?q=&type=image&image_filter=official

Different dependencies will result in different sized containers. Smaller is generally better, everything else being the same:

https://www.brianchristner.io/docker-image-base-os-size-comparison/

https://docs.docker.com/docker-hub/official_images/

### Building

```
docker build -t simple-node .
docker run -p 3000:3000 simple-node
```

Now you should be able to connect to localhost without specifying a VM host. Without the explicit forward for the port, the port won't be available:

localhost:3000
    
To specify a Dockerfile, use -f:

```
docker build -t simple-node -f Dockerfile.debug .
```
    
https://docs.docker.com/engine/reference/commandline/build/




## Running a Container

When you 'run' a command with docker, you specify the docker image to use to run it. The run command will download the image, build the container (if it doesn't exist already), and then run the command in the container.

```
docker run mhart/alpine-node node --version
```
    
Even with single container setups, it may make sense to use docker-compose to specify what the container is named and any volumes that should be mounted. That also makes it easier to integrate with other docker-compose setups. 
    
### Connecting to a Container 

start and connect to a docker container:

```
docker run -i -t --entrypoint /bin/bash <imageID>
docker run -i -t --entrypoint /bin/bash docker_web_run_1
```

start a new shell in an already running container:

```
docker exec -it <containerIdOrName> bash
docker exec -it 393b12a61839 /bin/sh
docker exec -it docker_web_run_1 bash
```

connect to a (already running) docker container (Note: this will share the same shell if another instance is already connected interactively)

```
docker attach loving_heisenberg 
```

via:
http://askubuntu.com/questions/505506/how-to-get-bash-or-ssh-into-a-running-container-in-background-mode

### Stopping a Container

```
docker container stop devtest
```

To stop everything:

```
docker stop $(docker ps -q)
```
    
or 

```
docker container stop $(docker container list -q)
```

### Removing a Container

```
docker rm [container]
```

### Cleaning up old images

```
docker system prune 
```

Sometimes this still doesn't notice changes to build layers. In that case, try:

```
docker builder prune
```

This seems like a well maintained answer with up-to-date options & descriptions:

https://stackoverflow.com/questions/32723111/how-to-remove-old-and-unused-docker-images

[via](https://duckduckgo.com/?t=ffab&q=docker-compose+remove+old+images&ia=web)

Some of the following options may be more aggressive in what they delete. Be careful if you have important data stored!

Sometimes when testing builds, docker will complain about running out of space:

```
Thin Pool has 2738 free data blocks which is less than minimum required 2915 free data blocks. Create more free space in thin pool or use dm.min_free_space option to change behavior
```

You can get rid of all images with:

```
docker image prune -a --force 
```

https://stackoverflow.com/questions/41531962/docker-run-error-thin-pool-has-free-data-blocks-which-is-less-than-minimum-req

Clear everything out (!!! dangerous !!!)

```
docker image rm $(docker image ls -a -q)
docker image rm -f $(docker image ls -a -q)
```

See also:
https://docs.docker.com/engine/reference/commandline/image_prune/

Still not enough space?
There are some nuclear options outlined here -- they will clear everything out, including volumes that may have data on them!

https://stackoverflow.com/questions/36918387/space-issue-on-docker-devmapper-and-centos7

### Restarting

Containers can be set to restart automatically. 

In `docker-compose.yml`, configure this with:

```
   restart: unless-stopped
```

I prefer `unless-stopped` over `always` so you don't end up with lingering services that were launched while testing a service that didn't work out. 

As long as the parent docker process is configured to run at start up (usually is by default), then those containers will restart automatically. 

You could also do a systemctrl setup like:

```
sudo systemctl enable docker-MYPROJECT-oracle_db.service
```

As described in

https://stackoverflow.com/questions/30449313/how-do-i-make-a-docker-container-start-automatically-on-system-boot  
How do I make a Docker container start automatically on system boot? - Stack Overflow  

https://docs.docker.com/config/containers/start-containers-automatically/  
Start containers automatically | Docker Documentation  

https://duckduckgo.com/?q=docker+start+container+at+boot&t=ffab&ia=web  
docker start container at boot at DuckDuckGo  



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

### Copy Data

You can copy data into and out of a container manually:

// don't use `*` with `docker cp`
docker cp e9f10889f0da:/synthea/output/fhir ui/public/data/

https://docs.docker.com/engine/reference/commandline/cp/  
docker cp | Docker Documentation  


## Networking

See all networks currently configured:

```
docker network ls
```

See details for a specific network:

```
docker network inspect bridge
```

Docker containers can be referenced from other containers using the container name. Be sure to use the full container name, not the abbreviated service name that is used in `docker-compose` files. 

`ping` is not always available. On debian based containers, install it with:

```
apt-get update
apt-get install iputils-ping
```

From there, can testing pinging containers by name:

```
ping nginx
```

See a list of all IP addresses for all containers:

```
sudo docker ps | tail -n +2 | while read cid b; do echo -n "$cid\t"; sudo docker inspect $cid | grep IPAddress | cut -d \" -f 4; done
```

via:
http://stackoverflow.com/questions/17157721/getting-a-docker-containers-ip-address-from-the-host


wasn't sure about how to get one container to talk to another...  
there's good documentation on the topic:

https://docs.docker.com/engine/userguide/containers/networkingcontainers/

How to generalize reference to containers in configuration files?  
Just use the docker container name. 
The IP for containers will change. Unless it is statically set, using an IP directly would require manually updating configuration (e.g. nginx.conf) files.  

### DNS

I ran into an issue where a container was not able to resolve DNS lookups. (to confirm this, connect to the container via bash and run `ping google.com`)

This turned out to be an issue with the way lookups are configured on my host machine (20.04). 

Docker uses the host's name resolution. Running this on the host fixes the resolution within containers: 

```
sudo ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf
```

via:  
https://serverfault.com/questions/642981/docker-containers-cant-resolve-dns-on-ubuntu-14-04-desktop-host  

Note:
If you're using hosts defined in `/etc/hosts`, the updated symlink won't resolve those correctly. 


### Troubleshooting connections in docker

https://www.docker.com/blog/why-you-dont-need-to-run-sshd-in-docker

A useful tactic is to launch the services and connect to the container directly using another shell. From there, you can install other tools for troubleshooting. The exact technique will vary based on what base distribution is being used in the container. 

```
apk update
```
    
To install a testing tool permanently in the container, use a `Dockerfile` to bake tools in. For example, this provides the `ss` command for "socket statistics"

```
RUN apk add --no-cache iproute2
```

e.g. to see if a server is running on expected port:

```
ss -lntp
```

verify server was on correct ports using above command

`curl` is a good option!

lynx could also be installed

```
apk add lynx

lynx 127.0.0.1:8080
```

To see what ports are open, install `netstat`. This approach will not persist across restarts. 


```
apt update
apt install net-tools

netstat -pan | egrep " LISTEN "
```    


## Users

https://medium.com/@mccode/processes-in-containers-should-not-run-as-root-2feae3f0df3b

```
RUN groupadd --gid 9876 projectgroup
RUN useradd -ms /bin/bash --uid 1234567 --gid 9876 projectdev
USER projectdev
```


## Context Specific Applications

### Node

See [Node Notes](../../code/javascript/node.md)

### Python

Example Dockerfile

```
FROM python:3
COPY requirements.txt /srv/app/requirements.txt
WORKDIR /srv/app
RUN pip install -r requirements.txt
```

https://hub.docker.com/_/python/

https://stackoverflow.com/questions/34398632/docker-how-to-run-pip-requirements-txt-only-if-there-was-a-change


## See Also

[Kubernetes](kubernetes.md)

[Docker Compose](docker-compose.md)

[Orchestration](orchestration.md)

https://gitlab.com/fern-seed/web-ui-api-db/

https://gitlab.com/fern-seed/web-ui-api-db/-/blob/main/README-docker.md

https://github.com/veggiemonk/awesome-docker  
GitHub - veggiemonk/awesome-docker: A curated list of Docker resources and projects  


