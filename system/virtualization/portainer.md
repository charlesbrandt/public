# Portainer


https://docs.portainer.io/v/ce-2.9/start/install/server/docker/linux#deployment
Install Portainer with Docker on Linux - Portainer Documentation  

```
docker volume create portainer_data
```

Then

```
docker run -d -p 8000:8000 -p 9443:9443 --name portainer \
  --restart=always \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v portainer_data:/data \
  cr.portainer.io/portainer/portainer-ce:2.9.3
```

To make sure it's running:

```
docker ps 
```

Then open the service in a browser:

https://localhost:9443

Create a new account as needed / or import settings from else where. 


## Docker Compose

For management under docker-compose:

```
  portainer:
    image: cr.portainer.io/portainer/portainer-ce:2.9.3
    container_name: portainer
    restart: unless-stopped
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - portainer_data:/data
    ports:
      - 8000:8000
      - 9443:9443 
```


## See Also

https://docs.portainer.io/v/ce-2.11/user/kubernetes/cluster
💤 Cluster - Portainer Documentation
https://docs.portainer.io/v/ce-2.11/user/kubernetes
💤 Kubernetes - Portainer Documentation
https://docs.portainer.io/v/ce-2.11/user/docker/secrets
💤 Secrets - Portainer Documentation
https://docs.portainer.io/v/ce-2.11/user/docker/networks
💤 Networks - Portainer Documentation
https://docs.portainer.io/v/ce-2.11/user/docker
💤 Docker/Swarm - Portainer Documentation
https://docs.portainer.io/v/ce-2.11/user/docker/templates
💤 App Templates - Portainer Documentation
https://www.portainer.io/about
💤 About Portainer Open Source Container GUI | Portainer
https://www.redhat.com/en/topics/devops/what-is-sre
💤 What is SRE?
https://duckduckgo.com/?q=SRE.&t=fpas&ia=stock
💤 SRE. at DuckDuckGo


https://duckduckgo.com/?t=ffab&q=docker+gui&ia=web  
💤 docker gui at DuckDuckGo  
https://www.how2shout.com/tools/4-best-docker-gui-tools-to-manage-containers-graphically.html  
💤 4 Best Docker GUI Application platfroms to manage containers - H2S Media  
https://duckduckgo.com/?t=ffcm&q=Portainer&ia=web  
💤 Portainer at DuckDuckGo  
https://docs.portainer.io/v/ce-2.9/start/install  
💤 Install Portainer CE - Portainer Documentation  

https://docs.portainer.io/v/ce-2.9/start/requirements-and-prerequisites  
Requirements and prerequisites - Portainer Documentation  
https://duckduckgo.com/?t=ffab&q=portainer+docker+compose&ia=web  
portainer docker compose at DuckDuckGo  
https://www.portainer.io/blog/stacks-docker-compose-the-portainer-way  
Stacks = docker-compose, the Portainer way  



## See also

https://www.qnap.com/en/how-to/tutorial/article/how-to-use-container-station  
💤 How to Use Container Station | QNAP  
