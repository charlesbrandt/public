# Kubernetes

Container orchestration.

Abbreviated as 'K8s' because there are eight letters between the 'k' and the 's' in 'kubernetes'. 

https://kubernetes.io/  
Kubernetes  

https://en.wikipedia.org/wiki/Kubernetes

https://github.com/ramitsurana/awesome-kubernetes
ramitsurana/awesome-kubernetes: A curated list for awesome kubernetes sources


## General Info

https://kubernetes.io/docs/home/  
Kubernetes Documentation | Kubernetes  
https://kubernetes.io/docs/concepts/overview/what-is-kubernetes/  
What is Kubernetes? | Kubernetes  
https://kubernetes.io/docs/concepts/overview/components/  
Kubernetes Components | Kubernetes  
https://kubernetes.io/docs/concepts/overview/working-with-objects/kubernetes-objects/  
Understanding Kubernetes Objects | Kubernetes  
https://kubernetes.io/docs/concepts/workloads/pods/  
Pods | Kubernetes  

## Tools

Make sure you have the basics installed locally:

  - Docker  
  - kubectl  
  - kubeadm  


## Cluster Distributions

There are many different ways to configure kubernetes clusters. A common way is to deploy them to a managed cloud service like Google, AWS, or Azure. The exact distribution you use may vary, but the concepts should transfer from one context to another. 

These notes focus on local setups, since that's a good way to learn. 

## Stock Kubernetes

Currently following along and summarizing these resources:

https://blog.codeship.com/getting-started-with-kubernetes/

Choose where you want to run your Kubernetes cluster

https://kubernetes.io/docs/setup/
   
https://kubernetes.io/docs/tutorials/kubernetes-basics/create-cluster/cluster-intro/

### K3s

Minimalistic and lightweight distribution. Allows running / developing on a single server. Works even on a Pi! 

https://k3s.io/

https://rancher.com/docs/k3s/latest/en/architecture/

```
curl -sfL https://get.k3s.io | sh -
```

```
[INFO]  Verifying binary download
[INFO]  Installing k3s to /usr/local/bin/k3s
[INFO]  Creating /usr/local/bin/kubectl symlink to k3s
[INFO]  Creating /usr/local/bin/crictl symlink to k3s
[INFO]  Skipping /usr/local/bin/ctr symlink to k3s, command exists in PATH at /usr/bin/ctr
[INFO]  Creating killall script /usr/local/bin/k3s-killall.sh
[INFO]  Creating uninstall script /usr/local/bin/k3s-uninstall.sh
[INFO]  env: Creating environment file /etc/systemd/system/k3s.service.env
[INFO]  systemd: Creating service file /etc/systemd/system/k3s.service
[INFO]  systemd: Enabling k3s unit
Created symlink /etc/systemd/system/multi-user.target.wants/k3s.service → /etc/systemd/system/k3s.service.
[INFO]  systemd: Starting k3s
```


```
sudo k3s kubectl get node
```

### Distributions

Running your own cluster

https://k3s.io/  
💤 K3s: Lightweight Kubernetes  
https://rancher.com/docs/k3s/latest/en/  
💤 Rancher Docs: K3s - Lightweight Kubernetes  
  
https://duckduckgo.com/?t=ffab&q=running+a+developer+setup+with+kubernetes&ia=web  
💤 running a developer setup with kubernetes at DuckDuckGo  
https://developer.ibm.com/components/kubernetes/articles/setup-guide-for-kubernetes-developers/  
💤 Setup guide for Kubernetes developers: So you want to fix Kubernetes? – IBM Developer  
https://loft.sh/blog/kubernetes-development-workflow-3-critical-steps/  
💤 The Kubernetes Development Workflow – 3 Critical Steps | Loft Blog  

### Local Development

https://developer.ibm.com/technologies/containers/blogs/options-to-run-kubernetes-locally/

Microk8s and k3s also come up as options. 
Microk8s is by Canonical (ubuntu) and uses proprietary snapcraft store. 




## Access, Administration, Interfaces


### Portainer

[Portainer](portainer.md)


https://192.168.2.85:9443/#!/2/docker/stacks
Warning: Potential Security Risk Ahead

https://github.com/portainer/portainer
💤 portainer/portainer: Making Docker and Kubernetes management easy.

https://docs.portainer.io/v/ce-2.11/user/docker/containers
💤 Containers - Portainer Documentation

https://howchoo.com/devops/how-to-add-a-health-check-to-your-docker-container
💤 How to Add a Health Check to Your Docker Container - Howchoo
https://duckduckgo.com/?q=container+health+check&t=fpas&ia=web
💤 container health check at DuckDuckGo
https://github.com/topics/docker
💤 docker · GitHub Topics



### kubectl

kubectl is the default CLI for managing kubernetes

#### Install kubectl

Make sure you have kubectl installed. You can install kubectl according to the instructions in Install and Set Up kubectl.

    curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl

    chmod +x ./kubectl
    
    sudo mv ./kubectl /usr/local/bin/kubectl
    
    kubectl version


### VS Code 

Use a graphical tool to get an overview of the environment, something as simple as the Kubernetes extension for Visual Studio Code works.

https://code.visualstudio.com/docs/azure/kubernetes

### k8slens

https://k8slens.dev 
for accessing the cluster

https://k8slens.dev/  
💤 Lens | The Kubernetes IDE  
https://github.com/lensapp/lens  
💤 GitHub - lensapp/lens: Lens - The Kubernetes IDE  
  

### K9s - Command line 

K9s - Kubernetes CLI To Manage Your Clusters In Style!

https://github.com/derailed/k9s

Binary releases

https://github.com/derailed/k9s/releases




## Deploy a service

Start with something simple. Nginx serving static files seems like a good place to begin. 

## Helm

Configuration Management for the services running on your cluster. Similar to docker-compose.yml files in the Docker ecosystem.

From their site: 

```
Helm helps you manage Kubernetes applications — Helm Charts help you define, install, and upgrade even the most complex Kubernetes application.

Charts are easy to create, version, share, and publish — so start using Helm and stop the copy-and-paste.
```

https://helm.sh/

https://github.com/helm/helm

Download an appropriate release:

https://github.com/helm/helm/releases/tag/v3.0.3

Unpack it.

    cd ~/Downloads
    tar zxvf helm-v3.0.3-linux-amd64.tar.gz 
    cd [directory]
    
    helm create [chart-name]





## Auth

https://kubernetes.io/  
Kubernetes  
https://duckduckgo.com/?t=ffab&q=kubernetes+authentication&ia=web  
kubernetes authentication at DuckDuckGo  
https://kubernetes.io/docs/reference/access-authn-authz/authorization/  
Authorization Overview | Kubernetes  
https://kubernetes.io/docs/reference/access-authn-authz/authentication/  
Authenticating | Kubernetes  
https://kubernetes.io/docs/concepts/security/controlling-access/  
Controlling Access to the Kubernetes API | Kubernetes  


https://duckduckgo.com/?t=ffab&q=how+to+manage+private+keys+store&ia=web
how to manage private keys store at DuckDuckGo
https://duckduckgo.com/?t=ffab&q=kubernetes+key+storage&ia=web
kubernetes key storage at DuckDuckGo
https://duckduckgo.com/?t=ffab&q=kubernetes+identity&ia=web
kubernetes identity at DuckDuckGo
https://kubernetes.io/docs/reference/access-authn-authz/authentication/
Authenticating | Kubernetes
https://duckduckgo.com/?q=keystone+identity+manager&t=ffcm&ia=web
keystone identity manager at DuckDuckGo
https://keystonev1.docs.apiary.io/#
Keystone Identity Manager SSO API v1 · Apiary
https://docs.openstack.org/keystone/latest/
Keystone, the OpenStack Identity Service — keystone 20.0.1.dev10 documentation
https://docs.openstack.org/api-ref/identity/index.html
💤 Welcome to keystone’s documentation! — keystone documentation



## Similar Tools

Kubernetes is compared to Docker Swarm.

Docker-compose is it's own thing (what to run on a local instance)

https://duckduckgo.com/?t=ffab&q=docker-compose+vs+kubernetes&ia=web  
💤 docker-compose vs kubernetes at DuckDuckGo  
https://stackoverflow.com/questions/47536536/whats-the-difference-between-docker-compose-and-kubernetes  
What's the difference between Docker Compose and Kubernetes? - Stack Overflow  

## Community

The Cloud Native group manages general community under the broader umbrella rather than one specific project / tool. 

https://radar.cncf.io/
Home | CNCF Radars

https://events.linuxfoundation.org/kubecon-cloudnativecon-north-america/

https://landscape.cncf.io/
CNCF Cloud Native Interactive Landscape

