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


## Minikube

https://minikube.sigs.k8s.io/docs/start/

```
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube
```

then 

```
minikube start
```

look for

> Done! kubectl is now configured to use "minikube" cluster and "default" namespace by default



https://minikube.sigs.k8s.io/docs/start/

If you already have a `kubectl` client available locally, see what's running:

```
kubectl get po -A
```

Or, have `minikube` get `kubectl` for you:


```shell
minikube kubectl -- get po -A
```

You can also make your life easier by adding the following to your shell config:

```shell
alias kubectl="minikube kubectl --"
```


Web interface for cluster

```
minikube dashboard
```

### Workflow (kubectl usage)

Start with a deployment

```
kubectl create deployment hello-minikube --image=docker.io/nginx:1.23
```

This will create some pods which you can verify with kubectl

To be able to access the pods, create a service:

```
kubectl expose deployment hello-minikube --type=NodePort --port=80
```

Now the cluster knows how to access it. To access the service running on the cluster:


The easiest way to access this service is to let minikube launch a web browser for you:

```
minikube service hello-minikube
```

Alternatively, use kubectl to forward the port:

```
kubectl port-forward service/hello-minikube 7080:80
```

See all services

kubectl get services hello-minikube

You can try deleting the pod(s) powering the service. They should be restarted automatically. To stop the whole thing, remove the deployment:

```
kubectl delete deployment hello-minikube
```

https://kubernetes.io/docs/reference/kubectl/cheatsheet/


## Skaffold

```
cd ~/Downloads
curl -Lo skaffold https://storage.googleapis.com/skaffold/releases/latest/skaffold-linux-amd64 && \
sudo install skaffold /usr/local/bin/
```

I had an easier time getting running with Minikube, but K3s should work equally well. 

The examples are a great way to get started.

- Clone the Skaffold repository:
    
    ```bash
    git clone --depth 1 https://github.com/GoogleContainerTools/skaffold
    ```
    
- Change to the `examples/getting-started` in skaffold directory.
    
    ```bash
    cd skaffold/examples/getting-started
    ```

https://skaffold.dev/docs/quickstart/  
Quickstart | Skaffold  

Try running them and testing them out (locally).

https://skaffold.dev/docs/design/config/  
Skaffold Pipeline | Skaffold  
https://skaffold.dev/docs/pipeline-stages/deployers/  
Deploy | Skaffold  
https://skaffold.dev/  
Skaffold  

https://skaffold.dev/docs/pipeline-stages/  
Skaffold Pipeline Stages | Skaffold  
https://skaffold.dev/docs/pipeline-stages/filesync/  
File Sync | Skaffold  
https://skaffold.dev/docs/design/  
Architecture and Design | Skaffold  
https://skaffold.dev/docs/tutorials/  
Tutorials | Skaffold  





## Tools

Make sure you have the basics installed locally:

  - Docker  
  - kubectl  
  - kubeadm  

```
cd ~/tools
```

TODO: consider best path for binaries for a user


### kubectl

`kubectl` is the main CLI for managing kubernetes

https://kubernetes.io/docs/reference/kubectl/cheatsheet/


Make sure you have `kubectl` installed. You can install `kubectl` according to the instructions in Install and Set Up kubectl.

```
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x kubectl
```

Validate

```
curl -LO "https://dl.k8s.io/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl.sha256"
echo "$(<kubectl.sha256)  kubectl" | sha256sum --check
```

https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/

#### global


```
curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl

chmod +x ./kubectl
    
sudo mv ./kubectl /usr/local/bin/kubectl
    
kubectl version
```

#### local

Any reason to install as root?
local bin seems sufficient
Some notes about paths

```
vi ~/.bashrc

PATH="${PATH}:~/tools/"  

source ~/.bashrc
```




### kompose

When migrating from docker-compose, Kompose is a useful utility for converting from a `docker-compose.yml` to a manifest

https://github.com/kubernetes/kompose

https://github.com/kubernetes/kompose/blob/master/docs/installation.md#ubuntudebian

```
wget https://github.com/kubernetes/kompose/releases/download/v1.26.1/kompose_1.26.1_amd64.deb # Replace 1.26.1 with latest tag
sudo apt install ./kompose_1.26.1_amd64.deb
```

Skaffold has a convenience wrapper for calling kompose

```
skaffold init --compose-file docker-compose.yml
```




### kubeadm

Many good system checks to review here:

https://kubernetes.io/docs/setup/production-environment/tools/kubeadm/install-kubeadm/

Saw many mentions about the importance of disabling swap. 
Not sure what happens, but I'll heed the warning. 

https://serverfault.com/questions/684771/best-way-to-disable-swap-in-linux

```
cat /proc/swaps
sudo swapoff -a
sudo vi /etc/fstab

```

Warning: These instructions exclude all Kubernetes packages from any system upgrades. This is because kubeadm and Kubernetes require special attention to upgrade. 

https://kubernetes.io/docs/tasks/administer-cluster/kubeadm/kubeadm-upgrade/

These will install `kubectl` too! :)

```
sudo apt-get update
sudo apt-get install -y apt-transport-https ca-certificates curl

sudo curl -fsSLo /usr/share/keyrings/kubernetes-archive-keyring.gpg https://packages.cloud.google.com/apt/doc/apt-key.gpg

echo "deb [signed-by=/usr/share/keyrings/kubernetes-archive-keyring.gpg] https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee /etc/apt/sources.list.d/kubernetes.list

sudo apt-get update
sudo apt-get install -y kubelet kubeadm kubectl
sudo apt-mark hold kubelet kubeadm kubectl
```

## Manifests

Configuration Management for the services running on your cluster. Similar to docker-compose.yml files in the Docker ecosystem.

https://prefetch.net/blog/2019/10/16/the-beginners-guide-to-creating-kubernetes-manifests/

Time to describe the service and its architecture. 

Describe your service's architecture. 

Start with something simple. Nginx serving static files seems like a good place to begin. 

```
# nginx-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-deployment
  labels:
    app: nginx
spec:
  replicas: 3
  selector:
    matchLabels:
      app: nginx
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
      - name: nginx
        image: nginx:1.14.2
        ports:
        - containerPort: 80
```

Apply the manifest to the cluster

```
kubectl apply -f nginx-deployment.yaml
```

See the effects of the deployment

```
kubectl get deployments

kubectl get pods
```

via: https://www.endpointdev.com/blog/2022/01/kubernetes-101/




Then, to see the service, apply a 'service' to the cluster:

```
# nginx-service.yaml
apiVersion: v1
kind: Service
metadata:
  name: nginx-service
spec:
  type: NodePort
  selector:
    app: nginx
  ports:
    - name: "http"
      port: 80
      targetPort: 80
      nodePort: 30080
```

If you haven't yet, explore the way skaffold examples are configured. Those are good to leverage. 


## Ingress

An Ingress Controller handles how traffic outside of a pod is routed within the pod. Nginx is a common option. Kong is built on Nginx and is another good option.

### traefik

### Kong

https://duckduckgo.com/?t=ffab&q=kong+rbac&ia=web  
kong rbac at DuckDuckGo  
https://github.com/mr5/kong-rbac  
mr5/kong-rbac: RBAC plugin for KongCE, it's still in development.DO NOT use it in production.  
https://keyvatech.com/2020/01/15/setting-up-role-based-access-control-rbac-with-kong-enteprise/  
Setting up Role-Based Access Control (RBAC) with Kong Enteprise - Keyva  
https://duckduckgo.com/?t=ffab&q=secure+kong+community+edition&ia=web  
secure kong community edition at DuckDuckGo  
https://medium.com/devopsturkiye/kong-api-gateway-installing-configuring-and-securing-dfea423ee53c  
Kong Api Gateway: Installing, Configuring and Securing. | by Cagri Ersen | Devops Türkiye| Medium  
https://duckduckgo.com/?t=ffab&q=kong+authentication+plugins&ia=web  
kong authentication plugins at DuckDuckGo  
https://docs.konghq.com/hub/kong-inc/oauth2/  
OAuth 2.0 Authentication plugin | Kong Docs  
https://duckduckgo.com/?t=ffab&q=kong+vs+nginx&ia=web  
kong vs nginx at DuckDuckGo  
https://discuss.konghq.com/t/kong-vs-nginx-kunernetes-ingress-controller/1985  
Kong VS nginx kunernetes ingress controller - Questions - Kong Nation  


## Helm

Templating system for manifests.

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

```
cd ~/Downloads
tar zxvf helm-v3.0.3-linux-amd64.tar.gz 
cd [directory]
    
helm create [chart-name]
```

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
 Welcome to keystone’s documentation! — keystone documentation





## Cluster Distributions

There are many different ways to configure kubernetes clusters. A common way is to deploy them to a managed cloud service like Google, AWS, or Azure. The exact distribution you use may vary, but the concepts should transfer from one context to another. 

These notes focus on local setups (running your own cluster), since that's a good way to learn. 

Especially useful for development purposes

https://duckduckgo.com/?t=ffab&q=running+a+developer+setup+with+kubernetes&ia=web  
 running a developer setup with kubernetes at DuckDuckGo  
https://developer.ibm.com/components/kubernetes/articles/setup-guide-for-kubernetes-developers/  
 Setup guide for Kubernetes developers: So you want to fix Kubernetes? – IBM Developer  
https://loft.sh/blog/kubernetes-development-workflow-3-critical-steps/  
 The Kubernetes Development Workflow – 3 Critical Steps | Loft Blog  

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


To install on worker nodes and add them to the cluster, run the installation script with the `K3S_URL` and `K3S_TOKEN` environment variables. Here is an example showing how to join a worker node:

```bash
curl -sfL https://get.k3s.io | K3S_URL=https://myserver:6443 K3S_TOKEN=mynodetoken sh -
```

Setting the `K3S_URL` parameter causes K3s to run in worker mode. The K3s agent will register with the K3s server listening at the supplied URL. The value to use for `K3S_TOKEN` is stored at `/var/lib/rancher/k3s/server/node-token` on your server node.


via [Rancher Docs: Quick-Start Guide](https://rancher.com/docs/k3s/latest/en/quick-start/)



```
sudo k3s kubectl get node
```


https://k3s.io/  
 K3s: Lightweight Kubernetes  
https://rancher.com/docs/k3s/latest/en/  
 Rancher Docs: K3s - Lightweight Kubernetes  

To uninstall

```
/usr/local/bin/k3s-uninstall.sh
```


### minikube

see above

### kind

Primarily for development and testing. Don't expect to deploy a local cluster using this approach?


```
curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.11.1/kind-linux-amd64
chmod +x ./kind
mv ./kind /some-dir-in-your-PATH/kind
```

https://kind.sigs.k8s.io/docs/user/quick-start/

```
kind create cluster
Creating cluster "kind" ...
 ✓ Ensuring node image (kindest/node:v1.21.1) 🖼 
 ✓ Preparing nodes 📦  
 ✓ Writing configuration 📜 
 ✓ Starting control-plane 🕹️ 
 ✓ Installing CNI 🔌 
 ✓ Installing StorageClass 💾 
Set kubectl context to "kind-kind"
You can now use your cluster with:

kubectl cluster-info --context kind-kind

Have a nice day! 👋
```

By default, the cluster access configuration is stored in ${HOME}/.kube/config if $KUBECONFIG environment variable is not set.


```
Security Goose Says:
NOTE: You should really think thrice before exposing your kind cluster publicly! kind does not ship with state of the art security or any update strategy (other than disposing your cluster and creating a new one)! We strongly discourage exposing kind to anything other than loopback.
```

So not a great fit for local network service deployment then? 
Ideally use the same tools everywhere. 



https://kind.sigs.k8s.io/docs/user/quick-start/  
kind – Quick Start  
https://kind.sigs.k8s.io/docs/user/ingress  
kind – Ingress  
https://kind.sigs.k8s.io/docs/user/configuration/  
kind – Configuration  
https://kind.sigs.k8s.io/docs/design/principles/  
kind – Principles  
https://raw.githubusercontent.com/kubernetes-sigs/kind/main/site/content/docs/user/kind-example-config.yaml  
raw.githubusercontent.com/kubernetes-sigs/kind/main/site/content/docs/user/kind-example-config.yaml  


### Stock Kubernetes

Currently following along and summarizing these resources:

https://blog.codeship.com/getting-started-with-kubernetes/

Choose where you want to run your Kubernetes cluster

https://kubernetes.io/docs/setup/
   
https://kubernetes.io/docs/tutorials/kubernetes-basics/create-cluster/cluster-intro/

### Microk8s

https://developer.ibm.com/technologies/containers/blogs/options-to-run-kubernetes-locally/

Microk8s and k3s also come up as options. 
Microk8s is by Canonical (ubuntu) and uses proprietary snapcraft store. 



## Access, Administration, Interfaces


### Portainer

[Portainer](portainer.md)


https://192.168.2.85:9443/#!/2/docker/stacks
Warning: Potential Security Risk Ahead

https://github.com/portainer/portainer
 portainer/portainer: Making Docker and Kubernetes management easy.

https://docs.portainer.io/v/ce-2.11/user/docker/containers
 Containers - Portainer Documentation

https://howchoo.com/devops/how-to-add-a-health-check-to-your-docker-container
 How to Add a Health Check to Your Docker Container - Howchoo
https://duckduckgo.com/?q=container+health+check&t=fpas&ia=web
 container health check at DuckDuckGo
https://github.com/topics/docker
 docker · GitHub Topics




### VS Code 

Use a graphical tool to get an overview of the environment, something as simple as the Kubernetes extension for Visual Studio Code works.

https://code.visualstudio.com/docs/azure/kubernetes

### k8slens

https://k8slens.dev 
for accessing the cluster

https://k8slens.dev/  
 Lens | The Kubernetes IDE  
https://github.com/lensapp/lens  
 GitHub - lensapp/lens: Lens - The Kubernetes IDE  
  

### K9s - Command line 

K9s - Kubernetes CLI To Manage Your Clusters In Style!

https://github.com/derailed/k9s

Binary releases

https://github.com/derailed/k9s/releases




## Similar Tools

Kubernetes is compared to Docker Swarm.

Docker-compose is it's own thing (what to run on a local instance)

https://duckduckgo.com/?t=ffab&q=docker-compose+vs+kubernetes&ia=web  
docker-compose vs kubernetes at DuckDuckGo  
https://stackoverflow.com/questions/47536536/whats-the-difference-between-docker-compose-and-kubernetes  
What's the difference between Docker Compose and Kubernetes? - Stack Overflow  

## Community

The Cloud Native group manages general community under the broader umbrella rather than one specific project / tool. 

https://radar.cncf.io/
Home | CNCF Radars

https://events.linuxfoundation.org/kubecon-cloudnativecon-north-america/

https://landscape.cncf.io/
CNCF Cloud Native Interactive Landscape

