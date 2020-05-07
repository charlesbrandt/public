# Kubernetes

Abbreviated as 'K8s'.

https://en.wikipedia.org/wiki/Kubernetes

Container orchestration.

Currently following along and summarizing these resources:

https://blog.codeship.com/getting-started-with-kubernetes/

Choose where you want to run your Kubernetes cluster

https://kubernetes.io/docs/setup/
   
https://kubernetes.io/docs/tutorials/kubernetes-basics/create-cluster/cluster-intro/

## Local Development

https://developer.ibm.com/technologies/containers/blogs/options-to-run-kubernetes-locally/

Microk8s and k3s also come up as options. 
Microk8s is by Canonical (ubuntu) and uses proprietary snapcraft store. 

## Kind

https://github.com/kubernetes-sigs/kind/

Docker in Docker now references Kind:
https://github.com/kubernetes-retired/kubeadm-dind-cluster

Seems to be the way to go. Most compliant with what you would find in a production environment. 

https://kind.sigs.k8s.io/

Requires go language:
https://golang.org/doc/install

Then `GO111MODULE="on" go get sigs.k8s.io/kind@v0.7.0 && kind create cluster`

for me, kind was downloaded to home

~/go/bin/kind

sudo ~/go/bin/kind create cluster
[sudo] password for charles: 
Creating cluster "kind" ...
 ✓ Ensuring node image (kindest/node:v1.17.0) 
 ✓ Preparing nodes 
 ✓ Writing configuration 
 ✓ Starting control-plane 
 ✓ Installing CNI
 ✓ Installing StorageClass
Set kubectl context to "kind-kind"
You can now use your cluster with:

kubectl cluster-info --context kind-kind

Thanks for using kind!


## K3s

https://github.com/rancher/k3s

## Minikube

"Minikube is a lightweight Kubernetes implementation that creates a VM on your local machine and deploys a simple cluster containing only one node".

https://kubernetes.io/docs/tasks/tools/install-minikube/

Check if virtualization is supported on Linux

    grep -E --color 'vmx|svm' /proc/cpuinfo
    
### Install kubectl

Make sure you have kubectl installed. You can install kubectl according to the instructions in Install and Set Up kubectl.

    curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl

    chmod +x ./kubectl
    
    sudo mv ./kubectl /usr/local/bin/kubectl
    
    kubectl version


### Install Minikube

```
curl -Lo minikube https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64 \
 && chmod +x minikube
    
sudo mkdir -p /usr/local/bin/
sudo install minikube /usr/local/bin/
```

### Confirm Installation 

Depending on what virtualization layer you're using, you'll need to specify the correct driver type. [More information is available here](https://kubernetes.io/docs/setup/learning-environment/minikube/). 

https://minikube.sigs.k8s.io/docs/reference/drivers/


    minikube start --vm-driver=<driver_name>
    
    minikube start --vm-driver=virtualbox

    minikube start --vm-driver=kvm2

    minikube status
    
To stop your cluster, run:

    minikube stop


## Interfaces

### kubectl

kubectl is the default CLI for managing kubernetes

### K9s - Command line 

K9s - Kubernetes CLI To Manage Your Clusters In Style!

https://github.com/derailed/k9s

Binary releases

https://github.com/derailed/k9s/releases

### VS Code 

Use a graphical tool to get an overview of the environment, something as simple as the Kubernetes extension for Visual Studio Code works.

https://code.visualstudio.com/docs/azure/kubernetes



## Helm

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

## Deploy a service

Start with something simple. Nginx serving static files seems like a good place to begin. 


 packages. 
