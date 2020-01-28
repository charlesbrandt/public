# Kubernetes

Currently following along and summarizing these resources:

https://blog.codeship.com/getting-started-with-kubernetes/

Choose where you want to run your Kubernetes cluster

https://kubernetes.io/docs/setup/
   
https://kubernetes.io/docs/tutorials/kubernetes-basics/create-cluster/cluster-intro/

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

    minikube start --vm-driver=<driver_name>
    
    minikube start --vm-driver=virtualbox

    minikube status
    
To stop your cluster, run:

    minikube stop

## Deploy a service

Start with something simple. Nginx serving static files seems like a good place to begin. 
