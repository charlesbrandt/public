# Virtualization

Virtualization allows system services to be isolated from one another. 

Containers are a lighter-weight alternative in terms of resource utilization since they only isolate the running applications but still share some of the host operating system services. 

Virtual Machines emulate the hardware needed to run a computer and then run a complete copy of the operating system in that virtual environment. 

## Containers

[Docker](docker.md)  
[Docker Compose](docker-compose.md)  
[Dockerfile](dockerfiles.md)  

### Orchestration

Once you start having a lot of different containers, you'll want a tool to help you keep track of all of the different setups. Orchestration tools can help with that. 

Kubernetes and Helm can help with this. 

Container Orchestration. Compared to Docker Swarm. Kubernetes seems to be the go-to solution for this level of virtualization  

[Kubernetes](kubernetes.md)  


## Virtual Machines

[Virtual Machine](virtual-machine.md)  

[Physical to Virtual](p2v.md)  
[Img Files](img-files.md)  

[KVM](kvm.md)  

[Virtual Box](virtual-box.md)  

[Virtual Box Guests](virtual-box-guests.md)  
