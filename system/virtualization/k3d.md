# K3d

https://k3d.io/v5.2.2/#installation

```
wget -q -O - https://raw.githubusercontent.com/rancher/k3d/main/install.sh | bash
```

k3d installed into /usr/local/bin/k3d
Run 'k3d --help' to see what you can do with it.

k3d is a wrapper CLI that helps you to easily create k3s clusters inside docker.
Nodes of a k3d cluster are docker containers running a k3s image.
All Nodes of a k3d cluster are part of the same docker network.

Usage:
  k3d [flags]
  k3d [command]

Available Commands:
  cluster      Manage cluster(s)
  completion   Generate completion scripts for [bash, zsh, fish, powershell | psh]
  config       Work with config file(s)
  help         Help about any command
  image        Handle container images.
  kubeconfig   Manage kubeconfig(s)
  node         Manage node(s)
  registry     Manage registry/registries
  version      Show k3d and default k3s version

Flags:
  -h, --help         help for k3d
      --timestamps   Enable Log timestamps
      --trace        Enable super verbose output (trace logging)
      --verbose      Enable verbose output (debug logging)
      --version      Show k3d and default k3s version

Use "k3d [command] --help" for more information about a command.

## Quick Start¶

Create a cluster named mycluster with just a single server node:

```
k3d cluster create mycluster
```

Use the new cluster with kubectl, e.g.:

```
kubectl get nodes
```

```
kubectl cluster-info
```

