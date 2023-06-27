# Jupyter Notebooks

A web based interpreter available to run on any system. Useful for remote systems. 

Similart to Colab Notebooks. If you need a cloud solution, Google makes this one available to all. It is running Jupyter Notebooks behind the scenes, just integrates Google's cloud service for storage and sharing. 


## Virtual Environment

```
python -m venv venv
source venv/bin/activate
pip install jupyter
ipython kernel install --user --name=venv
jupyter notebook
```

https://stackoverflow.com/questions/55448244/using-virtualenv-on-jupyter-notebook

This will launch the jupyter notebook and make it available locally. You could access it remotely via an ssh tunnel (port forward). 

You can also configure it to be available on the local network. 

```
jupyter notebook --generate-config
```



## Docker

TODO: Should be possible to get access to GPU via a docker container, but I haven't been able to get that to work just yet. 

https://github.com/jupyter/docker-stacks

https://github.com/iot-salzburg/gpu-jupyter

Enable GPU support in docker:

```
sudo apt-get update 
    && sudo apt-get install -y nvidia-container-toolkit-base
```
https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#installation-guide

```
distribution=$(. /etc/os-release;echo $ID$VERSION_ID) \
      && curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
      && curl -s -L https://nvidia.github.io/libnvidia-container/$distribution/libnvidia-container.list | \
            sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
            sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list
```


