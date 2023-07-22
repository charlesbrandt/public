# Jupyter Notebooks

A web based interpreter available to run on any system with Python. Useful for remote systems. 

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

This will launch the jupyter notebook and make it available locally. 

## Connections

### SSH 

You could access it remotely via an ssh tunnel (port forward). 


### Remote IP

You can also configure it to be available on the local network. 

```
jupyter notebook --generate-config
```
Then edit the config

```
micro ~/.jupyter/jupyter_notebook_config.py
```

Especially: set the IP if you want to access from another machine on the network. 

https://jupyter-notebook.readthedocs.io/en/stable/public_server.html

## Run

Once in the right environment, run with:

```
jupyter notebook
```

### Environment Variables


Use a `%` magic commands, like `%env`, e.g., `%env MY_VAR=MY_VALUE` or `%env MY_VAR MY_VALUE`. 
Use `%env` by itself to print out current environmental variables.

[python - How to set env variable in Jupyter notebook - Stack Overflow](https://stackoverflow.com/questions/37890898/how-to-set-env-variable-in-jupyter-notebook)
]
