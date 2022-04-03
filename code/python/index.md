# Python


Python is a good language for writing software. It encourages clean, readable code. It's a nice interface to the underlying system.

A great choice for synchronous workflows. Synchronous workflows can help processes on the system side. For example, walking a file system and reporting when the job finishes. 

Python doesn't run client-side on web browsers. For that you'll need Javascript (JS). Python shows up frequently on the API side or server side. 

## Installation

The context you are running in will determine the python environment. 

### Docker

For virtualization, I prefer running a container and describe the requirements in a `Dockerfile`.

Create a container with `docker-compose.yml`

```
  python:
    build:
      context: python
      dockerfile: Dockerfile
    container_name: boilerplate_python
    # restart: unless-stopped
    volumes:
      - ./python:/srv/python
    #ports:
    #  - 8080:8080
    entrypoint: ["tail", "-f", "/dev/null"]
```

Choose versions that line up with what you have available in production.  
In `python/Dockerfile`: 

```
FROM python:3

# Do this so the requirements are available when the image is built
COPY requirements.txt /srv/python/requirements.txt
WORKDIR /srv/python
RUN pip install -r requirements.txt

```

If you need access to both node/js and python in the same container (e.g. bullmq + python worker):

https://hub.docker.com/r/nikolaik/python-nodejs/#!

python3.9-nodejs12 	3.9.12 	12.22.11 	buster  
python3.9-nodejs12-bullseye 	3.9.12 	12.22.11 	bullseye  
python3.9-nodejs12-slim 	3.9.12 	12.22.11 	slim  
python3.9-nodejs12-alpine 	3.9.12 	12.22.11 	alpine  

```
FROM nikolaik/python-nodejs:python3.9-nodejs12

# Do this so the requirements are available when the image is built
COPY requirements.txt /srv/python/requirements.txt
WORKDIR /srv/python
RUN pip install -r requirements.txt

```

`requirements.txt`
TODO: best way to specify versions?

```
# https://stackoverflow.com/questions/16584552/how-to-state-in-requirements-txt-a-direct-github-source

-e git://github.com/charlesbrandt/sortable#egg=sortable
requests
```

### Shared System

On shared systems, you may not have the ability run containers or to install your own python packages if using the system python. In these situations, a self contained Python environment can come in handy. 

```
cd /srv/path/to/code

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && bash ./Miniconda3-latest-Linux-x86_64.sh -b -p /srv/path/to/code/bin/miniconda

cd bin

miniconda/bin/pip3 install pymongo
```

### Local

Ubuntu systems come with Python installed by default. Is pip available? 

Pip is useful:

    sudo apt install python3-pip

Add `.local/bin` to the PATH by editing ~/.bashrc

```
PATH="/opt/local/bin:~/.yarn/bin:~/.local/bin:${PATH}"
export PATH
```

All other dependencies should be installed as part of the container so they can be tracked with the project.

### Virtual Environment

[Virtual Environments](virtualenv.md)


## New Project

Best practices for starting a python project

Files and Directories that should be included:

https://docs.python-guide.org/writing/structure/

https://gist.github.com/sloria/7001839

I like starting with a template script to include things I like when getting started. A boilerplate. 

```
#!/usr/bin/env python3
"""
#!/usr/bin/env python3
"""
# By: 
# On: [date]
# License: MIT 

Usage:
template_script.py [source] [destination]
"""

import os
import sys


def usage():
    print(__doc__)


def read_file(source):
    with open(source, 'r') as f:
        for line in f.readlines():
            print(line)


def merge_logs(source, destination):
    if source and os.path.exists(source):
        options = os.listdir(source)
        print(options)
    else:
        usage()
        print("Couldn't find source path: %s" % source)
        exit()


if __name__ == '__main__':
    source = None
    destination = None
    if len(sys.argv) > 1:
        helps = ['--help', 'help', '-h']
        for i in helps:
            if i in sys.argv:
                usage()
                exit()
        source = sys.argv[1]
        if len(sys.argv) > 2:
            destination = sys.argv[2]

    merge_logs(source, destination)

```


## Walking Directories

```
def walk_files(source, destination):
    if os.path.exists(source):
        for root, dirs, files in os.walk(source):
            for f in files:
                if re.search('\?', f):
                    sname = os.path.join(root, f)
                
    else:
        print("Couldn't find path: %s" % source)
        exit()
```

## f-strings

Python 3.6 introduced a handy new syntax for substituting variables in a string. Prefix with an `f` and use curly brackets to designate the variables in the string:

```
name = "human"
print(f"Hello {name}")
```

## JSON

Use the built in library:

```
import json
```

Read

```
if Path(source).is_file():
    with open(source, 'r+') as f:
        items = json.load(f)
```


Write

```
def add_items(items, destination):
    with open(destination, "w") as f:
        json.dump(items, f, indent=2, default=str)

```

## Testing

Set up a test to describe the behavior you want to see. It's okay to use a template. 

Then run the test. 

Helps to have this process built in early.

I have used nosetests in the past, but it may be sufficient to just use the built in unit testing at this point. py.test is also recommeded

https://docs.python-guide.org/writing/tests/
Testing Your Code — The Hitchhiker's Guide to Python

Make it easy to run tests.

Run tests often. Before you start making changes. After changes are complete. And all along the way in between.

https://docs.python.org/3/library/unittest.html

### py.test

py.test is a more popular alternative to nose(2) test:

https://docs.pytest.org/en/latest/

These simplify the syntax of the native unnittest library with shortcuts like 'assert'. 

    pip3 install pytest

pwd
/data/data/com.termux/files/home/public/moments
$ pip3 install pytest

then in directory with tests:

    pytest


## Learning

Many great introductions to the language exist. Here are some resources that looked strong. There may be new options available. Try searching for "python learning resourxrs".

http://www.greenteapress.com/thinkpython/thinkpython.html

http://openbookproject.net//thinkCSpy/
How to Think Like a Computer Scientist — How to Think Like a Computer Scientist: Learning with Python v2nd Edition documentation

http://diveintopython.org/
Dive Into Python


## Regular Expressions

https://www.debuggex.com/cheatsheet/regex/python

https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial

The group() function returns the string matched by the re


## Development

how to install a module for development with pip?

https://pip.pypa.io/en/stable/reference/pip_install/

    sudo pip install -e sortable/
