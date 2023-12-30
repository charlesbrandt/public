# Python


Python is a good language for writing software. It encourages clean, readable code. It's a nice interface to the underlying system.

A great choice for synchronous workflows. Synchronous workflows can help processes on the system side. For example, walking a file system and reporting when the job finishes. 

Python doesn't run client-side on web browsers. For that you'll need Javascript (JS). Python shows up frequently on the API side or server side. 

## Jupyter Notebooks

[Jupyter Notebooks](jupyter-notebooks.md)

## Virtual Environments

Like docker, but for your code environment. Prevents installing modules in the global library. 

[Virtual Environments](virtualenv.md)
If you're already running in a docker container, the benefits may be moot.

## New Project

Best practices for starting a python project

Files and Directories that should be included:

https://docs.python-guide.org/writing/structure/

https://gist.github.com/sloria/7001839

I like starting with a template script to include things I like when getting started. A boilerplate. 

```
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
def read_metadata(source):
    items = []
    if pathlib.Path(source).is_file():
        with open(source, 'r+') as f:
            items = json.load(f)
    return items
```


Write

```
def write_metadata(items, destination):
    with open(destination, "w") as f:
        json.dump(items, f, indent=2, default=str)

```

## CSV

[CSV Files](csv.md)

## OS

[OS modules](os.md)

## Sleep / Wait

```
from time import sleep 
sleep(delay)
```

## Datetimes

Use UTC in the system to simplify

```
from datetime import datetime,timezone
now_utc = datetime.now(timezone.utc)
```

https://stackoverflow.com/questions/15940280/how-to-get-utc-time-in-python

Then, to print it in a format understood in JS:

```
.strftime("%Y-%m-%dT%H:%M:%S.%f3Z")
```

https://stackoverflow.com/questions/26193065/print-current-utc-datetime-with-special-format

https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior

## Logging

```
print(f"{stuff}")
```

Sooner or later it's a good idea to keep track of what was done when  
and ensure that those activities are written no matter what happens in the code (e.g. a crash)

enter logging

https://docs.python.org/3/howto/logging.html  
Logging HOWTO — Python 3.10.4 documentation  


```
import logging
import os
log_dest = os.path.join(metadata_dir, "example.log")
logging.basicConfig(filename=log_dest, level=logging.DEBUG, format='%(asctime)s: %(levelname)-8s: %(message)s', datefmt='%Y-%m-%d %H:%M:%S')

logging.debug('This message should go to the log file')
logging.info('So should this')
logging.warning('And this, too')
logging.error('And non-ASCII stuff, too, like Øresund and Malmö')
```


Initialize your logging instance in the main script to control the destination. 

If all settings should be the same across an application, use a common library that gets imported to other scripts like `common.py` or `helpers.py`.

### Multiple Logs

```python

    # log results from each validation separately for easier tracking
    LOG_FORMAT = ('%(asctime)s: %(levelname)-8s: %(message)s')
    LOG_LEVEL = logging.DEBUG

    tar_file = os.path.basename(tar_path)

    now_utc = datetime.now(timezone.utc)
    destination_name = tar_file.replace('.tar', '.validate.')
    destination_name += now_utc.strftime("%Y%m%d")
    log_destination = os.path.join(
        metadata_dir, snapshot, destination_name)

    print("Logging to:", log_destination)

    validate_logger = logging.getLogger("validate.details")
    validate_logger.setLevel(LOG_LEVEL)
    validate_logger_file_handler = logging.FileHandler(log_destination)
    validate_logger_file_handler.setLevel(LOG_LEVEL)
    validate_logger_file_handler.setFormatter(
        logging.Formatter(LOG_FORMAT, datefmt='%Y-%m-%d %H:%M:%S'))
    validate_logger.addHandler(validate_logger_file_handler)

```

Then pass the `validate_logger` around to use just like `logger`. Maybe there's a better way?



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

```
sudo pip install -e sortable/
```

## Installation

The context you are running in will determine the python environment. 

### Ubuntu



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

```
sudo apt install python3-pip
```

Add `.local/bin` to the PATH by editing ~/.bashrc

```
PATH="/opt/local/bin:~/.yarn/bin:~/.local/bin:${PATH}"
export PATH
```

All other dependencies should be installed as part of the container so they can be tracked with the project.

