# Python

Python is a good language for writing software. It encourages clean, readable code. It's a nice interface to the underlying system.

About the only downside is that it doesn't run clientside on web browsers. For that you'll need Javascript (JS).

## Installation

Many systems come with Python installed by default, but are a few other tools that are good to install globally.

    sudo apt install python3-pip

Make sure you have pipenv installed:

    pip3 install pipenv

Add `.local/bin` to the PATH by editing ~/.bashrc

```
PATH="/opt/local/bin:~/.yarn/bin:~/.local/bin:${PATH}"
export PATH
```

All other dependencies should be installed as part of the virtual environment (virtual-env) so they can be tracked with the project.

## New Project

Best practices for starting a python project
    
Install dependencies using pipenv:

    cd project_folder
    pipenv install moments

At this point you can launch the virtualenv shell (`pipenv shell`), or run individual commands within the environment (`pipenv run`). 


Files and Directories that should be included:

https://docs.python-guide.org/writing/structure/

https://gist.github.com/sloria/7001839

make a new virtualenv:

https://docs.python-guide.org/dev/virtualenvs/


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
