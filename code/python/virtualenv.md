# Virtual Environment

Virtual environments in Python are tools for managing dependencies and isolating different projects from each other. 

The basic pattern is to choose one of the virtual environment managers (see below). Then: 

  - Activate the virtual environment
    This sets the appropriate Python interpreter and ensures that any packages you install or scripts you run are within the scope of the virtual environment. 

  - Install dependencies within the virtual environment 
    Use the appropriate package manager (such as pip or conda) to install the dependencies specific to your project within the virtual environment. This ensures that the dependencies are isolated to the virtual environment and won't affect your system-wide Python installation.

There are several popular virtual environment managers available for Python, such as venv, virtualenv, and conda. These tools provide an easy and convenient way to create, manage, and activate virtual environments. You can choose the one that suits your workflow and learn how to use it effectively.

## Conda

[Conda](conda.md)

## Poetry

Similar to npm, but for python. Still requires manually activating / deactivating the virtual environment. 

https://python-poetry.org/docs/

https://python-poetry.org/
Poetry - Python dependency management and packaging made easy
https://github.com/python-poetry/poetry
GitHub - python-poetry/poetry: Python packaging and dependency management made easy


### Install

```
curl -sSL https://install.python-poetry.org | python3 -
```

Add `export PATH="/home/account/.local/bin:$PATH"` to your shell configuration file.

Test that everything is set up by executing

```
poetry --version
```

https://python-poetry.org/docs/#installing-with-the-official-installer

### Upgrade

Try uninstalling and then reinstall 
```
curl -sSL https://install.python-poetry.org | python3 - --uninstall
curl -sSL https://install.python-poetry.org | POETRY_UNINSTALL=1 python3 -
```

This didn't work for me

```
poetry self update
```

### Usage

Add poetry to existing project

```
poetry init
```

Add module to current project

```
poetry add [module name]
```

See what has already been added

```
poetry show --tree
```

https://python-poetry.org/docs/basic-usage/#specifying-dependencies
Basic usage | Documentation | Poetry - Python dependency management and packaging made easy


### Add packages from requirements.txt

```
cat requirements.txt | xargs -I % sh -c 'poetry add "%"'
```

https://duckduckgo.com/?t=ffab&q=poetry+install+requirements.txt&ia=web
poetry install requirements.txt at DuckDuckGo
https://stackoverflow.com/questions/62764148/how-to-import-requirements-txt-from-an-existing-project-using-poetry
python - How to import requirements.txt from an existing project using Poetry - Stack Overflow

### Running

Similar to `pipenv`

```
poetry run python ./check_devices.py 
```

or go into a shell

```
poetry shell
```

### Environments

```
poetry env list
```

You can remove an environment with:

```
poetry env remove [name of env from list]
```

Typically, virtualenvs are stored:

```
~/.cache/pypoetry/virtualenvs/
```

#### Local modules

How to use a local python module under a poetry managed environment?

TODO: Untested:

 To use a local Python module under a Poetry managed environment, you need to add it as a dependency in your `pyproject.toml` file and then install it locally. Here's a step-by-step guide:

1. Navigate to your project directory where your `pyproject.toml` file is located.

2. Open the `pyproject.toml` file in a text editor or IDE, and add the local package as a dependency under the `[tools.poetry]` section. For example:

```toml
[tools.poetry]
name = "my_project"
version = "0.1.0"

[dependencies]
local-package = { path = "/path/to/local/package", version = "0.1.2" }
```
Replace `/path/to/local/package` with the absolute path to your local package directory.

3. Save and close the file.

4. Install the local dependency using Poetry by running the following command in your terminal:

```bash
poetry add .
```
This will add your local package as a dev dependency in `pyproject.toml` and install it in your virtual environment.

5. Now you can import and use the local module in your Python code as if it were any other installed package. For example, in your script or module:

```python
from local_package import MyLocalModule

# Use MyLocalModule here...
```

6. Don't forget to activate your Poetry virtual environment before running your Python script or module. You can do this by running:

```bash
poetry shell
```
Then, you can run your Python script as usual with `python my_script.py`.




### venv

- Included by default  
- no extra dependencies  
- works with `requirements.txt`  

- manual activation  

### pipenv

`pipenv` has the advantage of working with exiting `requirements.txt` files. This can be helpful for external projects. 

Install `pipenv`

```
pip install pipenv --user
```

Install dependencies using pipenv:

```
cd project_folder
pipenv install moments
```

At this point you can launch the virtualenv shell (`pipenv shell`), or run individual commands within the environment (`pipenv run`). 

If you are using pipenv for virtual environment and package management, you can easily install dependencies from the requirements.txt file. Here's an example:

```sh
# Create a Pipfile and virtual environment
pipenv install --dev

# previous command automatically installs requirements.txt for me

# Install dependencies from requirements.txt
pipenv run pip install -r requirements.txt
```




### Links

https://duckduckgo.com/?t=ffab&q=python+pipenv+vs+poetry&ia=web
python pipenv vs poetry at DuckDuckGo

https://dev.to/frostming/a-review-pipenv-vs-poetry-vs-pdm-39b4
A Review: Pipenv vs. Poetry vs. PDM - DEV Community
https://github.com/pdm-project/pdm
GitHub - pdm-project/pdm: A modern Python package and dependency manager supporting the latest PEP standards



## virtualenv

make a new virtualenv:

https://docs.python-guide.org/dev/virtualenvs/


## Comparison

Here's a comparison of some popular tools for managing Python virtual environments: venv, virtualenv, pipenv, conda, and poetry.

    pipenv: pipenv is a popular tool that combines package management (using pip) and virtual environment management in one tool. It provides a Pipfile for managing dependencies and a Pipfile.lock for environment locking. It also has features like automatic environment activation, handling of dev dependencies, and improved security features. However, some users find its workflow and error messages to be complex or overwhelming.

    poetry: poetry is a newer tool for managing Python dependencies and virtual environments. It aims to simplify the process of managing dependencies, building, and packaging Python projects. It provides a pyproject.toml file for managing dependencies and has features like automatic virtual environment creation, comprehensive dependency resolution, and build and packaging capabilities. However, it may have a steeper learning curve for users who are not familiar with its unique approach and file structure.

    venv (built-in): venv is a built-in module in Python 3 that provides a simple way to create virtual environments. It's easy to use and comes with Python by default. However, it lacks some advanced features like dependency management and environment locking, and it may not be compatible with older versions of Python.

    virtualenv: virtualenv is a widely used third-party tool for creating virtual environments. It supports both Python 2 and Python 3 and provides more advanced features like environment activation scripts and the ability to clone an existing environment. However, it does not include built-in dependency management.

    conda: conda is a cross-platform package management and environment management tool provided by Anaconda for scientific computing and data science. It supports multiple programming languages, including Python, and provides robust dependency management, environment isolation, and reproducibility. It also has a large ecosystem of pre-built packages for scientific computing. However, it may introduce additional complexity if you're not using it for its intended use case, and it requires a separate installation if you're not already using Anaconda.

