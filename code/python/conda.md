# Conda

Similar to a virtualenv. However, all common dependencies get included. 

https://www.anaconda.com/download/

## Install

https://docs.anaconda.com/free/anaconda/install/linux/#installing-on-linux

Prerequisites for GUI tools:

```
sudo apt-get install libgl1-mesa-glx libegl1-mesa libxrandr2 libxrandr2 libxss1 libxcursor1 libxcomposite1 libasound2 libxi6 libxtst6
```

```
curl -O https://repo.anaconda.com/archive/Anaconda3-2023.09-0-Linux-x86_64.s
```

Verify integrity of file with `shasum -a 256 [filename]`

https://docs.anaconda.com/free/anaconda/reference/hashes/


```
# Include the bash command even if you aren't using the Bash shell
# Replace ~/Downloads with your actual path
# Replace the .sh file name with the name of the file you downloaded
bash ~/Downloads/Anaconda3-2020.05-Linux-x86_64.sh
```



Want conda to launch automatically? I'm going to go with no for now:

```
installation finished.
Do you wish to update your shell profile to automatically initialize conda?
This will activate conda on startup and change the command prompt when activated.
If you'd prefer that conda's base environment not be activated on startup,
   run the following command when conda is activated:

conda config --set auto_activate_base false

You can undo this by running `conda init --reverse $SHELL`? [yes|no]
[no] >>> 

You have chosen to not have conda modify your shell scripts at all.
To activate conda's base environment in your current shell session:

eval "$(/home/account/anaconda3/bin/conda shell.YOUR_SHELL_NAME hook)" 

To install conda's shell functions for easier access, first activate, then:

conda init

Thank you for installing Anaconda3!


```

## Environments

**Create the Conda environment:** Open a terminal and navigate to the directory where you want the environment to be associated.

```
cd /path/to/your/directory
```

Create a new Conda environment with the desired name (e.g., `myenv`), and specify the packages and versions you need:

```
conda create --name myenv python=3.8 pandas numpy
```
    
**Activate the Conda environment:**Whenever you need to use this environment, navigate to the directory in your terminal and then run the activation command:
    
```
conda activate myenv
```

## Dependencies

If a dependency is available through `conda` use that. Check to see:

```
conda install modulename
```

If not, `pip` is still available here

```
pip install modulename
```
Conda manages installed modules directly. No need for something like `pip` in this case. 

keep track of the installed dependencies with

```
conda list --export > conda-requirements.txt
pip freeze > pip-requirements.txt
```

### Delete Environment

Find the path with 

```
conda env list
```

Then simply delete the corresponding directory 

```
rm -rf /home/account/anaconda3/envs/myenv
```


## Uninstall

If you want to completely uninstall Conda from your system and start from scratch, you would follow a process similar to the steps outlined below.

For Anaconda Distribution:

Deactivate Conda Environment:
First, exit from any active Conda environments by running:

```
conda deactivate
```

Repeat until you're back to your base system prompt.

Remove Conda Directories:

After cleaning up the configurations, you can manually remove the Conda installation directory. The default is typically ~/anaconda3 or ~/miniconda3 depending on which distribution you installed.

```
rm -rf ~/anaconda3
```

or, if you installed Miniconda:

```
rm -rf ~/miniconda3
```

Remove the Remaining Files and Hidden Directories:

If the anaconda-clean tool was used, it should have handled configuration files and directories. However, if there are any remaining files or directories (like .condarc, .conda, or .continuum), remove them with:

```
rm -rf ~/.condarc ~/.conda ~/.continuum
```

Remove Conda from PATH Environment Variable:

Edit your shell's initialization file (such as ~/.bashrc, ~/.bash_profile, ~/.zshrc, etc.) and remove any lines that add Conda directories to the PATH variable. 

If you're reinstalling, ok to skip last step
