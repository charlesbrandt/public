# Technical Notes

Be sure to get the submodules:

    git clone --recurse-submodules https://github.com/charlesbrandt/system public

or 

    git submodule update --init --recursive


## Editor

```
cd
rm .emacs
rm .emacs.d
ln -s ~/public/system/editors/emacs/.emacs .emacs
ln -s ~/public/system/editors/emacs/.emacs.d .emacs.d
```


## Terminal

```
cd
rm .bashrc
ln -s ~/public/system/linux/.bashrc .bashrc
```

## TOC / Topics / Categories:

  - [System](system/)

    - [Administration](system/administration/)
  
  - [Documentation](documentation/)
  
  - [Code](code/)
  
  - [Web](web/)


