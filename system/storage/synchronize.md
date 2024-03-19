# Synchronize Drives

`rsync` is the simplest way to synchronize two drives. 

Can't remember where to sync to/from? leave notes in `sync.md` with the specific commands used. 

I forget... do I keep a slash at the end? or does that cause it to be in a subdirectory. 

If the destination directory exists, keep a slash at the end of the source directory. If the destination does not already exist, keeping a slash at the source directory will result in everything in source being placed in the main directory of the destination, which may not be what you want. 


## Comparisons / Diff

To compare two directories:

```
diff -q directory-1/ directory-2/
```

or recurse into subdirectories:

```
diff -qr directory-1/ directory-2/
```

via:  
https://www.tecmint.com/compare-find-difference-between-two-directories-in-linux/


## meld

There is also a utility called meld. Haven't tried that yet. Notes?

```
sudo apt install meld
```