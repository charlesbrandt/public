# Searching

## Command Line

On *nix systems, the following command line interface (CLI) tools are usually available. 


### Locate

this uses a database to look for something. 
This can be fast, and can include sources that are not currently online. 

Should be easy to shard off searches (where do you want to look?). Base it on collections. 

```
locate
```

Is there a way to limit the scope of the search? The whole system can often return too many values to parse through manually.


### Find 

there is also find

if you just want to look for file names (not look within a file)
this is a good place to start
much faster than a grep

```
find * -iname "*{{look_for}}*"
```

lots of parameters that can help. What about narrow by extension?


### Grep

```
grep -ir "look for" * 

grep -ir "look for" * | grep -i "one more thing"
```


## Locating Files

use updatedb to create indexes of different drives
then use a locate command to quickly query different drive catalogs

Locate databases can be used to create an index of what exists on a filesystem. This is useful for documenting what is on a drive before taking it offline or re-formatting it. 

### updatedb & locate

    sudo updatedb -U /media/account/ -o database.file.name

(for more options, see:

    man updatedb

Then, to use a previously generated database file to look for files, use locate:

```
    man locate

      -d, --database DBPATH
              Replace  the  default database with DBPATH.  DBPATH is a :-sepa‐
              rated list of database file names.  If more than one  --database
              option  is  specified,  the resulting path is a concatenation of
              the separate paths.
```

### Print contents (TODO)

how to unpack a locate database and print all files from it
(printing to screen is sufficient)
    
### Links

#### update db

https://duckduckgo.com/?q=updatedb+specify+output+file&t=canonical&ia=web  
updatedb specify output file at DuckDuckGo  
https://www.lifewire.com/updatedb-linux-command-4095883  
updatedb - Linux Command  
https://unix.stackexchange.com/questions/379725/what-kind-of-database-do-updatedb-and-locate-use#379729  
find - What kind of database do `updatedb` and `locate` use? - Unix & Linux Stack Exchange  
https://www.tutorialspoint.com/unix_commands/updatedb.htm  
updatedb - Unix, Linux Command - Tutorialspoint  
  
  
https://duckduckgo.com/?q=linux+create+list+of+all+files+on+a+drive&t=canonical&ia=web  
linux create list of all files on a drive at DuckDuckGo  
https://stackoverflow.com/questions/105212/recursively-list-all-files-in-a-directory-including-files-in-symlink-directories  
linux - Recursively list all files in a directory including files in symlink directories - Stack Overflow  
https://duckduckgo.com/?q=linux+locate+database&t=canonical&ia=web  
linux locate database at DuckDuckGo  
http://www.linfo.org/locate.html  
How to use the locate command, by The Linux Information Project  



#### Windows utilities

https://duckduckgo.com/?q=index+of+drive&t=canonical&ia=web  
index of drive at DuckDuckGo  
http://ask-leo.com/how_do_i_print_a_list_of_whats_on_my_usb_drive.html  
How do I print a list of what's on my USB drive?  
https://duckduckgo.com/?q=karens+directory+printer+linux&t=canonical&ia=web  
karens directory printer linux at DuckDuckGo  
https://alternativeto.net/software/directory-printer/  
Karen's Directory Printer Alternatives and Similar Software - AlternativeTo.net  


