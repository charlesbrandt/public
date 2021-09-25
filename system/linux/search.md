# Searching

See also: [../drives/locate.md](../drives/locate.md)


## Command Line

On *nix systems, the following command line interface (CLI) tools are usually available. 


### Locate

this uses a database to look for something. 
This can be fast, and can include sources that are not currently online. 

Should be easy to shard off searches (where do you want to look?). Base it on collections. 

    locate
    
Is there a way to limit the scope of the search? The whole system can often return too many values to parse through manually.


### Find 

there is also find

if you just want to look for file names (not look within a file)
this is a good place to start
much faster than a grep

    find * -iname "*{{look_for}}*"

lots of parameters that can help. What about narrow by extension?


### Grep

grep -ir "look for" * 

grep -ir "look for" * | grep -i "one more thing"
