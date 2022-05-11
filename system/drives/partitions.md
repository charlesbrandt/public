# Partitions

Common layouts

If you want something like a swap partition so that you can hibernate the machine, it's better to lay the drive partitions out manually. Manual layouts require knowing all the other things that the system is taking care of for you. 

## EFI System Partition

How big should it be? 

This time (2020.12.19) I went with 500 MB

https://www.ctrl.blog/entry/esp-size-guide.html

https://duckduckgo.com/?q=how+big+to+make+efi+partition

## Swap

How big should it be?

In my opinion, it should be slightly larger than the available system memory so that you can hibernate the machine. 

32GB of memory? 32,768 MB (often need to use MB in partition tools)

I've read some sources that using any SSD as swap can be harmful to the SSD. <--- have not looked into this in more detail. YMMV


## root

`/` 

The main space.

Save this for everything left over at the end
