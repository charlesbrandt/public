# Unmount a locked disk 

When a disk cannot be ejected due to a process having an open file,
can use:

    lsof | grep /media/account/whatever

to find the active process(es) using the resource

See also

https://unix.stackexchange.com/questions/3109/how-do-i-find-out-which-processes-are-preventing-unmounting-of-a-device
