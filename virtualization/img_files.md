# IMG files

Some distributions are shipped as .img files. These are meant to be written to a USB device for creating installation media. 

VirtualBox does not know how to mount these by default. 

However, it's possible to convert them to a VDI. 

    cd ~/Downloads
    vboxmanage convertfromraw -format VDI name_of_file.img name_of_file.vdi

via:
http://www.kombitz.com/2009/10/06/how-to-mount-an-img-file-on-virtualbox/

Also, be sure to allot at least 8gb of storage space for the new disk image after converting, otherwise there won't be room for the initramfs. 
I did this via Global Tools in VirtualBox

https://duckduckgo.com/?q=tails+unable+to+find+a+medium+containing+a+live+file+system&t=canonical&ia=web

