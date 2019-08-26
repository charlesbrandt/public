# System Notes

Be sure to get the submodules:

    git clone --recurse-submodules https://github.com/charlesbrandt/system

or 

    git submodule update --init --recursive

Try setting up a computer with a fresh operating system. In general, I find it easier to set up a system from scratch quickly, rather than fix a broken system or upgrade an obsolete system.  I feel more confident that the system is clean and stable that way.  This is also a good chance to make sure note taking, configuration management, and backup habits are working.

If you don't have a preferred operating system, consider one that is open source. Open source software is a wonderful thing, and I encourage you to take some time to learn why:
general/free_software.txt

Linux is the most common on desktop machines in the open source world. There are many flavors:

[Linux distributions](linux-distributions.md)

Ubuntu is an open source linux operating system.  

http://www.ubuntu.com

Whatever you choose, start by downloading and burning an image of the operating system (OS) of your choice from their site.

If you don't have the ability to download and burn CDs or flash drives, there are also sources to order the system on CD for minimal cost, or you can look into transferring the image to a USB thumb drive.

Once you have your copy of Ubuntu, boot your computer with it and Ubuntu should load into a "Live" instance.  This is a fully functional Linux system.  Cooool.  With enough memory, the only noticable performance hit comes when the system needs to access a program from the source media.  

At this point you are also able to install Ubuntu to a local drive. Whether you run live or install to disk, that's up to you.

For more details on installing a new Ubuntu system, see:
system/ubuntu.txt

When running from an Ubuntu live session, I add some configurations with:
system/startup.sh
Be sure to update $NAME to be your name/username and $USBPATH to point to where this instance is.

# ansible
A collection of ansible scripts and notes about setting up different systems. Also includes notes on common system administration tasks. These notes change based on current personal configurations.

When starting a new system, the trick is getting the pieces you need most installed first.  

For (X)Ubuntu systems, see ubuntu.txt for downloading packages and startup.sh for installing them.

These are always changing as systems change.
