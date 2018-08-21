SYSTEM NOTES
-----------------
These notes document how I set up a computer with a fresh operating system. In general, I find it easier to set up a system from scratch quickly, rather than fix a broken system or upgrade an obsolete system.  I feel more confident that the system is clean and stable that way.  This is also a good chance to make sure my note taking and backup habits are working.

Notes on setting up different operating systems are included in:
system/

If you don't already have a preference in operating systems, you might try Ubuntu.
http://www.ubuntu.com

Ubuntu is an open source operating system.  Open source software is a wonderful thing, and I encourage you to take some time to learn why:
general/free_software.txt

Ubuntu makes getting started easy.  Start by downloading and burning an image of Ubuntu from their site.

If you don't have the ability to download and burn CDs, there are also sources to order the system on CD for minimal cost, or you can look into transferring the image to a USB thumb drive.

Once you have your copy of Ubuntu, boot your computer with it and Ubuntu should load into a "Live" instance.  This is a fully functional Linux system.  Cooool.  With enough memory, the only noticable performance hit comes when the system needs to access a program from the source media.  

At this point you are also able to install Ubuntu to a local drive. Whether you run live or install to disk, that's up to you.

For more details on installing a new Ubuntu system, see:
system/ubuntu.txt

When running from an Ubuntu live session, I add some configurations with:
system/startup.sh
Be sure to update $NAME to be your name/username and $USBPATH to point to where this instance is.

# system
A collection of ansible scripts and notes about setting up different systems. Also includes notes on common system administration tasks. These notes change based on current personal configurations.

When starting a new system, the trick is getting the pieces you need most installed first.  

For (X)Ubuntu systems, see ubuntu.txt for downloading packages and startup.sh for installing them.

These are always changing as systems change.
