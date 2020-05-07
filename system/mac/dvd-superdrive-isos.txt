*2017.02.15 10:22:02
it is possible to use Apple's Superdrive hardware with linux. This is a good guide:

https://christianmoser.me/use-apples-usb-superdrive-with-linux/

Summary:

    sudo apt-get install sg3-utils

    ls /dev

    sg_raw /dev/sr0 EA 00 00 00 00 00 01

Now it should be possible to insert optical media. This was good enough for a quick solution. Other options are mentioned in original reference.


TODO:
copy here for archives (just in case resource disappears)

TODO:
easy way to convert HTML to markdown (script)


How to create ISO from DVD:

    mkisofs -r -o windows_pro_10-64bit-english.iso /media/brandtc/CCSA_X64FRE_EN-US_DV5

That command worked, but I have not tested the resulting ISO file yet.

via:
http://askubuntu.com/questions/136165/how-to-create-iso-images

(
For Windows 10, you can download an ISO directly from Microsoft, FYI:
http://www.microsoft.com/en-us/software-download/windows10ISO
)
