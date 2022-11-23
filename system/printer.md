# Printers

Check your router for the most up to date settings.

Ideally give your printer a static IP.

Jot it down once you find it. 

Keepass is ok for something like a printer, but generally [best to keep network configurations outside of keepass.](network.md)

## System Settings

Open the system settings

In settings open the printer setting 

Enter in the IP address of the printer.


Note the Printer type if available: e.g. HL-2270DW

Choose "LPD-Printer"

For the Driver, choose from above notes. e.g.

Brother HL-2170W Foomatic/hpijs-pcl5e (recommended)
Brother HL-2070N Foomatic/hl1250 (recommended)


if that doesn't work, choose "JetDirect-Printer"

## Troubleshooting

If a printer is on a different subnet from the computer, it can be challenging to get more details about why it's failing. 

You can use the local CUPS interface to see more details

http://ip-of-printer:631/jobs/?

via: 
http://tips4pc.com/linux-tips/secret-hidden-interface-fix-linux-printer-problems.htm

From there, search for the error messages. In my case, the recommended driver did not work, but the first choice did. 

https://serverfault.com/questions/602523/cups-is-not-printing-with-filter-failed-message-how-to-get-more-info

