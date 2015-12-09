to create image:

sudo dd if=/dev/sda of=/media/images/thinkpad-winxp-20090427.dd

to restore image:

sudo dd if=/media/images/thinkpad-winxp-20090427.dd of=/dev/sda 

with gzip:

sudo dd if=/dev/sda | gzip -9 > /media/images/compaq-netbook-winxp-apps_installed-20091230.gz

gunzip /media/images/compaq-netbook-winxp-20091227.gz - | dd of=/dev/sda
