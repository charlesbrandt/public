If running linux on apple hardware (e.g. macbooks) you may need to swap the command key with the alt key to work as expected. Additionally, if running a linux VM on mac hardware using VirtualBox, you may need to first adjust the Host Key Combination from "Left Command" to "Right Command" (via Input -> Keyboard).
Then, use xev command to see the keys (keycode 133 == Super_L, want to swap with keycode 64 == Alt_L)
vi modmap.file

keycode 64 = Alt_L
keycode 133 = Super_L
remove Mod1 = Alt_L
remove Mod4 = Super_L
add Mod1 = Super_L
add Mod4 = Alt_L

xmodmap -v modmap.file
#worked for me, then move so it runs by default (this does not run in Xfce):
mv modmap.file ~/.xmodmaprc
#this does not run by default, so add a startup item (Settings->Session and Startup):
/bin/bash -c "sleep 20; /usr/bin/xmodmap /home/$USER/.xmodmaprc"

#this may also work if running linux natively on apple hardware (not in VM):
echo "1" > /sys/module/hid\_apple/parameters/swap\_opt\_cmd
#solution via:
http://unix.stackexchange.com/questions/86933/swap-alt-and-super
https://wiki.xfce.org/faq

Another setting that helps with scrolling on macs is (when guest is powered off), in VirtualBox->Machine->Settings->System->Motherboard->Pointing Device
set it to: "PS/2 Mouse"

