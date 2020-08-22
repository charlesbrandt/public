# Browsers

The window to the internet.  
Settings for an optimal session. 

## Firefox

Running multiple browsers with different profiles helps keeps active references in the right place. When the work is finished, just close that browser. Good for keeping memory free. 

If you need a browser context for a specific task / job

`firefox -p` does the trick! 

If no relevant profile is available, just create one.

Be sure to set Menu -> Preferences -> Restore previous session.  
Pick your preferred search engine while you're at it!

### Tab Links

https://addons.mozilla.org/en-US/firefox/addon/copy-all-tabs/  
Copy All Tabs :: Add-ons for Firefox

have used this in the past as well:  
https://addons.mozilla.org/en-US/firefox/addon/copy-urls-expert/?src=search  
and this may have similar functionality  
https://addons.mozilla.org/en-US/firefox/addon/tab-mix-plus  

OneTab?

### Tab Suspender

'Auto Tab Discard' does what I'm after. Closest working alternative to 'The Great Suspender'

https://addons.mozilla.org/en-US/firefox/addon/auto-tab-discard/  
https://www.google.com/search?q=firefox+tab+suspender

https://duckduckgo.com/?q=great+suspender+firefox&t=canonical&ia=web  
great suspender firefox at DuckDuckGo  
https://www.reddit.com/r/firefox/comments/374gx3/chromes_the_great_suspender_for_firefox/  
Chrome's 'The Great Suspender' for firefox? : firefox  
https://addons.mozilla.org/en-US/firefox/addon/unload-tabs/  
UnloadTabs – Get this Extension for 🦊 Firefox (en-US)  

this one didn't work so well for me.. major memory usage  
https://addons.mozilla.org/en-US/firefox/addon/ff-tab-suspender/  
Tab Suspender – Get this Extension for 🦊 Firefox (en-US)  

### Ad Blockers

Instead of adblock, I have been exploring a more general filter / tracking blocker:  
https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/

https://github.com/gorhill/uBlock/

Beware of false immitations:  
Tom's Hardware: Popular Ad Blockers AdBlock and uBlocker Deemed 'Fake'.  
https://www.tomshardware.com/news/adblock-ublock-fake-google-chrome-browser-extensions,40422.html

https://disconnect.me/  
https://addons.mozilla.org/en-us/firefox/addon/disconnect/  

[via](http://lifehacker.com/disconnect-2-speeds-up-the-web-protects-you-from-third-472942968)

### Site Analysis

See what libraries are in use with a site you are visiting:

wappalyzer
find out the technology stack used by any website

W3C CSS Validator is a simple

via
webdesignerdepot.com: 10 Tools I Can’t Design Without.
https://www.webdesignerdepot.com/2020/06/10-tools-i-cant-design-without/




### General Settings

For Firefox preferences:
Preferences-> General
Set as default browser

Change the home page: about:newtab

Search->Default Search Engine: DuckDuckGo (until results are not adequate)
Privacy -> Tell sites that I do not want to be tracked

Advanced -> Data Choices -> disable "Enable Firefox Health Report" and "Enable Crash Reporter"

Clear History


### Vue Dev Tools

https://addons.mozilla.org/en-US/firefox/addon/vue-js-devtools/

### Less Common Add-ons

*2019.02.03 09:21:03
ColorZilla
https://addons.mozilla.org/en-us/firefox/addon/colorzilla

This is good, but it's usually easier to use a similar function at the operating system level instead -- that way it's not limited to a specific window / application type

*2015.11.19 12:58:35 browsers todo systems javascript
#seems like it might be better to deny javascript by default. Give it a try and see how it works out:
https://addons.mozilla.org/en-US/firefox/addon/noscript

should consider adding noscript to firefox configuration:
https://noscript.net/
http://boingboing.net/2015/11/18/the-web-is-pretty-great-with-j.html

other noscript like options:
http://www.gnu.org/software/librejs/
http://www.gnu.org/philosophy/javascript-trap.html

*2016.05.03 09:23:04
https://addons.mozilla.org/en-US/firefox/addon/selenium-ide/

*2015.11.03 11:50:23
There are a few items that are great additions (Add-ons) (plugins):

custom new tab screen
https://addons.mozilla.org/en-us/firefox/addon/moment/

Restart browser, close extra tabs

Be sure to change Moment screen preferences:
chrome://moment/content/options.html

Farenheit
Y M D
Location: Bloomington, IN
Save

*2016.01.27 08:39:11 browsers todo
some of these add-ons look promising

Highlighter for text?
https://addons.mozilla.org/en-US/firefox/addon/wired-marker/?src=collection&collection_id=69b74e8b-ed2c-4274-aeff-fc051f2330c4
Wired-Marker :: Add-ons for Firefox

Flashgot media download add on:
https://addons.mozilla.org/en-US/firefox/addon/flashgot/?src=hp-dl-featured
May be similar to:
https://addons.mozilla.org/en-US/firefox/addon/video-downloadhelper
https://addons.mozilla.org/en-US/firefox/addon/downthemall

http://www.mozilla.org/en-US/firefox/new/

#measure pixels, add rulers, etc:
https://addons.mozilla.org/en-US/firefox/addon/measureit
#this didn't work for me

update to using nightly builds:
sudo add-apt-repository ppa:ubuntu-mozilla-daily/ppa
sudo apt-get update
sudo apt-get install firefox-trunk



## Chrome / Chromium:

For non-linux machines, go with Chrome:
https://www.google.com/intl/en/chrome/

Otherwise:
http://www.chromium.org

   sudo apt-get install chromium-browser

### Settings

Change "On startup" to: 

"Continue where you left off"

to restore previous session.

### Extensions


*2019.02.09 07:04:35
The Great Suspender!
Suspend Tabs after a certain period of inactivity
This is great!
Saves so much memory and resources!
Wish I could find something equivalent that works for Firefox...

https://chrome.google.com/webstore/detail/the-great-suspender/klbibkeccnjlkjkiokjodocebajanakg/related

https://github.com/deanoemcke/thegreatsuspender

*2011.09.05 09:46:51
copy
https://chrome.google.com/webstore/detail/djdmadneanknadilpjiknlnanaolmbfk?hc=search&hcp=main

need to configure the format to be equivalent to copy all urls

right click, options,
$url
$title

*2019.02.09 08:05:54
https://chrome.google.com/webstore/detail/vuejs-devtools/nhdogjmejiglipccpnnnanhbledajbpd

*2014.03.11 23:19:34 link_router
https://chrome.google.com/webstore/detail/link-router/mniabhjoaeegihknhoibknnfnidgnecp

yay!

*2013.10.15 10:11:17
custom new tab page:
https://chrome.google.com/webstore/detail/currently/ojhmphdkpgbibohbnpbfiefkgieacjmh

*2016.02.06 09:09:25
http://felixniklas.com/dimensions/
https://chrome.google.com/webstore/detail/dimensions/baocaagndhipibgklemoalmkljaimfdj
via:
https://www.elegantthemes.com/blog/resources/the-best-web-development-tools-you-probably-arent-using

*2014.11.21 08:05:48
Chrome Apps & Extensions Developer Tool
https://chrome.google.com/webstore/detail/chrome-apps-extensions-de/ohmmkhmmmpcnpikjeljgnaoabkaalbgc/related?hl=en-US

*2015.02.27 07:40:38
React Developer Tools
https://chrome.google.com/webstore/detail/react-developer-tools/fmkadmapgofadopljbjfkapdkoienihi


*2012.01.20 15:59:07
if using chromium, flash is not included by default.  Decide if you want flash:
http://get.adobe.com/flashplayer/

*2014.01.21 09:10:26
be sure no extensions fall into this category:
http://lifehacker.com/many-browser-extensions-have-become-adware-or-malware-1505117457
http://discuss.howtogeek.com/t/warning-your-browser-extensions-are-spying-on-you/12394
also [2014.09.25 13:20:50]
didn't see the few in use on the list, but good to check every now and again


#Not as critical:

*2013.07.07 05:33:22
http://www.one-tab.com/
(might be better than copy)

*2013.07.07 05:30:22 consider
Modern New Tab Page
Awesome New Tab Page is also pretty cool

https://www.dragdis.com/

*2012.08.27 02:33:19
Window Size 1.0
Shows window dimensions on resize
https://chrome.google.com/webstore/detail/window-size/gocemkoelbpknmanpfcabkbeppbbigio

*2011.08.11 12:14:52
google-talkplugin




