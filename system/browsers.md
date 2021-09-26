# Browsers

The window to the internet.  
Settings for an optimal session. 


## Firefox

Running multiple browsers with different profiles helps keeps active references in the right place. When the work is finished, just close that browser. Good for keeping memory free. 

If you need a browser context for a specific task / job

`firefox -p` does the trick! 

If no relevant profile is available, just create one.

Be sure to set Menu -> Settings -> Restore previous session.  

Ensure "Ctrl+Tab cycles through tabs in recently used order" option is unchecked.

Pick your preferred search engine while you're at it!


### Tab Suspender

'Auto Tab Discard' does what I'm after. Closest working alternative to 'The Great Suspender'

https://addons.mozilla.org/en-US/firefox/addon/auto-tab-discard/  

This feature goes by many names, including: 

Tab Sleep, Tab Snooze, Tab Freeze, Tab Suspend, Tab Discard or Tab Hibernation.

/search?q=firefox+tab+suspender

https://www.askvg.com/tip-disable-or-enable-tab-sleep-or-tab-snooze-feature-in-web-browsers/

May be a similar feature built in to Firefox

in about:config
browser.tabs.unloadOnLowMemory

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


### Tab Links

When creating documentation, it is useful to be able to copy all open tabs in a window. 


https://addons.mozilla.org/en-US/firefox/addon/copy-all-tabs/  
Copy All Tabs :: Add-ons for Firefox

have used this in the past as well:  
https://addons.mozilla.org/en-US/firefox/addon/copy-urls-expert/?src=search  
and this may have similar functionality  
https://addons.mozilla.org/en-US/firefox/addon/tab-mix-plus  

OneTab?


### New Tab

This is optional. 

I love pocket. 

Tabliss is a clean minimal alternative. 

https://addons.mozilla.org/en-US/firefox/addon/tabliss/

https://tabliss.io/

https://github.com/joelshepherd/tabliss

An open source, pluggable New Tab web extension written in TypeScript, React and Redux.

No special permissions required!


### Copy Selection as Markdown

https://addons.mozilla.org/en-US/firefox/addon/copy-selection-as-markdown/
Copy Selection as Markdown – Get this Extension for 🦊 Firefox (en-US)

https://duckduckgo.com/?t=ffab&q=firefox+extension+copy+html+as+markdown&ia=web
firefox extension copy html as markdown at DuckDuckGo


### Profile Management

If you make frequent use of profiles, it can help to create a default profile.

Locate where your profiles are stored:
https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data

Available on `about:support` using the "Open Directory" button in the "Profile Directory" section. 

```
~/.mozilla/firefox
```

Read up on how to backup and restore profiles:

https://support.mozilla.org/en-US/kb/back-and-restore-information-firefox-profiles

Recommendation is to use built in Firefox Profile manager to create a new profile first, then copy the contents of your default profile in to the newly created directory. That way all of the configurations in the browser are set up correctly. 

If you need to update the order, can try manually editing:
~/.mozilla/firefox/profiles.ini

(make a backup first!)

A template Firefox profile that tracks these notes is available here:

https://gitlab.com/charlesbrandt/firefox-profile

```
mkdir repos
cd repos/
git clone git@gitlab.com:charlesbrandt/firefox-profile.git
cd .mozilla/firefox/
# Create the new profile via Firefox
rm -r hn95wtr8.example/*
cp -r ~/repos/firefox-profile/* hn95wtr8.example/
```

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

#### The Great Suspender!

Suspend Tabs after a certain period of inactivity
This is great!
Saves so much memory and resources!
Wish I could find something equivalent that works for Firefox...

https://chrome.google.com/webstore/detail/the-great-suspender/klbibkeccnjlkjkiokjodocebajanakg/related

https://github.com/deanoemcke/thegreatsuspender

#### Copy Tabs

https://chrome.google.com/webstore/detail/djdmadneanknadilpjiknlnanaolmbfk?hc=search&hcp=main

need to configure the format to be equivalent to copy all urls

right click, options,
$url
$title


