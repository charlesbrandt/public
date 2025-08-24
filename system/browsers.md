# Browsers

A window to the internet.  
Settings and extensions for an optimal session. 


## Firefox

Running multiple browsers with different profiles helps keeps active references in the right place. When the work is finished, just close that browser. This strategy helps keep memory free. 

If you need a browser context for a specific task / job, launch f
irefox with:

```
firefox -p
```

If no relevant profile is available, create one from scratch, or clone an existing template. A template Firefox profile that tracks these notes is available here:

https://gitlab.com/charlesbrandt/firefox-profile

[Profile Management](#profile-management)


### Settings

Open via the menu: Menu -> Settings  
or in a new window load: `about:preferences#general`

Set the following:

-> Open previous windows and tabs (AKA: Restore previous session)

-> Pick your preferred search engine

-> Update the homepage and new tabs preference.  
  - Lately I've been opting for "Blank Page" to minimize distractions.  
  - [Tabliss (see below)](#new-tab) is another nice option


Ensure "Ctrl+Tab cycles through tabs in recently used order" option is unchecked. (This has been unchecked by default for a while.)


### Tab Links

When creating documentation, it is useful to be able to copy all open tabs in a window. I wrote this extension to help: 

https://addons.mozilla.org/en-US/firefox/addon/copy-all-tabs/  
Copy All Tabs :: Add-ons for Firefox

This may have similar functionality?  
https://addons.mozilla.org/en-US/firefox/addon/tab-mix-plus-webextension/

OneTab?


### Copy Selection as Markdown

https://addons.mozilla.org/en-US/firefox/addon/copy-selection-as-markdown/  
Copy Selection as Markdown – Get this Extension for Firefox (en-US)  

https://github.com/0x6b/copy-selection-as-markdown

I prefer to change a few of the default settings:

Uncheck the following:

"Prepend quote (>) to the selection"
"Include link to source page in the copied text" (Be sure to still give attribution!)

Code block style

```
let code == "fenced";
```

https://duckduckgo.com/?t=ffab&q=firefox+extension+copy+html+as+markdown&ia=web  
firefox extension copy html as markdown at DuckDuckGo  


### Ad Blockers

Instead of only blocking ads, ublock-origin is a general filter / tracking blocker:  
https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/

https://github.com/gorhill/uBlock/

Beware of false imitations:  
Tom's Hardware: Popular Ad Blockers AdBlock and uBlocker Deemed 'Fake'.  
https://www.tomshardware.com/news/adblock-ublock-fake-google-chrome-browser-extensions,40422.html

Disconnect me is a similar privacy focused extension:

https://disconnect.me/  
https://addons.mozilla.org/en-us/firefox/addon/disconnect/  

[via](http://lifehacker.com/disconnect-2-speeds-up-the-web-protects-you-from-third-472942968)

### Tab Suspender

'Auto Tab Discard' will automatically put non-active tabs to sleep. It's a good way to keep a lot of tabs loaded in the browser without consuming a lot of active memory or resources. 

https://addons.mozilla.org/en-US/firefox/addon/auto-tab-discard/  

This feature goes by many names, including: 

Tab Sleep, Tab Snooze, Tab Freeze, Tab Suspend, Tab Discard or Tab Hibernation.

It's similar to Chrome's 'The Great Suspender'.

Preferences:

Remove: Prepend a symbol to the discarded tabs (e.g. 💤 or ⏻︎)

#### Firefox Unload

There is starting to be similar functionality built in to Firefox, however this still appears to be a manual process, and the tabs are prefixed with "firefox hibernate tabs" (too long)

Type `about:unloads` in the address bar and press Enter. 

https://firefox-source-docs.mozilla.org/browser/tabunloader/  
https://support.mozilla.org/en-US/kb/unload-inactive-tabs-save-system-memory-firefox  

Another old Firefox setting that came close, but does not provide unload on demand and automatically:

in `about:config`
```
browser.tabs.unloadOnLowMemory
```

https://www.askvg.com/tip-disable-or-enable-tab-sleep-or-tab-snooze-feature-in-web-browsers/

https://duckduckgo.com/?q=great+suspender+firefox&t=canonical&ia=web  
great suspender firefox at DuckDuckGo  
https://www.reddit.com/r/firefox/comments/374gx3/chromes_the_great_suspender_for_firefox/  
Chrome's 'The Great Suspender' for firefox? : firefox  
https://addons.mozilla.org/en-US/firefox/addon/unload-tabs/  
UnloadTabs – Get this Extension for Firefox (en-US)  

this one didn't work so well for me.. major memory usage  
https://addons.mozilla.org/en-US/firefox/addon/ff-tab-suspender/  
Tab Suspender – Get this Extension for Firefox (en-US)  


### Custom CSS 

Some sites, like Wikipedia, don't yet have a user configurable dark mode for their site. (as of 2024) 

https://addons.mozilla.org/en-US/firefox/addon/styl-us/
Stylus – Get this Extension for 🦊 Firefox (en-US)

It may be possible to use uBlock for this purpose, but I think that requires managing the CSS to be applied directly:

https://www.reddit.com/r/firefox/comments/8w9nqm/ublock_can_style_css_you_dont_need_a_separate/
uBlock can style CSS - you don't need a separate addon for that : r/firefox


;

### Profile Management

If you make frequent use of profiles, it can help to create a default profile.

Locate where your profiles are stored:  
https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-dat

Available on `about:support` using the "Open Directory" button in the "Profile Directory" section. 

Under snap at

```
~/snap/firefox/common/.mozilla/firefox
```

Previously

```
~/.mozilla/firefox
```

#### Creating Profiles

https://support.mozilla.org/en-US/kb/profile-manager-create-remove-switch-firefox-profiles

```
about:profiles
```

It's faster to create new profiles with the browser already loaded. 

I make profiles for different domains of research. For example:

```
development
shopping
home
music
work
design
```

Ideally each one starts with a unique letter/number to make it easier to update them with the default profile. 

#### Backup and Restore Profiles

https://support.mozilla.org/en-US/kb/back-and-restore-information-firefox-profiles

Recommendation is to use built in Firefox Profile manager to create a new profile first, then copy the contents of your default profile in to the newly created directory. That way all of the configurations in the browser settings are initialized correctly. 

To apply your template profile

```
cd path/to/profiles
# Create the new profile via Firefox
rm -r hn95wtr8.example/*
cp -r firefox-profile/* hn95wtr8.example/
```

There's a script that helps do this: `browsers.py`




If you need to update the order, try manually editing the settings (make a backup copy first!)

```
cd [profile-directory]
cp profiles.ini profiles.ini.backup.2
edit profiles.ini
```


### Snap

Newer versions of Ubuntu are shipping Firefox as a snap package. This leads to annoying "Close the app to avoid disruption messages". 

It looks like it is possible to run the update manually:

```
killall firefox;
sudo snap refresh;
```

https://askubuntu.com/questions/1412140/how-to-solve-pending-update-of-firefox-snap-close-the-app-to-avoid-disruptio  
How to solve "Pending update of "firefox" snap. Close the app to avoid disruptions" error? - Ask Ubuntu  


### Process Manager

Even with Autotab Discard, browser contexts can still eat a lot of resources. Process Manager can help identify which sites are misbehaving. Available via: 

Menu -> More Tools -> Task Manager

about:processes  
Process Manager  

https://duckduckgo.com/?t=ffab&q=firefox+show+tab+consuming+cpu&ia=web  
firefox show tab consuming cpu at DuckDuckGo  
https://superuser.com/questions/234923/determine-which-tab-in-firefox-is-using-cpu-resources  
cpu usage - Determine which tab in Firefox is using CPU resources? - Super User  


### New Tab

This is optional. 

Tabliss is a clean minimal alternative to the default new tab page. 

https://addons.mozilla.org/en-US/firefox/addon/tabliss/

https://tabliss.io/

https://github.com/joelshepherd/tabliss

An open source, pluggable New Tab web extension written in TypeScript, React and Redux.

No special permissions required!



### Themes

Colorway themes can be helpful for distinguishing one Firefox context from another, especially if they get spread across multiple workspaces. 

https://support.mozilla.org/en-US/kb/personalize-firefox-colorways

Menu -> Add-ons and Themes

Not configured in "Settings". 




## Chrome / Chromium:

For non-linux machines, go with Chrome:
https://www.google.com/intl/en/chrome/

Otherwise:
http://www.chromium.org

```
sudo apt-get install chromium-browser
```

### Settings

Change "On startup" to: 

"Continue where you left off"

to restore previous session.

### Extensions

#### The Great Suspender!

Suspend Tabs after a certain period of inactivity.  
This is great!  
Saves so much memory and resources!  

https://chrome.google.com/webstore/detail/the-great-suspender/klbibkeccnjlkjkiokjodocebajanakg/related

https://github.com/deanoemcke/thegreatsuspender

#### Copy Tabs

https://chrome.google.com/webstore/detail/djdmadneanknadilpjiknlnanaolmbfk?hc=search&hcp=main

Configure the format as desired. 

right click, options,

```
$url
$title
```

