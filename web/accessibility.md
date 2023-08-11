# Accessibility

Often abbreviated as "a11y". There are 11 characters between the 'a' and the 'y' in 'accessibility'. Plus, it looks like "ally", which is a good thing to be!

Touches on many other topics, including:

  - design
  - user interface 
  - components

## Summary

If it's possible to use a native html element to implement a feature on a page, do that. But that also means using it the way it was intended. For some elements, the line is a bit blurry.

## WCAG

https://www.w3.org/TR/WCAG20/

A resource called Web Content Accessibility Guidelines (WCAG) outlines strategies and resources for creating accessible sites. Authored and vetted by a cadre of international experts, WCAG includes best practices for mobile devices.

## Tools

https://github.com/GoogleChrome/web-vitals  
GitHub - GoogleChrome/web-vitals: Essential metrics for a healthy site.  

## Buttons vs Links

Based on my reading, to summarize, a button is meant to take an action in a form on a page. A link is meant to take you somewhere else. Either can be styled to look any number of ways. If the object performs an action, use a button.

https://duckduckgo.com/?t=canonical&q=accessibility+button+vs+link&ia=web
accessibility button vs link at DuckDuckGo
http://web-accessibility.carnegiemuseums.org/content/buttons/
Buttons & Links | Accessibility Guidelines
https://www.a11y-101.com/design/button-vs-link
Button versus Link | Introduction to Accessibility

Use buttons for buttons! (actions)

links take you to different pages

button
vs
.button

can mark up both as needed (make them different or similar)

e.g. sign up and log in can stay as link but look like buttons

Search button should be a button

## Alt tags for SVG

Alt tags are helpful for any image.

https://svgontheweb.com/#implementation

If using a standard `<img>` tag, just use `alt`

"Note that this method limits manipulation functionality."

### Embedded

It is is also possible to embed description tags directly in the svg file itself.

Be sure to only describe information contained in the image.

> In general, HTML5 tries to discourage authors from providing content that's hidden from sighted users, because (a) it often contains new information that would be of use to sighted users, (b) it's frequently poorly written because there's little feedback to the (normally) sighted author, and (c) it is not maintained as carefully and therefore can go stale quickly.

```
<svg role="img" aria-label="[title + description]">
  <title>[title]</title>
  <desc>[long description]</desc>
  ...
</svg>
```

https://stackoverflow.com/questions/4697100/accessibility-recommended-alt-text-convention-for-svg-and-mathml

https://duckduckgo.com/?q=svg+alt+text

# 2020.12.06 15:12:48 documentation tip preference

Use SVG for visual elements

The CSS 3 line bar hamburger menu is cute
But it's one more hoop to have to jump through to get a visual element rendered to the screen
browsers should support svg at this point.

## Firefox

https://support.mozilla.org/en-US/kb/accessibility-features-firefox-make-firefox-and-we

### Reader view

Reader view in Firefox

Not strictly an accessibility feature
but making information easier to read makes it more accessible in my opinion

Didn't find an easy way to enable it by default for all pages

supposedly open:

    about:config

and set to true:

    reader.parse-on-load.force-enabled

https://duckduckgo.com/?t=canonical&q=accessibility+features+web+browser&ia=web
accessibility features web browser at DuckDuckGo
https://duckduckgo.com/?t=canonical&q=firefox+toggle+reader+view&ia=web
firefox toggle reader view at DuckDuckGo
https://addons.mozilla.org/en-US/firefox/addon/activate-reader-view/
Activate Reader View – Get this Extension for 🦊 Firefox (en-US)
https://support.mozilla.org/en-US/kb/about-config-editor-firefox
Configuration Editor for Firefox | Firefox Help
https://support.mozilla.org/en-US/kb/firefox-reader-view-clutter-free-web-pages
Firefox Reader View for clutter-free web pages | Firefox Help
https://duckduckgo.com/?t=canonical&q=firefox+what+makes+a+page+have+a+reader+view&ia=web
firefox what makes a page have a reader view at DuckDuckGo
https://blog.mozilla.org/firefox/reader-view/
Reader View: See the web with fresh eyes | The Firefox Frontier
https://duckduckgo.com/?t=canonical&q=firefox+use+reader+view+to+enhance+accessibility&ia=web
firefox use reader view to enhance accessibility at DuckDuckGo
https://support.mozilla.org/en-US/kb/accessibility-features-firefox-make-firefox-and-we
Accessibility features in Firefox - Make Firefox and web content work for all users | Firefox Help
https://developer.mozilla.org/en-US/docs/Mozilla/Accessibility/Accessibility_Features_in_Firefox
Accessibility Features in Firefox - Mozilla | MDN
about:config
Advanced Preferences


# 2023.08.10 14:16:20 links

https://khan.github.io/tota11y/  
tota11y – an accessibility visualization toolkit  
https://github.com/Khan/tota11y  
GitHub - Khan/tota11y: an accessibility (a11y) visualization toolkit  
https://github.com/addyosmani/a11y  
GitHub - addyosmani/a11y: Accessibility audit tooling for the web (beta)  
https://github.com/topics/a11y  
a11y · GitHub Topics · GitHub  
https://github.com/topics/accessibility  
accessibility · GitHub Topics · GitHub  
https://github.com/ghosh/Micromodal  
GitHub - ghosh/Micromodal: ⭕ Tiny javascript library for creating accessible modal dialogs  
https://github.com/paypal/accessible-html5-video-player  
GitHub - paypal/accessible-html5-video-player: Accessible HTML5 Video Player  
https://github.com/leongersen/noUiSlider  
GitHub - leongersen/noUiSlider: noUiSlider is a lightweight, ARIA-accessible JavaScript range slider with multi-touch and keyboard support. It is fully GPU animated: no reflows, so it is fast; even on older devices. It also fits wonderfully in responsive designs and has no dependencies.  
https://refreshless.com/nouislider/  
noUiSlider - JavaScript Range Slider | Refreshless.com  
https://github.com/collections/web-accessibility  
Collection: Web accessibility · GitHub  
https://github.com/brunopulis/awesome-a11y  
GitHub - brunopulis/awesome-a11y: A curate list about A11Y  
https://github.com/brunopulis/awesome-a11y/blob/main/topics/tools.md  
awesome-a11y/tools.md at main · brunopulis/awesome-a11y · GitHub  
https://www.ssa.gov/accessibility/andi/help/howtouse.html  
ANDI - Accessibility Testing Tool - Tutorial  
https://wiki.gnome.org/Projects/Orca  
Projects/Orca - GNOME Wiki!  
https://wiki.gnome.org/Projects/Orca/Roadmap  
Projects/Orca/Roadmap - GNOME Wiki!  
https://help.gnome.org/users/orca/stable/  
Orca Screen Reader  
https://github.com/tailwindlabs/headlessui  
GitHub - tailwindlabs/headlessui: Completely unstyled, fully accessible UI components, designed to integrate beautifully with Tailwind CSS.  
https://headlessui.com/  
Headless UI - Unstyled, fully accessible UI components  
https://github.com/chakra-ui/zag  
GitHub - chakra-ui/zag: Finite state machines for building accessible design systems and UI components.  
https://github.com/chakra-ui/chakra-ui-vue  
GitHub - chakra-ui/chakra-ui-vue: ⚡️ Build scalable and accessible Vue.js applications with ease.  
https://github.com/vmware-archive/clarity  
GitHub - vmware-archive/clarity: Clarity is a scalable, accessible, customizable, open source design system built with web components. Works with any JavaScript framework, built for enterprises, and designed to be inclusive.  
https://github.com/vmware-clarity/core  
GitHub - vmware-clarity/core: Clarity is a scalable, accessible, customizable, open-source design system built with web components. Works with any JavaScript framework, created for enterprises, and designed to be inclusive.  

## 2018.07.05 14:30:48
https://uxdesign.cc/designing-for-accessibility-is-not-that-hard-c04cc4779d94

## 2018.06.06 15:20:56
https://www.uxmatters.com/mt/archives/2018/04/vital-accessibility-design-principles.php

## 2015.05.20 17:07:14
consider accessibility implications of html5 / javascript
https://www.youtube.com/watch?v=lMrkCoqgoxw

http://accessibility.oit.ncsu.edu/training/aria/modal-window/version-3/
