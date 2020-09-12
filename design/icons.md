# Icons

## Meta Icon

TODO

include with font sorter & svg editor

ui only (don't even need a web layer)

https://www.google.com/search?channel=fs&client=ubuntu&q=comparison+of+tags+used+for+icon+fonts

Didn't see anything that matches this use case. 

a comparison of words used across different icon libraries
then standardize based on the library being applied
maybe this already exists
but go ahead and try implementing it
it will be a good learning experience. 

look at configuration for selected icon library. 
convert accordingly in the icon component

wrap it with the bulma CSS markup that is preferred. 

ideally use the <i> tag in the generated markup

<micon></micon>
or <metaicon>
or <mi> (short, but difficult to locate programmatically? (maybe <mi> is unique enough?) test that out... grep and see. 

rank and compare the different libraries for:
 - size
 - size of icons used
 
automatically parse / evaluate code for what is being used. 
then generate a dedicated icon pack on build. 
(maybe webpack offers this already... but make it dedicated for now)
parse codebase. 

also [2020.08.16 00:41:03] 
this ties closely in with tags
and il8n 
different languages == different tag sets

different icon libraries are different representations of the same tags. 
sortable list
pick preferred:
  - word 
  - icon / svg

also [2020.08.16 01:00:21] 
maybe this is just a generalized search application for icon fonts

toggle between collections
see equivalency
add appropriate alt-tags

make it super [accessible](accessibility.md)
todo: where are accessibility notes?
summarize
import them with notes about fonts, icons, front-end frameworks


## Wrappers

### <i>

https://stackoverflow.com/questions/11135261/should-i-use-i-tag-for-icons-instead-of-span
html - Should I use <i> tag for icons instead of <span>? - Stack Overflow

https://www.google.com/search?q=html5+i+tag
html5 i tag - Google Search

### Bulma

Bulma doesn't provide icons -- it's a CSS framework. But it does work with the popular icon fonts. 

https://bulma.io/documentation/elements/icon/
Icon | Bulma: Free, open source, and modern CSS framework based on Flexbox

### Vuetify

https://duckduckgo.com/?q=vuetify+icons+list&t=canonical&ia=web
vuetify icons list at DuckDuckGo
https://vuetifyjs.com/en/components/icons
Icon component — Vuetify.js
https://vuetifyjs.com/en/customization/icons
Icons — Vuetify.js

Material Design Icons are configured by default in Vuetify.

Shouldn't need to do anything else to use them. 

when do they require the 'mdi-' prefix? (don't see it in the demo)

    <v-icon>mdi-folder-open</v-icon>


## Libraries / Fonts / Collections

For now, I find it easiest to include the font in my header CSS, and then move on. 
Sure, it's ideal to compile and only bundle those fonts that are in use. 
This is not necessary early on. 

### Ionicons

https://ionicons.com/
Ionicons: The premium icon pack for Ionic Framework
https://ionicons.com/usage/
Ionicons: The premium icon pack for Ionic Framework

### Unicons

https://www.google.com/search?channel=fs&client=ubuntu&q=unicons
unicons - Google Search
https://github.com/Iconscout/unicons
GitHub - Iconscout/unicons: 1000+ Pixel-perfect vector icons and Iconfont for your next project.
https://github.com/Iconscout
Iconscout · GitHub
https://iconscout.com/unicons
3300+ Free vector icons - Unicons by Iconscout

https://github.com/antonreshetov/vue-unicons

https://github.com/antonreshetov/vue-unicons
antonreshetov/vue-unicons: 1000+ Pixel-perfect svg icons for your next project as Vue components
https://github.com/antonreshetov/vue-unicons/blob/master/src/components/Unicon.vue
vue-unicons/Unicon.vue at master · antonreshetov/vue-unicons
https://antonreshetov.github.io/vue-unicons/
Vue Unicons

### Material Design Icons

https://materialdesignicons.com/
Material Design Icons

### Emojis

https://www.google.com/search?channel=fs&client=ubuntu&q=open+source+emoji+outlines
open source emoji outlines - Google Search
https://openmoji.org/
OpenMoji

### Font Awesome

not sure why these have fallen out of favor
is it the prefixes needed? ('fa', 'fas', etc)

### Collection Searches

https://learnvue.co/2019/12/8-free-vue-icon-libraries-to-pretty-up-your-web-app/
8 Free Vue Icon Libraries to Pretty Up Your Web App – LearnVue
https://github.com/robcresswell/vue-material-design-icons
robcresswell/vue-material-design-icons: Material Design Icons as Vue Single File Components
https://www.google.com/search?&q=vue+icon+library&ie=utf-8&oe=utf-8
vue icon library - Google Search


## Inclusion / Compilation / Installation

Different approaches. Stick with what works for you

comparison of tags used for icon fonts - Google Search
https://www.lambdatest.com/blog/its-2019-lets-end-the-debate-on-icon-fonts-vs-svg-icons/
It's 2019! Let's End The Debate On Icon Fonts vs SVG Icons | LambdaTest
https://www.google.com/search?channel=fs&client=ubuntu&q=svg+icon+library
svg icon library - Google Search
https://www.google.com/search?channel=fs&client=ubuntu&q=svg+icon+system
svg icon system - Google Search
https://css-tricks.com/svg-sprites-use-better-icon-fonts/
Icon System with SVG Sprites | CSS-Tricks
https://css-tricks.com/html-for-icon-font-usage/
HTML for Icon Font Usage | CSS-Tricks
https://css-tricks.com/flat-icons-icon-fonts/
The Big List of Flat Icons & Icon Fonts | CSS-Tricks
https://css-tricks.com/pretty-good-svg-icon-system/
A Pretty Good SVG Icon System | CSS-Tricks






