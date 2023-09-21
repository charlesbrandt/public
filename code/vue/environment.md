# Vue Environment

## Prerequisites 

### Interpretor 

https://vuejs.org/v2/guide/installation.html

Be sure you have [node installed](../javascript/node.md) and up-to-date. Verify with:

```
node -v
nvm install node
node -v
```

### Editor

[VS Code is helpful -- many good extensions here. ](/system/editors/vs-code/vs-code.md)

### Browser Dev Tools

https://github.com/vuejs/vue-devtools#vue-devtools

https://addons.mozilla.org/en-US/firefox/addon/vue-js-devtools/


### New projects 

`vue-cli` is another option.

many templates to explore and learn from
check awesome-vue


## Vite

Fast bundler for efficient development with Hot Module Reloading

[Vite](vite.md)


## Server Side Rendering

This is not necessarily the same thing as Static Site Generation, but it is usually a prerequisite

https://duckduckgo.com/?t=ffab&q=awesome+vue3&ia=web  
 awesome vue3 at DuckDuckGo  
https://github.com/vuesomedev/awesome-vue-3  
 vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  
https://vueschool.io/articles/vuejs-tutorials/nuxt-composition-api/  
 Nuxt Composition API - Vue.js Tutorials  
https://duckduckgo.com/?t=ffab&q=SSR+vue3&ia=web  
 SSR vue3 at DuckDuckGo  
https://v3.vuejs.org/guide/ssr.html#quasar-framework-ssr-pwa  
 Server-Side Rendering | Vue.js  
https://vitejs.dev/guide/ssr.html  
 Server-Side Rendering | Vite  
https://duckduckgo.com/?t=ffab&q=awesome+vite&ia=web  
awesome vite at DuckDuckGo  


## Static Assets

Files in `ui/public` will not be included by the build tool automatically (served separately)

Files in `ui/src/assets` will be imported using the build tool. 

Reference in templates with, e.g.

```
 <img
    src="@/assets/logo.png"
    width="36"
    height="36"
    alt="Logo"
 />
```

https://duckduckgo.com/?q=vue+src+assets+vs+public&t=ffab&ia=web  
vue src assets vs public at DuckDuckGo  
https://stackoverflow.com/questions/56008960/public-vs-src-directories-vue-cli  
vue.js - Public vs src directories (vue-cli) - Stack Overflow  
https://cli.vuejs.org/guide/html-and-static-assets.html#static-assets-handling  
HTML and Static Assets | Vue CLI  
https://duckduckgo.com/?t=ffab&q=Static+Assets+Handling+vue3&ia=web  
Static Assets Handling vue3 at DuckDuckGo  
https://v3.cli.vuejs.org/guide/html-and-static-assets.html#prefetch  
HTML and Static Assets | Vue CLI  
