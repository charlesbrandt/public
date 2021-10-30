# Vue 3

Items specific to vue 3
most of the concepts from [vue 2](index.md) still transfer. 

## Vite

[vite](vite.md)

## UI Frameworks / Component Libraries

[ui frameworks](ui-frameworks.md)

## General links

https://github.com/vuesomedev/awesome-vue-3  
vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  

https://vueschool.io/articles/vuejs-tutorials/suspense-new-feature-in-vue-3/  
💤 Suspense - new feature in Vue 3 - Vue.js Tutorials  
  
https://github.com/topics/vue3  
💤 vue3 · GitHub Topics  

## Migration

https://v3.vuejs.org/guide/migration/migration-build.html#upgrade-workflow

https://dev.to/alvarosaburido/how-to-migrate-your-library-from-vue2-to-vue3-1h81  

https://johnpapa.net/vue2-to-vue3/  
Migrating Vue 2 to Vue 3  

https://duckduckgo.com/?t=ffab&q=convert+vue2+to+vue3+components&ia=web  
convert vue2 to vue3 components at DuckDuckGo  

https://crisp.chat/blog/vuejs-migration/  
Vue.js: How to Migrate a large project from Vue 2 to Vue 3 | Crisp  
https://v3.vuejs.org/guide/migration/migration-build.html#expectations  
Migration Build | Vue.js  
https://duckduckgo.com/?t=ffab&q=vue+%24attrs+%24listeners&ia=web  
vue $attrs $listeners at DuckDuckGo  
https://v3.vuejs.org/guide/migration/listeners-removed.html#overview  
$listeners removed | Vue.js  
https://vuejs.org/v2/guide/list.html#Maintaining-State  
List Rendering — Vue.js  
https://duckduckgo.com/?t=ffab&q=vue+h+function&ia=web  
vue h function at DuckDuckGo  
https://v3.vuejs.org/guide/render-function.html#creating-component-vnodes  
Render Functions | Vue.js  
https://v3.vuejs.org/api/instance-properties.html#slots  
Instance Properties | Vue.js  


### Global libraries

registering at global app level now discouraged?

was looking at where to put axios
ended up importing in each component that needs it (pages)
is that desirable? help with build sites

https://duckduckgo.com/?q=vite+axios&t=ffab&ia=web  
vite axios at DuckDuckGo  
https://github.com/Sensanaty/vue3-vite-template  
GitHub - Sensanaty/vue3-vite-template  
https://developpaper.com/use-vite-to-quickly-build-vue3-elementplus-project/  
Use vite to quickly build vue3 + elementplus project | Develop Paper  
https://philipdevblog.hashnode.dev/real-world-vue-3-api-calls-with-axios  
Real World Vue 3: API Calls with Axios  
https://stackoverflow.com/questions/66769162/how-do-i-handle-the-keydown-esc-event-in-vue3  
vue.js - How do I handle the "keydown.esc" event in Vue3? - Stack Overflow  
https://duckduckgo.com/?q=vue3+plugins&t=ffab&ia=web  
vue3 plugins at DuckDuckGo  
https://v3.vuejs.org/guide/plugins.html#using-a-plugin  
Plugins | Vue.js  
https://stackoverflow.com/questions/50370939/import-axios-method-globally-in-vuejs  
vue.js - Import Axios Method Globally in Vuejs - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=vue3+vue.prototype&ia=web  
vue3 vue.prototype at DuckDuckGo  
https://stackoverflow.com/questions/65184107/how-to-use-vue-prototype-or-global-variable-in-vue-3  
javascript - How to use Vue.prototype or global variable in Vue 3? - Stack Overflow  
https://v3.vuejs.org/guide/migration/global-api.html#provide-inject  
Global API | Vue.js  
https://v3.vuejs.org/guide/migration/global-api-treeshaking.html#usage-in-plugins  
Global API Treeshaking | Vue.js  
https://duckduckgo.com/?t=ffab&q=vue3+provide+axios&ia=web  
vue3 provide axios at DuckDuckGo  
https://stackoverflow.com/questions/64269587/how-to-correctly-import-axios-in-vue-3-after-creating-new-project-with-cli  
javascript - How to correctly import Axios in vue 3 after creating new project with CLI? - Stack Overflow  

https://duckduckgo.com/?t=ffab&q=vueuse&ia=web  
vueuse at DuckDuckGo  
https://vueuse.org/integrations/useaxios/  
useAxios | VueUse  
https://vueuse.org/guide/config.html  
Configurations | VueUse  


## Starter Template

I've been very happy with:

https://github.com/web2033/vite-vue3-tailwind-starter  
💤 web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter) ⚡  

All the necessities, nothing too wild

Don't forget to disable automatic vscode launching if running under docker though

ui/vite.config.js

```
  server: {
    // open: true,
  },
  
```

## Composition API

Reading up on Vue3's new "Composition API"

https://v3.vuejs.org/guide/composition-api-introduction.html#standalone-computed-properties


`ref` is used for single variables. Values must be accessed via `item.value()`

`reactive` is used for javascript objects

https://softauthor.com/vue3-ref-vs-reactive/


If the composable only consists of Javascript, no need for a .vue file, just keep it in a .js file. (otherwise .vue files are an option too. Just pick the right one so syntax highlighting works)

https://michael-verschoof.medium.com/keep-state-easily-using-a-composable-in-vue-3-2e01b2c68d7f

Install with Nuxt version 2

https://composition-api.nuxtjs.org/getting-started/setup

Now, in `<script>` sections of components, use

```
export default {
  setup() {

  ...

  },
}
```

and explicitly make reactive objects by calling `reactive?()`

https://vueue.org/functions.html  
💤 Core Functions | VueUse  
  
  
https://vue-composition-api-rfc.netlify.com/api.html#setup  
API Reference | Vue Composition API  
  
Roughly based on React Hooks
  
https://reactjs.org/docs/hooks-intro.html  
Introducing Hooks – React  
  
https://www.google.com/search?client=ubuntu&channel=fs&q=vue3&ie=utf-8&oe=utf-8  
vue3 - Google Search  
https://madewithvuejs.com/blog/vue-3-roundup  
Vue 3 – A roundup of infos about the new version of Vue.js - Made with Vue.js  
https://vue-composition-api-rfc.netlify.com/#api-introduction  
Composition API RFC | Vue Composition API  

Lots more to explore on this topic



