# Vue 3

[Tools, setups, installs and environment configuration](environment.md)

https://github.com/vuesomedev/awesome-vue-3  
vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  

https://github.com/topics/vue3  
vue3 · GitHub Topics  

## Common Topics

Vue 3 builds on the ideas started in Vue 2

[Basic concepts common between vue 2 and vue 3](basics.md).

## New projects 

Many templates to explore and learn from

```
npm create vite@latest [project-name]
```

[Vite](vite.md)

Should see something like:

```
Need to install the following packages:
  create-vite@4.4.0
Ok to proceed? (y) 
✔ Project name: … project-name
✔ Select a framework: › Vue
✔ Select a variant: › TypeScript
```

Or, for a bit more customization:

```
✔ Project name: … project-name
✔ Select a framework: › Vue
✔ Select a variant: › Customize with create-vue ↗
Need to install the following packages:
  create-vue@3.7.1
Ok to proceed? (y) 

Vue.js - The Progressive JavaScript Framework

✔ Add TypeScript? … / Yes
✔ Add JSX Support? … No / 
✔ Add Vue Router for Single Page Application development? …  / Yes
✔ Add Pinia for state management? … / Yes
✔ Add Vitest for Unit Testing? …  / Yes
✔ Add an End-to-End Testing Solution? › Playwright
✔ Add ESLint for code quality? … / Yes
✔ Add Prettier for code formatting? … / Yes
```

Run with vite:

```
cd project-name
pnpm install
code .
pnpm dev
```

Should be running: `localhost:5173`

If using Playwright, enable browsers via VS Code (Run command: (Ctrl-Shift-P) Install Playwright Browsers) which triggers running:

```
npx playwright install --with-deps chromium firefox webkit
```

Choose the Testing tab in VS-Code. Make sure `Show browser` is enabled. Run the test. Hopefully a browser for testing loads and shows the page. Edit. Rinse. Repeat. 

### Components

Install the pieces and parts needed for your application

[Tailwind](tailwind.md)

[Supabase](/code/api/supabase.md#client)

[Components](components.md)




## Dark Mode

Initialize your project

```
npm create vite@latest new-project-name
```

Install dependencies, including VueUse

```
npm i @vueuse/core
```

Then work with the setting in your application:

``` js
<script setup>
import { useDark } from "@vueuse/core";
const isDark = useDark();
</script>

<template>
  <p>Dark theme: {{isDark}}</p>
</template>
```

via: 
https://www.vuemastery.com/blog/implementing-dark-mode-with-vueuse/  
Implementing Dark Mode with VueUse | Vue Mastery  

https://stackoverflow.com/questions/76579547/how-to-implement-dark-light-mode-in-vue3  
css - How to implement dark/light mode in vue3? - Stack Overflow  
  

https://duckduckgo.com/?t=ffab&q=vue3+automatic+dark+mode&atb=v343-1&ia=web vue3 automatic dark mode at DuckDuckGo  



### Starter Template

```js
<script setup>
// variable
const msg = 'Hello!'

// functions
function log() {
  console.log(msg)
}
</script>

<template>
  <button @click="log">{{ msg }}</button>
</template>
```

## Composition API

Vue3 introduces a new "Composition API" alternative to the original "Options API" in Vue2

https://v3.vuejs.org/guide/composition-api-introduction.html#standalone-computed-properties

## Ref vs Reactive

`ref` is used for single variables. Values must be accessed via `item.value()`


`reactive` is used for javascript objects.

In practice I'm not seeing reactive used as frequently as `ref()`

https://softauthor.com/vue3-ref-vs-reactive/


Now, in `<script>` sections of components, use

```
export default {
  setup() {

  ...

  },
}
```

and explicitly make reactive objects by calling `reactive()`

https://vueue.org/functions.html  
Core Functions | VueUse  
  
  
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




### Script Setup

In addition to the composition API, Vue 3 offers a new, simplified way for defining the script section of a component:

https://learnvue.co/2021/05/explaining-the-new-script-setup-type-in-vue-3-major-takeaways-from-the-rfc/  
Explaining The New script setup Type in Vue 3 - RFC Takeaways - LearnVue  

https://learnvue.co/2020/08/an-introduction-to-vue3-props-a-beginners-guide/  
An Introduction to Vue3 Props - A Beginner’s Guide - LearnVue  

https://v3.vuejs.org/guide/composition-api-setup.html#arguments  
Setup | Vue.js  

https://dev.to/alvarosaburido/using-script-setup-for-vue-3-sfcs-ba9  
Using script Setup for Vue 3 SFCs - DEV Community  

https://v3.vuejs.org/api/sfc-script-setup.html  

https://duckduckgo.com/?q=vue+3+script+setup+&t=ffab&ia=web  
vue 3 script setup at DuckDuckGo  



## Computed and Watcher

When using computed values, be sure the value is instantiated / applied in a template somewhere, otherwise it won't ever be called. 

https://v3.vuejs.org/guide/reactivity-computed-watchers.html#computed-debugging  
Computed and Watch | Vue.js  

Vue3 also has `watchEffect` which does not need to return a value (as a computed does). 

https://duckduckgo.com/?t=ffab&q=vue3+props+type+default&ia=web  
vue3 props type default at DuckDuckGo  
https://v3.vuejs.org/guide/component-props.html#prop-types  
Props | Vue.js  
https://duckduckgo.com/?t=ffab&q=vue3+computed&ia=web  
vue3 computed at DuckDuckGo  


## Vue Router

It's good to learn how vue-router works. 

It can also help when troubleshooting to be able to manually exclude problematic routes. 

### Automatically Generate Routes for Pages

Some cases, it may help to automatically generate the routes file based on the contents of the ui/pages directory. 


## Template References

https://duckduckgo.com/?t=ffab&q=vue3+reference+element+in+template+from+script&ia=web  
vue3 reference element in template from script at DuckDuckGo  
https://v3.vuejs.org/guide/composition-api-template-refs.html#usage-with-jsx  
Template Refs | Vue.js  


## Lifecycle Hooks

https://learnvue.co/2020/12/how-to-use-lifecycle-hooks-in-vue3/  
A Complete Guide to Vue Lifecycle Hooks - with Vue 3 Updates - LearnVue  

```
<script>
  import { onMounted } from 'vue'

  export default {
     setup () {
       onMounted(() => {
         console.log('mounted in the composition api!')
       })
     }
  }
</script>
```

It is possible to define lifecycle hooks when using `<script setup>` syntax too. 



## State management

With the Composition API, if the composable only consists of Javascript, no need for a .vue file, just keep it in a .js file. (otherwise .vue files are an option too. Just pick the right one so syntax highlighting works)

https://michael-verschoof.medium.com/keep-state-easily-using-a-composable-in-vue-3-2e01b2c68d7f


### Modules vs Global libraries

Registering libraries at the global app level is discouraged.

With the Composition API, if the module is imported directly where it is used, it will also be imported when the module using the submodule is imported. In other words, try to abstract a call out if you find you are importing it multiple times in to either a related service or a store call. 

was looking at where to put axios
ended up importing in each component that needs it (pages)

Alternatively, build a services object to handle importing and abstracting calls to the API so you don't need to recreate these in every component.

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



## Suspense

https://vueschool.io/articles/vuejs-tutorials/suspense-new-feature-in-vue-3/  
Suspense - new feature in Vue 3 - Vue.js Tutorials  
  

## Markdown

[Markdown](markdown.md)

## Tailwind

[Tailwind](tailwind.md)

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


