# Vue 3

Items specific to vue 3
For the foundation basic concepts common between vue 2 and vue 3, see [basics](basics.md).

[For tools, setups, and installs](environment.md)

## Vite

[vite](vite.md)

## UI Frameworks / Component Libraries

[ui frameworks](ui-frameworks.md)

## General links

https://github.com/vuesomedev/awesome-vue-3  
vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  

https://vueschool.io/articles/vuejs-tutorials/suspense-new-feature-in-vue-3/  
Suspense - new feature in Vue 3 - Vue.js Tutorials  
  
https://github.com/topics/vue3  
vue3 · GitHub Topics  

## Composition API

Vue3 introduces a new "Composition API" alternative to the original "Options API" in Vue2

https://v3.vuejs.org/guide/composition-api-introduction.html#standalone-computed-properties


`ref` is used for single variables. Values must be accessed via `item.value()`

`reactive` is used for javascript objects

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

### Lifecycle Hooks

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

### Script Setup

In addition to the composition API, Vue 3 offers a new, simplified way for defining the script section of a component:

https://duckduckgo.com/?q=vue+3+script+setup+&t=ffab&ia=web  
vue 3 script setup at DuckDuckGo  
https://learnvue.co/2021/05/explaining-the-new-script-setup-type-in-vue-3-major-takeaways-from-the-rfc/  
Explaining The New script setup Type in Vue 3 - RFC Takeaways - LearnVue  
https://learnvue.co/2020/08/an-introduction-to-vue3-props-a-beginners-guide/  
An Introduction to Vue3 Props - A Beginner’s Guide - LearnVue  
https://v3.vuejs.org/guide/composition-api-setup.html#arguments  
Setup | Vue.js  

https://dev.to/alvarosaburido/using-script-setup-for-vue-3-sfcs-ba9  
Using script Setup for Vue 3 SFCs - DEV Community  

It is possible to define lifecycle hooks in this context too. 

https://v3.vuejs.org/api/sfc-script-setup.html  

## Form Components

Now we can create custom input elements that accept a `v-model`.

https://v3.vuejs.org/guide/component-basics.html#emitting-a-value-with-an-event

For example:

```
<script setup>

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false,
  },
  label: String,
})

const emit = defineEmits(['update:modelValue'])

const value = computed({
  get() {
    return props.modelValue
  },
  set(val) {
    emit('update:modelValue', val)
  },
})

</script>

<template>
  <span class="flex">
    <label>{{ label }}</label>
    <input type="checkbox" v-model="value" />
  </span>
</template>
```

## Computed and Watcher

Be sure to make use of a computed value in a template somewhere, otherwise it won't be called. 

https://v3.vuejs.org/guide/reactivity-computed-watchers.html#computed-debugging  
Computed and Watch | Vue.js  

Vue3 also has `watchEffect` which does not need to return a value (as a computed does). 

https://duckduckgo.com/?t=ffab&q=vue3+props+type+default&ia=web  
💤 vue3 props type default at DuckDuckGo  
https://v3.vuejs.org/guide/component-props.html#prop-types  
💤 Props | Vue.js  
https://duckduckgo.com/?t=ffab&q=vue3+computed&ia=web  
💤 vue3 computed at DuckDuckGo  


## Template References

https://duckduckgo.com/?t=ffab&q=vue3+reference+element+in+template+from+script&ia=web  
💤 vue3 reference element in template from script at DuckDuckGo  
https://v3.vuejs.org/guide/composition-api-template-refs.html#usage-with-jsx  
💤 Template Refs | Vue.js  


## Starter Template

I've been very happy with:

https://github.com/web2033/vite-vue3-tailwind-starter  
web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter) ⚡  

All the necessities, nothing too wild

Don't forget to disable automatic vscode launching if running under docker though

ui/vite.config.js

```
  server: {
    // open: true,
  },
  
```

## Vue Router

It's good to learn how vue-router works. 

It can also help when troubleshooting to be able to manually exclude problematic routes. 

### Automatically Generate Routes for Pages

Some cases, it may help to automatically generate the routes file based on the contents of the ui/pages directory. 

## State management

With the Composition API, if the composable only consists of Javascript, no need for a .vue file, just keep it in a .js file. (otherwise .vue files are an option too. Just pick the right one so syntax highlighting works)

https://michael-verschoof.medium.com/keep-state-easily-using-a-composable-in-vue-3-2e01b2c68d7f


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

Registering libraries at the global app level is discouraged.

With the Composition API, if the module is imported directly where it is used, it will also be imported when the module using the submodule is imported. In other words, try to abstract a call out if you find you are importing it multiple times in to either a related service or a store call. 

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



