# Vue 3

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



