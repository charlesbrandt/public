# Vue

Guides:
https://vuehero.com/


## Install

https://vuejs.org/v2/guide/installation.html

Be sure you have [node installed](node.md) and up-to-date. Verify with:

    node -v
    nvm install node
    node -v


### Browser Dev Tools

https://github.com/vuejs/vue-devtools#vue-devtools

https://addons.mozilla.org/en-US/firefox/addon/vue-js-devtools/


## New projects / Nuxt

Nuxt simplifies the configuration and structure of a Vue project for a community standard. 

See: [Nuxt](nuxt.md)


## Components

A concise way to combine the markup (<template>), logic (<script>) and styling (<style>) in a single .vue file. 

https://vuejs.org/v2/guide/components.html#Dynamic-Components
Components Basics — Vue.js

```
<template>
  <div class="component">
    <draggable :list="list">
      <div v-for="element in list" :key="element.name" class="dragArea">
        {{ element.name }}
      </div>
    </draggable>
    <slot />
  </div>
</template>

<script>
import draggable from 'vuedraggable'
// import axios from 'axios'

export default {
  components: {
    draggable,
  },
  // https://vuejs.org/v2/guide/components-props.html#Type-Checks
  // String, Number, Boolean, Array, Object, Date, Function, Symbol
  props: {
    person: {
      type: Object,
      default: () => {},
    },
    parameter: {
      type: String,
      default: 'Greetings',
    },
    something: Boolean,
  },
  // https://nuxtjs.org/guide/async-data/
  /* async asyncData({ params }) {
    const { data } = await axios.get(`http://localhost:8888/json-path/data`)
    return { items: data }
  },
  */
  data() {
    return {
      list: [{ name: 'One' }, { name: 'Two' }, { name: 'Three' }],
    }
  },
  computed: {},
}
</script>

<style>
.dragArea {
  position: relative;
  text-align: left;
  display: inline-block;
  margin: 0;
  width: 150px;
  height: 150px;
  vertical-align: top;
}
</style>
```

### Renderless Components

https://dev.to/codinglukas/vue-js-pattern-for-async-requests-using-renderless-components-3gd

### Templates

https://vuejs.org/v2/guide/syntax.html

Common patterns:

List rendering
https://vuejs.org/v2/guide/list.html#key

### Scripts

The script block is where you put logic related to your component. 

data is made up of properties
computed is made up of getters.

via:
https://stackoverflow.com/questions/58931647/nuxt-component-computed-vs-data

### Dynamic Styles

https://stackoverflow.com/questions/47322875/vue-js-dynamic-style-with-variables


## Environment Variables (dotenv)

see [Nuxt](nuxt.md)


## Slots & Props

Use Props when you want to pass in a js object to a child component.

Use Slots when you want to pass in template markup to a child component. 

https://vuejs.org/v2/guide/components-props.html
Props — Vue.js
https://vuejs.org/v2/guide/components-slots.html#Scoped-Slots
Slots — Vue.js
https://github.com/vuejs/rfcs/blob/master/active-rfcs/0001-new-slot-syntax.md
rfcs/0001-new-slot-syntax.md at master · vuejs/rfcs
https://www.google.com/search?client=ubuntu&channel=fs&q=vue+props+vs+slots&ie=utf-8&oe=utf-8
vue props vs slots - Google Search
https://medium.com/@nicomeyer/vue-js-slots-vs-props-af87078a8bd
Vue.js: slots vs. props - Nico Meyer - Medium



## Forms


## Routing

In your script section, use:

    router.push({name: "yourroutename")

router.push("yourroutename") is NOT the same as router.push({name: "yourroutename"). First one defines the route directly. The second one takes the route with the specified name. 

via:
https://stackoverflow.com/questions/35664550/vue-js-redirection-to-another-page


## Custom Events

If you need to signal a parent component of something that has happened in a child component, use $emit. 

Note:
If you've hit a situation where this pattern comes up, it may be a good time to consider using [vuex](vuex.md).

Child component triggers clicked event:

```
export default {
  methods: {
    onClickButton (event) {
      this.$emit('clicked', 'someValue')
    }
  }
}
```

Parent component receive clicked event:

```
<div>
  <child @clicked="onClickChild"></child>
</div>

```

Then, in the parent script block, handle the emitted event as needed:

```
export default {
  methods: {
    onClickChild (value) {
      console.log(value) // someValue
    }
  }
}
```

via:
https://forum.vuejs.org/t/passing-data-back-to-parent/1201

