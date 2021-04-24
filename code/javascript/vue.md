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

`vue-cli` is another option.

## Style Guide / Naming Conventions

> Filenames of single-file components should either be always PascalCase or always kebab-case.

[via](https://vuejs.org/v2/style-guide/#Single-file-component-filename-casing-strongly-recommended)

In Javascript, camelCase is typically used for most variable and method names in code.

Components are typically named in PascalCase.

However, `kebab-case.vue` is the way to go for filenames for single file components.

It is tempting to use PascalCase for component filenames. Tried it out. Now leaning against that idea.

`index.vue` is required in nuxt under dynamic (e.g. `_id`) paths. Capital `Index.vue` will not work. Maybe this is a bug, but for now it seems safest to stick with kebab-cased filenames.

Also, less important, but it's difficult to navigate on the command line when cases are mixed. Have to remember to type upper-cased characters.

## Components

A concise way to combine the markup (<template>), logic (<script>) and styling (<style>) in a single .vue file.

https://vuejs.org/v2/guide/components.html#Dynamic-Components
Components Basics — Vue.js

For an example template, see
https://gitlab.com/charlesbrandt/web-ui-api-db/-/blob/main/ui/pages/blank.vue

### Templates

https://vuejs.org/v2/guide/syntax.html

Common patterns:

List rendering
https://vuejs.org/v2/guide/list.html#key

See all keys / values in a given object:

```
    <ul>
      <li v-for="key in Object.keys(item)" :key="key">
        {{ key }}: {{ item[key] }}
      </li>
    </ul>
```

#### For Loops

https://stackoverflow.com/questions/44617484/vue-js-loop-via-v-for-x-times-in-a-range
javascript - Vue Js - Loop via v-for X times (in a range) - Stack Overflow

    <li v-for="n in 10" :key="n">{{ n }} </li>

#### Attributes

:disabled="foo"

<textfield label="Name" value.sync="el.name" :disabled="myVar">

Then in Vue you can just set this.myVar = true and it will disable the input.

https://stackoverflow.com/questions/39247411/how-to-add-dynamically-attribute-in-vuejs

### Scripts

The script block is where you put logic related to your component.

data is made up of properties
computed is made up of getters.

via:
https://stackoverflow.com/questions/58931647/nuxt-component-computed-vs-data

### Styles (CSS)

Styling will depend a lot on how you configure your application, which front-end CSS framework you choose, etc.

Reminder: Anything one-off or customizable should go in the corresponding web-compenent file. A CSS utility framework like Tailwind makes this even more streamlined!

### Dynamic Styles

Using variable in vue component to affect CSS styles

https://stackoverflow.com/questions/47322875/vue-js-dynamic-style-with-variables
css - Vue.js dynamic <style> with variables - Stack Overflow

https://stackoverflow.com/questions/42872002/in-vue-js-component-how-to-use-props-in-css/52280182#52280182
In vue.js component, how to use props in css? - Stack Overflow

```
<template>
  <div id="a" :style="style" @mouseover="mouseOver()">
  </div>
</template>

<script>
  export default {
    name: 'SquareButton',
    props: ['color'],
    computed: {
      style () {
        return 'background-color: ' + this.hovering ? this.color: 'red';
      }
    },
    data () {
      return {
        hovering: false
      }
    },
    methods: {
      mouseOver () {
       this.hovering = !this.hovering
      }
    }
  }
</script>

<style scoped>
<style>
```

```
      :style="{
          top: marginTop,
          left: marginLeft,
          width: maxSquare,
          height: maxSquare,

      }"
```

generating the whole style dictionary in a computed didn't work:
:style="margins"

See also ~/design_system/ui/pages/windows.vue

## Slots & Props

Use Props when you want to pass in a js object to a child component.

Use Slots when you want to pass in template markup to a child component.

https://medium.com/@nicomeyer/vue-js-slots-vs-props-af87078a8bd
Vue.js: slots vs. props - Nico Meyer - Medium

### Props

Example props. Linters may encourage the full form

```
  // https://vuejs.org/v2/guide/components-props.html#Type-Checks
  // String, Number, Boolean, Array, Object, Date, Function, Symbol
  props: {
    person: {
      type: Object,
      default: () => {},
    },
    list: {
      type: Array,
      default: () => [],
    },
    parameter: {
      type: String,
      default: 'Greetings',
    },
    something: Boolean,
  },
```

https://vuejs.org/v2/guide/components-props.html
Props — Vue.js

### Slots

https://vuejs.org/v2/guide/components-slots.html#Scoped-Slots
Slots — Vue.js
https://github.com/vuejs/rfcs/blob/master/active-rfcs/0001-new-slot-syntax.md
rfcs/0001-new-slot-syntax.md at master · vuejs/rfcs
https://www.google.com/search?client=ubuntu&channel=fs&q=vue+props+vs+slots&ie=utf-8&oe=utf-8
vue props vs slots - Google Search

## Routing

In your script section, use:

    router.push({name: "yourroutename")

router.push("yourroutename") is NOT the same as router.push({name: "yourroutename"). First one defines the route directly. The second one takes the route with the specified name.

via:
https://stackoverflow.com/questions/35664550/vue-js-redirection-to-another-page

see also: router-link and nuxt-link

## Custom Events / Event Bus

If you need to signal a parent component of something that has happened in a child component, use $emit.

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

For multiple levels of children components, there is vm.$listeners

https://vuejs.org/v2/api/#vm-listeners

https://stackoverflow.com/questions/42615445/vuejs-2-0-emit-event-from-grand-child-to-his-grand-parent-component

Note:  
If you've hit a situation where an event bus pattern comes up, it may be a good time to consider using [vuex](vuex.md).


## Renderless Components

https://dev.to/codinglukas/vue-js-pattern-for-async-requests-using-renderless-components-3gd

## Desktop application

For creating a desktop application with Vue, see `vue-nodegui`

## Forms

## Environment Variables (dotenv)

see [Nuxt](nuxt.md)
