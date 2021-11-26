# Vue

Looking forward to getting [vue3](vue3.md) in the mix. 

## Install

https://vuejs.org/v2/guide/installation.html

Be sure you have [node installed](../javascript/node.md) and up-to-date. Verify with:

```
node -v
nvm install node
node -v
```

### Browser Dev Tools

https://github.com/vuejs/vue-devtools#vue-devtools

https://addons.mozilla.org/en-US/firefox/addon/vue-js-devtools/

### New projects 

`vue-cli` is another option.

many templates to explore and learn from
check awesome-vue


## Single File Components

A concise way to combine the markup (`<template>`), logic (`<script>`) and styling (`<style>`) in a single .vue file.

Often makes use of the "Options API" (vs. the "Composition API" in vue3), but not exclusively. 

Web components done right!

https://v3.vuejs.org/guide/single-file-component.html

https://v3.vuejs.org/guide/component-basics.html

https://vuejs.org/v2/guide/components.html#Dynamic-Components
Components Basics — Vue.js

```
<script>
export default {
  data() {
    return {
      greeting: 'Hello World!'
    }
  }
}
</script>

<template>
  <p class="greeting">{{ greeting }}</p>
</template>
```

### Templates

https://vuejs.org/v2/guide/syntax.html

#### Conditional Rendering

https://vuejs.org/v2/guide/conditional.html

```
<div v-if="type === 'A'">
  A
</div>
<div v-else-if="type === 'B'">
  B
</div>
<div v-else-if="type === 'C'">
  C
</div>
<div v-else>
  Not A/B/C
</div>
```

#### For Loops

List rendering  
https://vuejs.org/v2/guide/list.html#key  

https://stackoverflow.com/questions/44617484/vue-js-loop-via-v-for-x-times-in-a-range  
javascript - Vue Js - Loop via v-for X times (in a range) - Stack Overflow  

```
    <li v-for="n in 10" :key="n">{{ n }} </li>
```



See all keys / values in a given object:

```
    <ul>
      <li v-for="key in Object.keys(item)" :key="key">
        {{ key }}: {{ item[key] }}
      </li>
    </ul>
```

### Scripts

The script block is where you put logic related to your component.

data is made up of properties  
computed is made up of getters.  

via:  
https://stackoverflow.com/questions/58931647/nuxt-component-computed-vs-data

#### Computed 

A computed method will respond to changes made to data properties of the component. Similar to watch. Like a method that is called automatically. 


### Binding values

How to share data between the `<template>` and `<script>`?

Typically, just use a v-model to handle coordinating values.

```
<input v-model="search">
```

Some cases where it's easier to separate the value from what action you want to take when events occur

```
      <textarea
        class="resize min-h-screen w-3/4 m-auto"
        :value="articleString"
        @input="updateObject"
      />
```


```
  methods: {
    updateObject(event) {
      console.log('updateObject', event.target.value)
    }
  }
```

https://dilshankelsen.com/v-model-with-vuex/  
How To Use V-Model With Vuex | Dilshan Kelsen  



### Styles (CSS)

Styling will depend a lot on how you configure your application, which front-end CSS framework you choose, etc.

Reminder: Anything one-off or customizable should go in the corresponding web-compenent file. A CSS utility framework like Tailwind makes this even more streamlined!

#### Attributes

Trigger if an attribute is added to a DOM element using a boolean value in the script. For example

```
:disabled="foo"
```

```
<textfield label="Name" value.sync="el.name" :disabled="myVar">
```

Then in Vue you can just set this.myVar = true and it will disable the input.

https://stackoverflow.com/questions/39247411/how-to-add-dynamically-attribute-in-vuejs

It's possible to use a similar approach to add a class conditionally:

```
:class="{ 'light-background': index % 2 }"
```

#### Dynamic Styles

Using variable in vue component to affect CSS styles

https://stackoverflow.com/questions/47322875/vue-js-dynamic-style-with-variables  
css - Vue.js dynamic style with variables - Stack Overflow  

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
If you've hit a situation where an event bus pattern comes up, it may be a good time to consider [State Management](#state-management) patterns.

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

#### Watching Props

Sometimes props arrive to the child component after the component's .mounted() call happens. In these cases, it's necessary to `watch` for the arrival of the prop from the parent:

```
  watch: {
    // Sometimes you want to act on the values that changed
    // propertyName: function (newVal, oldVal) {
    propertyName: function () {
      // console.log('property value changed', newVal)
      this.methodToHandleUpdate()
    },
  },

```

[via](https://stackoverflow.com/questions/44584292/how-to-listen-for-props-changes)


### Slots

Pass in customized markup content from the parent template. 

Named slots are handy for layout components:

```
<div class="container">
  <header>
    <slot name="header"></slot>
  </header>
  <main>
    <slot></slot>
  </main>
  <footer>
    <slot name="footer"></slot>
  </footer>
</div>

```

via:  
https://v3.vuejs.org/guide/component-slots.html#named-slots  

https://vuejs.org/v2/guide/components-slots.html#Scoped-Slots  
Slots — Vue.js  
https://github.com/vuejs/rfcs/blob/master/active-rfcs/0001-new-slot-syntax.md  
rfcs/0001-new-slot-syntax.md at master · vuejs/rfcs  
https://www.google.com/search?client=ubuntu&channel=fs&q=vue+props+vs+slots&ie=utf-8&oe=utf-8  
vue props vs slots - Google Search  

## Layouts

Slots get us most of the way there for a layout pattern. 

https://markus.oberlehner.net/blog/dynamic-vue-layout-components/

Some vue-based framework pre-configure a layout pattern for us, however it can help to understand the pattern at work in cases where those frameworks are not a good fit. 

https://awesome-vue.js.org/components-and-libraries/ui-layout.html

## Forms

Heavy overlap with Web Components & UI Frameworks.

It helps to study the underlying mechanisms of syncing form content with the local script and parent components. 

TODO: v-model binding in vue2? or is that a new feature with vue3?

https://duckduckgo.com/?t=ffab&q=creating+form+components+vue3&ia=web  
💤 creating form components vue3 at DuckDuckGo  
https://javascript.plainenglish.io/how-to-build-flexible-form-factory-by-vue-3-form-builder-pattern-b88edaf94776  
Building a dynamic and scalable Form Factory by Vue 3 | JavaScript in Plain English  


## Routing

[Router](router.md)

In your script section, use:

    router.push({name: "yourroutename")

router.push("yourroutename") is NOT the same as router.push({name: "yourroutename"). First one defines the route directly. The second one takes the route with the specified name.

via:
https://stackoverflow.com/questions/35664550/vue-js-redirection-to-another-page

see also: router-link and nuxt-link

## Style Guide / Naming Conventions

[General conventions](../conventions.md)

Components should use more than one word in the name. 

> Filenames of single-file components should either be always PascalCase or always kebab-case.

[via](https://vuejs.org/v2/style-guide/#Single-file-component-filename-casing-strongly-recommended)  
[same in vue3](https://v3.vuejs.org/style-guide/#single-file-component-filename-casing-strongly-recommended)  

In Javascript, camelCase is typically used for most variable and method names in code.

Components are typically named in PascalCase.

`kebab-case` is recommended when using components in template markup. I also prefer kebab-case for filenames too.

I find it's more difficult to navigate on the command line when cases are mixed. Have to remember to type upper-cased characters. There are some filesystems that don't support case-sensitivity, but that should be moot in most cases. 

This is a good discussion on the topic

https://github.com/vuejs/vuejs.org/issues/1162  
Naming in PascalCase or kebab-case · Issue #1162 · vuejs/vuejs.org  


## Environment Variables (dotenv)

## Automatic importing / Bundle Size / Tree-shaking

In an effort to minimize bundle size, it helps to only import the code being used. 

https://github.com/antfu/unplugin-vue-components#readme

`unplugin-vue-components` helps import components automatically, which in turn helps with tree-shaking.

This is the approach recommended by some ui-frameworks, like Element+:

https://element-plus.org/en-US/guide/quickstart.html#on-demand-import

https://rollupjs.org/guide/en/#tree-shaking


## State Management

### Vuex (Persistence)

In Vue, variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of something like Vuex

See: [Vuex](vuex.md)

See also Vuex-ORM

## Nuxt

Nuxt simplifies the configuration and structure of a Vue project to get up and running quickly.

See: [Nuxt](nuxt.md)


