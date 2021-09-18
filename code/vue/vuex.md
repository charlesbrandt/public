# Vuex

https://vuex.vuejs.org/

From that page:

"Vuex is a state management pattern + library for Vue.js applications. It serves as a centralized store for all the components in an application, with rules ensuring that the state can only be mutated in a predictable fashion."

Vuex provides a reactive global shared object space. 

... like a storage system for your front-end application. Where to keep data that is shared between components. 


## 2 way computed property

<input v-model="message">

// ...
computed: {
  message: {
    get () {
      return this.$store.state.obj.message
    },
    set (value) {
      this.$store.commit('obj/updateMessage', value)
    }
  }
}

https://vuex.vuejs.org/guide/forms.html#two-way-computed-property

So, does `this.$store.commit('updateMessage', value)` trigger an action or a mutation? 

the `.commit` indicates it is triggering the mutation directly. For form bindings that may be sufficient. 

An action would use `.dispatch()`

https://vuex.vuejs.org/guide/actions.html#dispatching-actions

But why bother with an action then? 

> An action can dispatch more than 1 mutation at a time. An action implements the business logic, it doesn't care about data changing (which is managed by mutation)

https://stackoverflow.com/questions/39299042/vuex-action-vs-mutations

The two concepts may seem redundant for simple use cases. When used together, they can keep code easier to maintain as code grows. 


### Module Template

in store/obj.js (replacing obj with the name of your module)

```
export const state = () => ({
  list: []
})

export const mutations = {
  add(state, text) {
    state.list.push({
      text,
      done: false
    })
  },
  remove(state, { todo }) {
    state.list.splice(state.list.indexOf(todo), 1)
  },
  updateMessage(state, message) {
    todo.done = !todo.done
  }
}
```

https://nuxtjs.org/guide/vuex-store/


## Modules

https://vuex.vuejs.org/guide/state.html

https://nuxtjs.org/examples/vuex-store/

When using Vuex in a Nuxt project, you can use modules to structure the different data objects

https://itnext.io/efficiently-understanding-and-using-nuxt-vuex-7905eb8858d6

https://vuex.vuejs.org/guide/modules.html

There are two ways to structure your vuex code: Modules and Single Store. Separate modules seems like the way to go. 

Each module will have three aspects: State, Mutations, and Actions

"It’s very important to keep your state as flat as possible. Deeply nested objects in a state lose reactivity."

is store/index.js required when using modules?


## Process

https://vuex.vuejs.org/guide/

Define a state value in the 'state' attribute of the Vuex.Store.

Define a mutation to create the desired change to the state.

Trigger the mutation with a call to `store.commit('name_of_mutation')`



## Mapstate

https://github.com/tc39/proposal-object-rest-spread
tc39/proposal-object-rest-spread: Rest/Spread Properties for ECMAScript

```
When a component needs to make use of multiple store state properties or getters, declaring all these computed properties can get repetitive and verbose. To deal with this we can make use of the mapState helper which generates computed getter functions for us, saving us some keystrokes
```

It looks like mapState will only map a getter for the vuex value. If you want to set up a getter and a setter as in the 2-way computed propertery example above, mapstate won't help. 

https://forum.vuejs.org/t/dont-understand-how-to-use-mapstate-from-the-docs/14454/26

## Getters 

If you want to do the equivalent of a computed value within a Vuex store, you want getters:

https://vuex.vuejs.org/guide/getters.html

To use another getter from within a getter:

https://stackoverflow.com/questions/46210109/how-do-i-call-a-getter-from-another-getter-in-vuex

Having trouble pulling these back into a component:

https://tenmilesquare.com/understanding-mapgetters-in-vuex/


## Import from JSON

If you have data stored in JSON files, you can load that in when Nuxt initializes. 


## ORM

ORM based data modeling with a powerful query API on the front-end.

https://github.com/vuex-orm/vuex-orm


### Installation

    npm install vue vuex @vuex-orm/core --save
    
It's not necessary to add the module at the nuxt.config.js level... the store is already initialized as part of Nuxt initialization. We'll `import` from there.

in `@/store/index.js`:

```
import VuexORM from '@vuex-orm/core'
import database from 'database'

export const plugins = [
  VuexORM.install(database)
]
```

Then in `@/store/database.js`:

```
import { Database } from '@vuex-orm/core'
import User from '@/models/User'
import Todo from '@/models/Todo'

const database = new Database()

database.register(User)
database.register(Todo)

export default database
```


It is necessary to store your models somewhere. They could go in `@/store/`, but I'm not sure if that may cause confusion with the module approach to Vuex. I'm leaning toward `@/models/`. 

I don't think a separate `@/database/` directory is necessary.

Inspired by:
https://github.com/vuex-orm/vuex-orm-examples-nuxt


### Models

Define some models
Apply data to the models

That's the basic idea. 

Be sure to pass in the data in the form specified by the documentation, even if that means reformatting the source of the data to fit. 

Also, remember that when it comes time to retrieve the data, many-to-many related object will not be added to the object unless you use the 'with' syntax to load nested relations. 

https://vuex-orm.org/guide/prologue/getting-started.html#define-models
Getting Started | Vuex ORM
https://vuex-orm.org/guide/model/defining-models.html#uid-type
Defining Models | Vuex ORM
https://vuex-orm.org/guide/data/retrieving.html#where-clauses
Retrieving | Vuex ORM
https://vuex-orm.org/guide/data/inserting-and-updating.html#inserts
Inserting & Updating | Vuex ORM
https://vuex-orm.org/guide/model/retrieving-models.html#fetch-from-database
Retrieve Models | Vuex ORM


Nice video tutorials:
https://www.vuetiful.life/advanced/vuex-orm/#introduction-to-the-api


https://vuex-orm.org/guide/data/retrieving.html#get-single-data


https://vuex-orm.org/guide/prologue/what-is-vuex-orm.html#how-vuex-orm-handles-data
What is Vuex ORM? | Vuex ORM
https://vuex-orm.org/guide/prologue/getting-started.html#retrieving-data
Getting Started | Vuex ORM
https://vuex-orm.org/guide/digging-deeper/server-side-rendering.html
Server Side Rendering | Vuex ORM
https://vuex-orm.org/guide/digging-deeper/vuex-module.html#mutating-state
Vuex Module | Vuex ORM
https://vuex-orm.org/guide/model/retrieving-models.html#import-statement
Retrieve Models | Vuex ORM
https://redux.js.org/recipes/structuring-reducers/normalizing-state-shape/
Normalizing State Shape | Redux


https://duckduckgo.com/?q=vuex+many+to+many+relationship&t=canonical&ia=web
vuex many to many relationship at DuckDuckGo
https://stackoverflow.com/questions/55528073/vuex-orm-many-to-many-relationship-with-pivot-table-with-additional-attributes
javascript - Vuex ORM Many-to-Many Relationship with pivot table with additional attributes - Stack Overflow


## Saving

To save data to LocalStorage between page reloads:

    vuex-persistedstate
    
https://github.com/robinvdvleuten/vuex-persistedstate


This does not make sense for SSR applications?

```
Nuxt.js

It is possible to use vuex-persistedstate with Nuxt.js. It must be included as a NuxtJS plugin:

// nuxt.config.js

...
plugins: [{ src: '~/plugins/localStorage.js', ssr: false }]

```

`vuex-persistedstate` is the library Adam used in the [open311-nodejs](https://github.com/City-of-Bloomington/open311-nodejs/blob/master/package.json) project.

    
There is also this library:

https://github.com/championswimmer/vuex-persist

via
https://alligator.io/vuejs/vuex-persist-state/

https://www.npmjs.com/package/nuxt-vuex-localstorage
    

## Vuex Alternatives

For smaller applications, a simple state management approach may be sufficient
https://vuejs.org/v2/guide/state-management.html#Simple-State-Management-from-Scratch

It's also a good example of the type of problem Vuex solves.
