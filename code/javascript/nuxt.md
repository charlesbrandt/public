# Nuxt

[Vue](vue.md) is a good place to start, but Nuxt is even better.

Nuxt provides informed default options as a foundation for your project. Here are a few more reasons to use Nuxt:

https://zendev.com/2018/09/17/frontend-architecture-lessons-from-nuxt-js.html

## New Projects

Create a fresh project according to:

https://nuxtjs.org/guide/installation

Make sure you have npx installed (npx is shipped by default since NPM 5.2.0)

```
npx create-nuxt-app <project-name>
```

Or with yarn:

```
yarn create nuxt-app <my-project>
```

And answer questions accordingly.

When you set up a nuxt project, you will be able to include a number of different tools. This is an easy time to configure those. 

### Nuxt.js modules

Be sure to say "Yes!" to Nuxt.js modules: 

Axios
PWA Support
DotEnv

More info on these options:
 
 - [PWA (Progressive Web applications)](https://developers.google.com/web/progressive-web-apps/)
    - https://web.dev/what-are-pwas/
    - https://pwa.nuxtjs.org/
 
 - [DotEnv](https://github.com/motdotla/dotenv#readme)
   [Store config in the environment](https://12factor.net/config)


### Linting

I like linting too:

❯◉ ESLint
 ◉ Prettier
 ◉ Lint staged files
 ◉ StyleLint  <- Not sure about this yet... 

### Testing

Go with Jest for testing. 

  Jest 
  AVA 

### Deployment

You will need to consider how it will be deployed. Server Rendering is my first choice. 

Server Rendering (Universal SSR)
Universal == Server Side Rendering

SPA (Single Page Application)
SPA == Single Page Application

(Do *not* confuse SPA with PWA - they are different things)

https://nuxtjs.org/guide#server-rendered-universal-ssr-

https://developer.okta.com/blog/2019/04/26/tutorial-build-universal-apps-with-nuxt

### Static Builds

Both deployment options can be built for a static hosting situation. SPA will be one big .js file. Universal will have many. 

    npx nuxt generate

https://nuxtjs.org/api/configuration-generate/

https://medium.com/a-man-with-no-server/static-site-generators-nuxt-js-2fa9782d27c8




## Running

```
npm run dev
```

### Default Port

If you're running more than one node project, be sure to pick something other than the default '3000' port. 

### Configure host and port

via: https://nuxtjs.org/faq/host-port/

As direct arguments

    nuxt --hostname myhost --port 3333

Or in package.json

```
"scripts": {
  "dev": "nuxt --hostname myhost --port 3333"
}
```

Inside your nuxt.config.js:

```
export default {
  server: {
    port: 8000, // default: 3000
    host: '0.0.0.0' // default: localhost
  }
  // other configs
}
```

.env options available too


## Configuration Variable

https://nuxtjs.org/api/configuration-env

Variables in a .env file should be picked up if you configured your Nuxt project to use dotenv. 

https://github.com/nuxt-community/dotenv-module

They should be available in process.env.whatever (or context.env?)

```
  mounted() {
    // works
    console.log('Process:', process.env.BASE_URL)

    // does not work
    //console.log('Context:', context.env.BASE_URL)
    //console.log('Context:', this.env.BASE_URL)
  }
```

If you want to use them in a template in your component, you should define a local variable in `data {}` that references the corresponding process.env

``` component.vue
  data() {
    return {
      baseURL: process.env.BASE_URL
    }
  },
 ```

If you want to access those variables in a component's template without defining them locally every time, you could add them to the Vuex store



https://stackoverflow.com/questions/53535362/how-to-make-nuxt-global-object


https://nuxtjs.org/api/configuration-env/

"""
Note that Nuxt uses webpack's definePlugin to define the environmental variable. This means that the actual process or process.env from Node.js is neither available nor defined. Each of the env properties defined in nuxt.config.js is individually mapped to process.env.xxxx and converted during compilation.

Meaning, console.log(process.env) will output {} but console.log(process.env.your_var) will still output your value. When webpack compiles your code, it replaces all instances of process.env.your_var to the value you've set it to. ie: env.test = 'testing123'. If you use process.env.test in your code somewhere, it is actually translated to 'testing123'.
"""

Alternatively, you can manually configure variables in nuxt.config.js:

``` nuxt.config.js
  env: {
    baseUrl: process.env.BASE_URL || 'http://localhost:3000'
  },
```


## Templates

The general syntax for templates is handled by Vue:

https://vuejs.org/v2/guide/syntax.html

### Page Titles

  head() {
    return {
      title: this.$route.name 
    }
  },

https://stackoverflow.com/questions/48285476/using-nuxt-how-do-i-put-the-route-name-in-the-page-title#48286279


### Themes

For Vuetify, themes are defined in `nuxt.config.js`

and custom variables can be placed:

    assets/variables.scss


## External Modules

If you have an external module that you would like to use within your project, a plugin makes the most sense. You can also check if a pre-existing module already exists that handles the integration for you. 

This is a good summary of the process:

https://stackoverflow.com/questions/58694228/using-npm-packages-client-side-with-nuxt

Example:

    npm install --save lodash

Then, in plugins/lodash.js

```
import Vue from 'vue'
// import orderBy from 'lodash'
import _ from 'lodash'

Object.defineProperty(Vue.prototype, '$_', { value: _ })
// may also work...
// Vue.prototype.$_.orderBy = orderBy

```

Finally, be sure to include the plugin in your nuxt.config.js file

```
plugins: [{ src: '~/plugins/lodash' }],
  
```

https://nuxtjs.org/guide/plugins/

https://nuxtjs.org/guide/modules

https://nuxtjs.org/api/configuration-modules

See also:

https://vuejs.org/v2/guide/plugins.html


## Vuex (Persistence)

In Vue, variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of Vuex

See: [Vuex](vuex.md)


## Version

The current version is displayed when running the nuxt server interactively. Alternatively, to see the version of Nuxt currently installed for your project:

    head node_modules/nuxt/dist/nuxt.js 


