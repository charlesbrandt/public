# Nuxt

[Vue](vue.md) is a good place to start, but Nuxt is even better.

Nuxt provides informed default options as a foundation for your project. A few more reasons to use Nuxt:

https://zendev.com/2018/09/17/frontend-architecture-lessons-from-nuxt-js.html

## New Projects

Create a fresh project according to:

https://nuxtjs.org/guide/installation


Ideally run in a container to prevent node_modules from cluttering the source tree. (currently (2020.07.28) having trouble with this)

Make sure you have npx installed (npx is shipped by default since NPM 5.2.0)

```
npx create-nuxt-app <project-name>
```

Or with yarn:

```
yarn create nuxt-app <my-project>
```

And answer questions accordingly.


Or just run it outside of the container, but then remember:

```
rm -rf node_modules
```

### Default Configurations

This is what I'm using at the moment. Details about the choices below. Your mileage may vary. 

```
create-nuxt-app v3.2.0
✨  Generating Nuxt.js project in project
? Project name: project
? Programming language: JavaScript
? Package manager: Npm
? UI framework: Bulma
? Nuxt.js modules: Axios, Progressive Web App (PWA), Content
? Linting tools: ESLint, Prettier
? Testing framework: Jest
? Rendering mode: Universal (SSR / SSG)
? Deployment target: Static (Static/JAMStack hosting)
? Development tools: jsconfig.json (Recommended for VS Code if you're not using 
typescript)
```


When you set up a nuxt project, you will be able to include a number of different tools. This is an easy time to configure those. 

### Package manager: Npm

`npm` is available in the default docker container for node. 

Also not a big deal to go with `yarn` if it has features you prefer.

### UI framework: Bulma

Lots of good options. Use what your team is using for team projects. 

### Nuxt.js modules

Be sure to say "Yes!" to Nuxt.js modules: 

Axios  
PWA Support  
Content   (new support for referencing markdown content directly?)  

DotEnv   (haven't seen this lately... moved to core and part of default?)

More info on these options:
 
 - [PWA (Progressive Web applications)](https://developers.google.com/web/progressive-web-apps/)
    - https://web.dev/what-are-pwas/
    - https://pwa.nuxtjs.org/
 
 - [DotEnv](https://github.com/motdotla/dotenv#readme)
   [Store config in the environment](https://12factor.net/config)

### Linting

I like linting too:

 ◉ ESLint  
❯◉ Prettier  
(skip these)  
 ◯ Lint staged files  
 ◯ StyleLint  

### Testing

Jest is a good place to start for testing.  
Feel free to explore other options as desired.

### Rendering mode

Universal (SSR / SSG)

You will need to consider how it will be rendered. Server Rendering is my first choice. 

Server Rendering (Universal SSR)  
Universal == Server Side Rendering  

SPA (Single Page Application)  
SPA == Single Page Application  

(Do not confuse SPA with PWA -- they are different things)

https://nuxtjs.org/guide#server-rendered-universal-ssr-

https://developer.okta.com/blog/2019/04/26/tutorial-build-universal-apps-with-nuxt

### Deployment target

Static (Static/JAMStack hosting)

Static Builds

Both rendering options can be built for a static hosting situation. SPA will be one big .js file. Universal will have many. 

    npx nuxt generate

https://nuxtjs.org/api/configuration-generate/

https://medium.com/a-man-with-no-server/static-site-generators-nuxt-js-2fa9782d27c8

### Development tools 

jsconfig.json (Recommended for VS Code)




## Running

It's best to run the application in a container where the node_modules are on a shared volume as in [web-ui-api-db](https://gitlab.com/charlesbrandt/web-ui-api-db). This keeps the node_modules out of the source tree, making it easier to grep and find the files you're looking for without getting snagged in the mountain of libraries and dependencies. 

It is possible to run directly on the dev host, but see above about cluttering the path. 

```
docker-compose up -d
docker-compose exec ui bash
```

In the container

```
npm run dev

```


## Templates

The general syntax for templates is handled by Vue:

https://vuejs.org/v2/guide/syntax.html

## Routing

TODO:
How to specify parameters via the route? 

in ./pages/ directory:


## Vuex (Persistence)

In Vue, variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of Vuex

See: [Vuex](vuex.md)

## Page Titles

  head() {
    return {
      title: this.$route.name 
    }
  },

https://stackoverflow.com/questions/48285476/using-nuxt-how-do-i-put-the-route-name-in-the-page-title#48286279


## Style / Sass

Nuxt includes Vue Loader out of the box. To get Sass working, 

    npm install --save-dev node-sass sass-loader
    
https://nuxtjs.org/faq/pre-processors/

To get, for example, bulma utilities available in every component

This guide seems better... may allow skipping style-resources?
https://www.gavsblog.com/blog/adding-bulma-to-nuxt-js-with-changeable-variables


npm install --save @nuxtjs/style-resources

Then in components, this allows

<style scoped lang="scss">
.responsive-logo {
  border: 1px dashed #990000;
  @include until($desktop) {
    border-bottom: 1px solid green;
    width: '112';
    height: '28';
  }

  @include from($tablet) {
    border-left: 2px solid yellow;
  }

  @include tablet {
    border-top: 2px solid orange;
  }
  @include widescreen-only {
    border-right: 2px solid blue;
  }
}
</style>


another guide
https://www.freecodecamp.org/news/up-goind-with-nuxt-js-bulma-and-sass/



https://nuxtjs.org/api/configuration-css/


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


If the plugin depends on being rendered on the client, be sure to set 'ssr: false' when including it in the nuxt.config.js:

```
export default {
  plugins: [
    { src: '~/plugins/vuex-persist', ssr: false }
  ]
}
```

https://medium.com/@codebeast_/why-your-third-party-plugin-dont-work-in-nuxt-and-how-to-fix-it-d1a8caadf422



## Configuration

### Configure host and port

via: https://nuxtjs.org/faq/host-port/

As direct arguments

    nuxt --hostname 0.0.0.0 --port 3333

Or in package.json

```
"scripts": {
  "dev": "nuxt --hostname 0.0.0.0 --port 3333"
}
```

Inside your nuxt.config.js:

```
export default {
  server: {
    port: 8000, // default: 3000
    host: '0.0.0.0' // default: localhost
  },
  // other configs
}
```

.env options available too


### Configuration Variables (.env)

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

### Themes

For Vuetify, themes are defined in `nuxt.config.js`

and custom variables can be placed:

    assets/variables.scss




## Version

The current version is displayed when running the nuxt server interactively. Alternatively, to see the version of Nuxt currently installed for your project:

    head node_modules/nuxt/dist/nuxt.js 


