# Nuxt

[Vue](vue.md) is a good place to start, but Nuxt is even better.

Nuxt provides informed default options as a foundation for your project. Here are a few more reasons to use Nuxt:

https://zendev.com/2018/09/17/frontend-architecture-lessons-from-nuxt-js.html


## New Projects

Create a fresh project according to:

https://nuxtjs.org/guide/installation

Make sure you have npx installed (npx is shipped by default since NPM 5.2.0)

```
$ npx create-nuxt-app <project-name>
```

Or with yarn:

```
$ yarn create nuxt-app <my-project>
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
 - [DotEnv](https://github.com/motdotla/dotenv#readme)
   [Store config in the environment](https://12factor.net/config)


### Linting

I like linting too:

❯◉ ESLint
 ◉ Prettier
 ◉ Lint staged files
 ◉ StyleLint

### Testing

No preference yet on testing framework, other than choose and use one. 

  Jest 
  AVA 

### Deployment

You will need to consider how it will be deployed. 

Server Rendering (Universal SSR)
Universal == Server Side Rendering

SPA (Single Page Application)
SPA == Single Page Application

(Do *not* confuse SPA with PWA - they are different things)

https://nuxtjs.org/guide#server-rendered-universal-ssr-


## Running

```
npm run dev
```

## Templates

The general syntax for templates is handled by Vue:

https://vuejs.org/v2/guide/syntax.html

Common patterns:

List rendering
https://vuejs.org/v2/guide/list.html#key

### Page Titles

  head() {
    return {
      title: this.$route.name 
    }
  },

https://stackoverflow.com/questions/48285476/using-nuxt-how-do-i-put-the-route-name-in-the-page-title#48286279

### Scripts

The script block is where you put logic related to your component. 

data is made up of properties
computed is made up of getters.

via:
https://stackoverflow.com/questions/58931647/nuxt-component-computed-vs-data


## Vuex (Persistence)

Variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of Vuex

https://nuxtjs.org/guide/vuex-store/

https://nuxtjs.org/examples/vuex-store/

https://vuex.vuejs.org/

https://vuex.vuejs.org/guide/state.html

https://itnext.io/efficiently-understanding-and-using-nuxt-vuex-7905eb8858d6

