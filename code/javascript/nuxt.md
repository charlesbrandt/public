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
 
 - [DotEnv](https://github.com/motdotla/dotenv#readme)
   [Store config in the environment](https://12factor.net/config)


### Linting

I like linting too:

❯◉ ESLint
 ◉ Prettier
 ◉ Lint staged files
 ◉ StyleLint

### Testing

Go with Jest for testing. 

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

https://developer.okta.com/blog/2019/04/26/tutorial-build-universal-apps-with-nuxt

### Default Port

If you're running more than one node project, be sure to pick something other than the default '3000' port. 

### Static Builds

Both deployment options can be built for a static hosting situation. SPA will be one big .js file. Universal will have many. 

    npx nuxt generate

https://nuxtjs.org/api/configuration-generate/

https://medium.com/a-man-with-no-server/static-site-generators-nuxt-js-2fa9782d27c8


## Running

```
npm run dev
```

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

## External Modules

https://nuxtjs.org/guide/plugins/

https://nuxtjs.org/guide/modules

https://nuxtjs.org/api/configuration-modules


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


## Vuex (Persistence)

Variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of Vuex

https://nuxtjs.org/guide/vuex-store/

https://nuxtjs.org/examples/vuex-store/

https://vuex.vuejs.org/

https://vuex.vuejs.org/guide/state.html

https://itnext.io/efficiently-understanding-and-using-nuxt-vuex-7905eb8858d6

