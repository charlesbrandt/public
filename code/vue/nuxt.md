# Nuxt

Builds on [Vue](index.md). 

Nuxt provides informed default options as a foundation for your project. [A few more reasons to use Nuxt](https://zendev.com/2018/09/17/frontend-architecture-lessons-from-nuxt-js.html)

https://nuxtjs.org/api/context

## Components

Nuxt leverages all of Vue to take advantage of the Single File Component model.

`index.vue` is required in nuxt under dynamic (e.g. `_id`) paths. Capital `Index.vue` will not work. Maybe this is a bug, but for now it seems safest to stick with kebab-cased filenames.

### Sub-directories

It's okay to put components in the `components/` directory in sub-directories. e.g.

    components/common/*

Nuxt will still find them all! :)

## Where things go

### Assets vs Static

https://nuxtjs.org/guide/assets#static

https://raw.githubusercontent.com/nuxt/nuxtjs.org/master/content/en/guides/directory-structure/assets.md

The `assets` directory contains your uncompiled assets such as Stylus or Sass files, images, or fonts.

### Images

Inside your `vue` templates, if you need to link to your `assets` directory use `~/assets/your_image.png` with a slash before assets.

```html
<template>
  <img src="~/assets/your_image.png" />
</template>
```

Inside your `css` files, if you need to reference your `assets` directory, use `~assets/your_image.png`(without a slash)

```css
background: url("~assets/banner.svg");
```

When working with dynamic images you will need to use require

```html
<img :src="require(`~/assets/img/${image}.jpg`)" />

<base-alert type="next">
```

## Static directory

Good to reserve this for larger binary files like .jpg and media files.

Smaller files that

Lines up with the goal of having everything ready to go for a static site deployment.

`ui/static/*`

**This directory is not required, you can delete it if you don't want to use it.**

This directory contains your static files.
Each file inside this directory is mapped to `/`.
Thus you'd want to delete this README.md before deploying to production.

Example: `/static/robots.txt` is mapped as `/robots.txt`.

More information about the usage of this directory in [the documentation](https://nuxtjs.org/guide/assets#static).


## Page Titles / Meta Tags

```
  head() {
    return {
      title: this.$route.name,
      meta: [
        {
          hid: "description",
          name: "description",
          content:
            "Articles focused on the beautiful art of landscape painting.",
        }
      ]
    }
  },
```

https://nuxtjs.org/docs/2.x/features/meta-tags-seo/

If these are set in child components, those settings may over-ride settings in the main page being rendered. It is best to set these only at a page level, and avoid including pages in other pages.

https://stackoverflow.com/questions/48285476/using-nuxt-how-do-i-put-the-route-name-in-the-page-title#48286279

https://redfern.dev/articles/adding-social-media-seo-meta-data-using-nuxt-content/

## Layouts

A page component can specify the layout to use in script section with

```
<script>
export default {
   ...

   layout: 'default',
}
</script>
```

As of version 2.14, there is a bug when multiple layouts are available.

https://github.com/nuxt/nuxt.js/issues/3877

### Error Pages

Error pages are a special type of Layout.

Nuxt does not ship with a layout for error pages by default. This will be called for any un-handled error conditions.

https://duckduckgo.com/?t=canonical&q=ReferenceError%3A+NuxtError+is+not+defined&ia=web
ReferenceError: NuxtError is not defined at DuckDuckGo
https://stackoverflow.com/questions/63846716/nuxtjs-referenceerror-nuxterror-is-not-defined
javascript - NuxtJs: ReferenceError: NuxtError is not defined - Stack Overflow
https://nuxtjs.org/guides/directory-structure/layouts#error-page
layouts - NuxtJS

## Routing

TODO:
How to specify parameters via the route?

see web-ui-api-db/ui/pages/docs/\* as an example pattern

https://nuxtjs.org/docs/2.x/directory-structure/pages#dynamic-pages

in ./pages/ directory:

index.vue must be lowercase here! (Will not work with Index.vue)

## Content

see web-ui-ap-db/ui/content

The nuxt content module is a nice way to include static content with a project without needing to rely on a database.

One gotcha, if you intend to nest content in many sub-directories, be sure to have at least one file in the first directory of content, otherwise Nuxt Content won't find the deeper items.

See also:
https://gitlab.com/charlesbrandt/web-ui-api-db/ui/content/common/nuxt/content.md

## Fetching Data

This is a big important topic.

[See fetching data](/code/javascript/fetching-data.md)

## Generating

To generate a static version of a nuxt project, make sure you have `target: 'static'` in the `nuxt.config.js`, then:

    npm run generate

or

    nuxt generate

https://nuxtjs.org/blog/going-full-static

Apps must be written to utilize server side rendering features.

If API calls only happen once the page has loaded, you won't see much improvement in SEO or load times. See `asyncData` for more details.

## External Modules / Plugins

https://nuxtjs.org/guide/plugins/

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
// may be possible to get something like this to work...
// doesn't work as is
// Vue.prototype.$_.orderBy = _.orderBy

```

Finally, be sure to include the plugin in your nuxt.config.js file

```
plugins: [{ src: '~/plugins/lodash' }],

```

https://nuxtjs.org/guide/plugins/

https://nuxtjs.org/guide/modules

https://nuxtjs.org/api/configuration-modules

See also:https://getpocket.com/explore/item/a-world-without-clouds?utm_source=pocket-newtab

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

Vue level, what's happening:

https://blog.logrocket.com/accessing-properties-globally-in-vue-js-with-prototypes/


## Version

The current version is displayed when running the nuxt server interactively. Alternatively, to see the version of Nuxt currently installed for your project:

    head node_modules/nuxt/dist/nuxt.js

## Configuration

### Configure host and port

via: https://nuxtjs.org/faq/host-port/

As direct arguments

    nuxt --hostname 0.0.0.0 --port 3333

In package.json

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

### Linting

At first I liked having linting enabled by default. Fixing issues as they come up is a nice way to work. 

However, when using docker it's not always possible to use the same version of eslint in my IDE as the application. This can lead to situations when the editor auto-formats it one way and the application linter blocks saying there is an error. So far I haven't had success with hunting for the configuration parameters to make everything agree. 

In this situation, it's easier to just disable linting. In `nuxt.config.js`:

``` 
  // Modules for dev and build (recommended) (https://go.nuxtjs.dev/config-modules)
  buildModules: [
    // https://go.nuxtjs.dev/eslint
    // '@nuxtjs/eslint-module',
  ],
```

Maybe there is a better way?

https://github.com/nuxt/nuxt.js/issues/2097
How can I remove ESLint from an existing Nuxt app? · Issue #2097 · nuxt/nuxt.js

https://duckduckgo.com/?t=ffab&q=nuxt+disable+lint+checker&ia=web
nuxt disable lint checker at DuckDuckGo
https://nuxtjs.org/docs/2.x/features/configuration#pre-processors
Configuration - NuxtJS


### Configuration Variables 

It's possible to leverage [Node's built in environment variable loading](../javascript/node.html#environment-variables)

Best Practices:

x Don’t commit sensitive values or secret keys to git

x Don't store secret keys or sensitive values in your nuxt.config or .env unless is gitignored

✅ Use default values for runtimeConfig such as process.env.baseURL || 'https://nuxt.js.org'

✅ Store secret keys correctly using your hosting platform such as on Heroku or Netlify etc
✅ Follow JS naming convention (secretKey rather than SECRET_KEY) for runtimeConfig
✅ Prefer using runtimeConfig rather than env option

[via](https://nuxtjs.org/blog/moving-from-nuxtjs-dotenv-to-runtime-config)

#### Defining Environment Variables

in nuxt.config.js

```
export default {

  publicRuntimeConfig: {
    baseURL: process.env.BASE_URL || 'https://nuxtjs.org'
  },
  privateRuntimeConfig: {
    apiSecret: process.env.API_SECRET
  },

}
```

This approach specifies which variables get sent to the public / client / browser when that gets built (publicRuntimeConfig) versus those that are only meant for the server side (privateRuntimeConfig).

#### Using Environment Variables

In async data, you can pass in the configs you want to use:

```
async asyncData ({ $config: { baseURL } }) {
```

and use it with

```
const posts = await fetch(`${baseURL}/posts`)
```

In other (client-side) methods in a component, it is available via

```
this.$config.baseURL
```

If you have code that is using the env variables you can migrate to using the \$config option. For example if in your code you had

```
<p>{{process.env.baseURL}}</p>
```

You can change this by using \$config instead

```
<p>{{$config.baseURL}}</p>
```

## Nuxt 3

Nuxt 3 is now available as a public beta option:

https://v3.nuxtjs.org/  
Nuxt 3 - The Hybrid Vue Framework  
https://nuxtjs.org/announcements/nuxt3-beta/  
Nuxt - Introducing Nuxt 3 Beta  
https://v3.nuxtjs.org/getting-started/introduction  
Nuxt 3 - Introduction  
https://github.com/unjs  
unjs · GitHub  
