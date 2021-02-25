# Nuxt

[Vue](vue.md) is a good place to start, but Nuxt is even better.

Nuxt provides informed default options as a foundation for your project. [A few more reasons to use Nuxt](https://zendev.com/2018/09/17/frontend-architecture-lessons-from-nuxt-js.html)

Nuxt is improving rapidly. Always check the official docs for the most up-to-date API.

https://nuxtjs.org/api/context

## New Projects

Here's a starter boilerplate

[web-ui-api-db](https://gitlab.com/charlesbrandt/web-ui-api-db)

Or, some notes about setting up from scratch and what the options mean

[New project from scratch](./nuxt-new-project.md)

## Running

I prefer to run the application in a container where the node_modules are on a shared volume as in [web-ui-api-db](https://gitlab.com/charlesbrandt/web-ui-api-db). This keeps the node_modules out of the source tree, making it easier to grep and find the files you're looking for without getting snagged in the mountain of libraries and dependencies.

```
docker-compose up -d
```

If you need to do any maintenance / debugging in the container itself, you can always connect to it by a shell

```
docker-compose exec ui bash
```

It's also possible to run directly on the dev host.

## Components

Nuxt leverages all of Vue to take advantage of the Single File Component model.

### Sub-directories

It's okay to put components in the `components/` directory in sub-directories. e.g.

    components/common/*

Nuxt will still find them all! :)

## CSS / Styles

Styling will depend a lot on how you configure your application, which front-end CSS framework you choose, etc.

Reminder: Anything one-off or customizable should go in the corresponding web-compenent file. A CSS utility framework like Tailwind makes this even more streamlined!

### Global / Default / Main

Most projects need a default CSS setup.

For the truly global stuff, make a `main.css` file in
`@/assets/css/main.css` and configure in nuxt.config.js

No need for tailwind here? TBD

## Static directory

Lines up with the goal of having everything ready to go for a static site deployment.

`ui/static/*`

**This directory is not required, you can delete it if you don't want to use it.**

This directory contains your static files.
Each file inside this directory is mapped to `/`.
Thus you'd want to delete this README.md before deploying to production.

Example: `/static/robots.txt` is mapped as `/robots.txt`.

More information about the usage of this directory in [the documentation](https://nuxtjs.org/guide/assets#static).

## Favicon

SVG works as a Favicon in most cases
don't worry about generating all of the different sized .ico files. The SVG is it!

Place the svg in the `ui/static/` directory and then update nuxt.config.js:

    // link: [{ rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' }],
    link: [{ rel: 'icon', type: 'image/svg+xml', href: '/favicon.svg' }],

There are a few other cases that should be included. Worry about that in production:

https://medium.com/swlh/are-you-using-svg-favicons-yet-a-guide-for-modern-browsers-836a6aace3df

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

## Content

The nuxt content module is a nice way to include static content with a project without needing to rely on a database.

One gotcha, if you intend to nest content in many sub-directories, be sure to have at least one file in the first directory of content, otherwise Nuxt Content won't find the deeper items.

See also:
https://gitlab.com/charlesbrandt/web-ui-api-db/ui/content/common/nuxt/content.md

## Fetching Data

This is a big important topic.

[See fetching_data.md](fetching_data.md)

## Generating

To generate a static version of a nuxt project, make sure you have `target: 'static'` in the `nuxt.config.js`, then:

    npm run generate

or

    nuxt generate

https://nuxtjs.org/blog/going-full-static

Apps must be written to utilize server side rendering features.

If API calls only happen once the page has loaded, you won't see much improvement in SEO or load times. See `asyncData` for more details.

## Vuex (Persistence)

In Vue, variables can be passed in parameters to the component as properties and rendered in templates with slots. Eventually you may have variables that need to be referenced by multiple components. This is the time that you'll want to take advantage of Vuex

See: [Vuex](vuex.md)

Consider adding in Vuex-ORM

https://gitlab.com/charlesbrandt/public/-/blob/master/code/javascript/vuex.md

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

### Configuration Variables (.env) dotenv

Node.js automatically loads environment variables into process.env

The way to use them has changed over time.

Try out a ui/.env file. Is the value available via process.env.whatever?

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

    async asyncData ({ $config: { baseURL } }) {

and use it with

    const posts = await fetch(`${baseURL}/posts`)

In other (client-side) methods in a component, it is available via

    this.$config.baseURL

If you have code that is using the env variables you can migrate to using the \$config option. For example if in your code you had

    <p>{{process.env.baseURL}}</p>

You can change this by using \$config instead

    <p>{{$config.baseURL}}</p>

## Version

The current version is displayed when running the nuxt server interactively. Alternatively, to see the version of Nuxt currently installed for your project:

    head node_modules/nuxt/dist/nuxt.js
