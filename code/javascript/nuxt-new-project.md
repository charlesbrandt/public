# New Nuxt Project


## From Scratch

If you want to generate a new Nuxt project from scratch, create a fresh project according to:

https://nuxtjs.org/guide/installation

I find I have to run `npx create-nuxt-app <project-name>` outside of the container, but then remove the node_modules directory at the host level to prevent cluttering searches with .node_module files. (`rm -r node_modules`)

Make sure you have npx installed (npx is shipped by default since NPM 5.2.0)

```
npx create-nuxt-app <project-name>
```

Or with yarn:

```
yarn create nuxt-app <my-project>
```

And answer questions accordingly.

### package.json

edit package.json:

```
"scripts": {
  "dev": "nuxt --hostname 0.0.0.0",
}
```


## Configuration Option Descriptions

This is what I'm using at the moment. Details about the choices below. Your mileage may vary.

create-nuxt-app v3.2.0
✨ Generating Nuxt.js project in boilerplate
? Project name: boilerplate
? Programming language: JavaScript
? Package manager: Yarn
? UI framework: None
? Nuxt.js modules: Axios, Progressive Web App (PWA), Content
? Linting tools: ESLint, Prettier
? Testing framework: Jest
? Rendering mode: Universal (SSR / SSG)
? Deployment target: Static (Static/JAMStack hosting)
? Development tools: jsconfig.json (Recommended for VS Code if you're not using
typescript)

```

When you set up a nuxt project, you will be able to include a number of different tools. This is an easy time to configure those, but it's also good to learn how to add them later.

### Package manager: Yarn

Both `yarn` and `npm` are available in the default docker container for node.

Collaborators seem to prefer `yarn` so I'm going with that. I don't have a strong preference here. 

### UI framework: None

Lots of good options. Use what your team is using for team projects.

There are some configurations I like to set up to make the variables available to the components. (see below)

### Nuxt.js modules

Be sure to say "Yes!" to Nuxt.js modules:

Axios
PWA Support
Content (new support for referencing markdown content directly?)

DotEnv (haven't seen this lately... moved to core and part of default?)

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
```
