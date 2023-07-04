# Vite

"A build tool that aims to provide a faster and leaner development experience for modern web projects."

Next generation js bundler & development server.

The new build / bundler. And so much more.

https://vitejs.dev/guide/#overview

https://vitejs.dev/guide/#community-templates  
Community templates  

https://vitejs.dev/guide/#index-html-and-project-root  
Project Root | Vite  

https://github.com/vitejs/awesome-vite  
vitejs/awesome-vite: curated list of awesome things related to Vite.js  

https://github.com/vitejs  
vite  


## General Configuration 

https://duckduckgo.com/?q=vite+server+configuration&t=ffab&ia=web  
vite server configuration at DuckDuckGo  
https://vitejs.dev/config/#server-middlewaremode  
Configuring Vite | Vite  
https://vitejs.dev/guide/build.html#public-base-path  
Building for Production | Vite  
https://vitejs.dev/guide/backend-integration.html  
Backend Integration | Vite  
https://duckduckgo.com/?t=ffab&q=vite+server+proxy&ia=web  
vite server proxy at DuckDuckGo  


## Dev Server Proxy

When running vite directly (i.e. not behind a separate `nginx` proxy), it is possible to configure the Vite dev server to act as a proxy to the API, even if the API is at a different address. This is useful to proxy to an API being served by a local container. 


```js
import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import Components from "unplugin-vue-components/vite";
import Icons from 'unplugin-icons/vite'
import IconsResolver from 'unplugin-icons/resolver'

// https://vitejs.dev/config/
export default defineConfig({
  resolve: {
    alias: {
      '~/': `${path.resolve(__dirname, 'src')}/`,
    },
  },
  plugins: [
    vue(),
    Components({
      resolvers: [
        IconsResolver({
          componentPrefix: '',
          // enabledCollections: ['carbon']
        }),

      ],
    }),

    // https://github.com/antfu/unplugin-icons
    Icons({
      autoInstall: true,
    }),
  ],
  server: {
    // https://vitejs.dev/config/#server-https
    https: true,
    // https://vitejs.dev/config/#server-proxy
    proxy: {
      "/api": {
        target: "http://localhost:3030",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
    },
  },
});
```

## SSL

https://duckduckgo.com/?t=ffab&q=vite+ssl&ia=web  
vite ssl at DuckDuckGo  
https://github.com/web2033/vite-vue3-tailwind-starter/discussions/112  
Enabling https on localhost · Discussion #112 · web2033/vite-vue3-tailwind-starter  
https://github.com/FiloSottile/mkcert  
FiloSottile/mkcert: A simple zero-config tool to make locally trusted development certificates with any names you'd like.  


## Server Side Rendering

This is not necessarily the same thing as Static Site Generation, but it is usually a prerequisite

https://duckduckgo.com/?t=ffab&q=awesome+vue3&ia=web  
awesome vue3 at DuckDuckGo  
https://github.com/vuesomedev/awesome-vue-3  
vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  
https://vueschool.io/articles/vuejs-tutorials/nuxt-composition-api/  
Nuxt Composition API - Vue.js Tutorials  
https://duckduckgo.com/?t=ffab&q=SSR+vue3&ia=web  
SSR vue3 at DuckDuckGo  
https://v3.vuejs.org/guide/ssr.html#quasar-framework-ssr-pwa  
Server-Side Rendering | Vue.js  
https://vitejs.dev/guide/ssr.html  
Server-Side Rendering | Vite  
https://duckduckgo.com/?t=ffab&q=awesome+vite&ia=web  
awesome vite at DuckDuckGo  


## Auto Import Components

Generally, I like using auto importing with external packages that have a clear component name. Icons are a good fit. External component libraries (e.g. Element Plus) are also a good fit. 

I don't like leaving out the import statements for local code. It helps to know where components are being resolved from. It helps when importing a component to another project to know the dependencies. 

https://github.com/antfu/unplugin-vue-components

e.g.

```
npm i unplugin-vue-components -D
```

then update vite config to use the new resolver

```js
// vite.config.js
import Vue from '@vitejs/plugin-vue'
import Icons from 'unplugin-icons/vite'
import IconsResolver from 'unplugin-icons/resolver'
import Components from 'unplugin-vue-components/vite'

export default {
  plugins: [
    Vue(),
    Components({
      resolvers: IconsResolver(),
    }),
    Icons(),
  ],
}
```


## Nginx Server / Docker / Proxy / HMR

Getting vite to run behind a proxy in development mode requires forwarding ports used for websockets. 

https://vitejs.dev/config/#server-hmr  

If you run your vite server behind an upstream proxy, it can be tricky to configure to prevent noticable page refreshes as the hot module reloading (HMR) (?) stays in sync with the dev server. 

More discussion: 

https://github.com/vitejs/vite/discussions/4795  
Noticable page refresh when using nginx proxy in front of vite · Discussion #4795 · vitejs/vite  

```
    hmr: {
      // vite@2.5.1 and older
      port: 8888
      // vite@2.5.2 and newer:
      // clientPort: 8888,
    },
```

In nginx, be sure the `Upgrade` and `Connection` headers are set:

```
    location / {
        proxy_read_timeout 300s;
        proxy_connect_timeout 75s;
        proxy_pass https://boilerplate_ui:3000/;

        # allows websocket connections
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_buffering off;
    }
```

https://www.nginx.com/blog/websocket-nginx/

For a configuration example, see `docker-compose.yml`:

https://gitlab.com/fern-seed/web-ui-api-db

https://vitejs.dev/config/#publicdir  
Configuring Vite | Vite  
https://duckduckgo.com/?t=ffab&q=vite+local+development+allow+CORS+access+to+api+at+a+different+url&ia=web  
vite local development allow CORS access to api at a different url at DuckDuckGo  
https://dev.to/alirezahamid/how-to-fix-cors-issue-in-vuejs-545o  
How to fix the CORS issue in Vuejs - DEV Community  
https://duckduckgo.com/?t=ffab&q=vite+devserver+proxy&ia=web  
vite devserver proxy at DuckDuckGo  
https://stackoverflow.com/questions/64677212/how-to-configure-proxy-in-vite  
vuejs3 - How to configure proxy in Vite? - Stack Overflow  
https://duckduckgo.com/?q=vite+dev+server+ssl&t=ffab&ia=web  
vite dev server ssl at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=vite+run+dev+server+on+port+80&ia=web  
vite run dev server on port 80 at DuckDuckGo  
https://github.com/vitejs/vite/issues/726  
Allow different frontend port · Issue #726 · vitejs/vite · GitHub  
https://github.com/vitejs/vite/issues/652  
WebSocket connection can not work inside Docker container · Issue #652 · vitejs/vite · GitHub  
https://github.com/angela-1/azalea  
GitHub - angela-1/azalea: 人员管理系统  
https://github.com/vitejs/vite/issues/3093  
Vite HMR is unusable behind reverse proxies with random port numbers for client · Issue #3093 · vitejs/vite · GitHub  
https://github.com/vitejs/vite/discussions/2563  
Caddy Server Reverse Proxy SSL Infinite Loop · Discussion #2563 · vitejs/vite  

https://duckduckgo.com/?t=ffab&q=vite+nginx+proxy+visible+refresh&ia=web  
vite nginx proxy visible refresh at DuckDuckGo  

https://github.com/vitejs/vite/issues?q=is%3Aissue+is%3Aopen+visible+refresh  
Issues · vitejs/vite  
https://github.com/vitejs/vite/issues/3208  
Issues with real world app - slow refresh, rendering parts. · Issue #3208 · vitejs/vite  
https://github.com/vitejs/vite/discussions/4795  
Noticable page refresh when using nginx proxy in front of vite · Discussion #4795 · vitejs/vite · GitHub  
https://github.com/vitejs/vite/discussions/4577  
How do I debug HMR/React Fast Refresh not working? · Discussion #4577 · vitejs/vite  
https://www.google.com/search?channel=fs&client=ubuntu&q=vite+nginx+proxy+visible+refresh  
vite nginx proxy visible refresh - Google Search  
https://linuxtut.com/en/2c321374e917cc679efb/  
Points stuck when running vite + Nginx in Docker environment  

https://serverfault.com/questions/586586/nginx-redirect-via-proxy-rewrite-and-preserve-url  
Nginx Redirect via Proxy, Rewrite and Preserve URL - Server Fault  
https://duckduckgo.com/?t=ffab&q=nginx+proxy_pass+change+url&ia=web  
nginx proxy_pass change url at DuckDuckGo  
https://stackoverflow.com/questions/54084239/proxy-pass-overwrites-the-url-changed-by-rewrite-directive  
nginx - proxy_pass overwrites the URL changed by rewrite directive - Stack Overflow  


## Static Assets

Place static assets in the `/public` directory

vite assets at DuckDuckGo  
https://vitejs.dev/guide/assets.html#importing-script-as-a-worker  


## Misc Links

Vite + Cypress

https://v3.vuejs.org/guide/composition-api-introduction.html#why-composition-api  
Introduction | Vue.js  
https://vueuse.org/guide/index.html  
Get Started | VueUse  
https://duckduckgo.com/?t=ffab&q=vite+cypress+io&ia=web  
vite cypress io at DuckDuckGo  
https://github.com/Vannsl/vite-vue-cypress-tailwind  
GitHub - Vannsl/vite-vue-cypress-tailwind  
https://github.com/Uninen/vite-ts-tailwind-starter  
GitHub - Uninen/vite-ts-tailwind-starter: Opinionated Vite + Vue 3 + TypeScript + Tailwind CSS starter.  
https://github.com/cypress-io/cypress/commit/4095af74d633  
feat: adding support for vite-dev-server · cypress-io/cypress@4095af7 · GitHub  
https://github.com/JessicaSachs/cypress-loves-vite  
GitHub - JessicaSachs/cypress-loves-vite  
https://duckduckgo.com/?t=ffab&q=vue3+project+setup&ia=web  
vue3 project setup at DuckDuckGo  
https://learnvue.co/2020/12/setting-up-your-first-vue3-project-vue-3-0-release/  
Creating Your First Vue 3 Project - A Vue Tutorial – LearnVue  
https://duckduckgo.com/?q=vue3+server+side+render+vite&t=ffab&ia=web  
vue3 server side render vite at DuckDuckGo  
https://vitejs.dev/guide/ssr.html#setting-up-the-dev-server  
Server-Side Rendering | Vite  
https://vitejs.dev/guide/backend-integration.html  
Backend Integration | Vite  
https://v3.vuejs.org/guide/ssr.html#the-complete-ssr-guide  
Server-Side Rendering | Vue.js  
https://v3.vuejs.org/guide/ssr/introduction.html#why-ssr  
Server-Side Rendering Guide | Vue.js  
https://duckduckgo.com/?q=vue+put+scaffold+generate+in+different+%2Fsrc+directory%3F&t=ffab&ia=web  
vue put scaffold generate in different /src directory? at DuckDuckGo  
https://itnext.io/how-to-structure-a-vue-js-project-29e4ddc1aeeb  
How to Structure a Vue.js Project | by Sandoche ADITTANE | ITNEXT  
https://stackoverflow.com/questions/48851677/how-to-direct-vue-cli-to-put-built-project-files-in-different-directories  
vue.js - How to direct vue-cli to put built project files in different directories? - Stack Overflow  

https://github.com/antfu/vitesse  
GitHub - antfu/vitesse: 🏕 Opinionated Vite Starter Template  
https://github.com/frandiox/vitesse-ssr-template  
GitHub - frandiox/vitesse-ssr-template: 🏕 Opinionated Vite Starter Template with SSR in Node.js  
https://github.com/JohnCampionJr/vitesse-addons  
GitHub - JohnCampionJr/vitesse-addons: Different branches show how to add on different things to a Vitesse setup  
https://duckduckgo.com/?t=ffab&q=vite+ssg&ia=software  
vite ssg at DuckDuckGo  
https://github.com/antfu/vite-ssg  
GitHub - antfu/vite-ssg: Server-side generation for Vite  
https://github.com/frandiox/vite-ssr  
GitHub - frandiox/vite-ssr: Use Vite for server side rendering in Node  


https://github.com/topics/frontend  
frontend · GitHub Topics · GitHub  
https://github.com/vueuse/head  
GitHub - vueuse/head: Document `<head>` manager for Vue 3. SSR ready.  
https://github.com/vueuse  
VueUse · GitHub  
https://github.com/vueuse/vueuse-vite-starter/tree/master/src  
vueuse-vite-starter/src at master · vueuse/vueuse-vite-starter · GitHub  

## See Also

https://duckduckgo.com/?t=ffab&q=vite+setup&ia=web  
vite setup at DuckDuckGo  

[vitepress](vitepress.md)

