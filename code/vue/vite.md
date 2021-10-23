# Vite

Next generation js bundler & deployment tool (???)

The new build / bundler. And so much more. 

https://vitejs.dev/guide/#community-templates  
Getting Started | Vite  
https://vitejs.dev/guide/#index-html-and-project-root  
Getting Started | Vite  


https://duckduckgo.com/?t=ffab&q=vite+setup&ia=web  
vite setup at DuckDuckGo  


https://github.com/vitejs/awesome-vite  
💤 vitejs/awesome-vite: ⚡️ A curated list of awesome things related to Vite.js  

## Getting Started

https://vitejs.dev/guide/#overview


## Configuration

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



https://serverfault.com/questions/586586/nginx-redirect-via-proxy-rewrite-and-preserve-url  
Nginx Redirect via Proxy, Rewrite and Preserve URL - Server Fault  
https://duckduckgo.com/?t=ffab&q=nginx+proxy_pass+change+url&ia=web  
nginx proxy_pass change url at DuckDuckGo  
https://stackoverflow.com/questions/54084239/proxy-pass-overwrites-the-url-changed-by-rewrite-directive  
nginx - proxy_pass overwrites the URL changed by rewrite directive - Stack Overflow  


## Vitepress

https://github.com/vuejs/vitepress  
GitHub - vuejs/vitepress: Vite & Vue powered static site generator.  

### Configuration

Vitepress leverages vite for the build. Vite configurations apply here as well.   
  
https://vitepress.vuejs.org/guide/configuration.html  
Configuration | VitePress  
https://vitepress.vuejs.org/config/basics.html  
App Config: Basics | VitePress  
https://github.com/vuejs/vitepress/blob/main/src/node/config.ts#L15  
vitepress/config.ts at main · vuejs/vitepress · GitHub  
  
https://duckduckgo.com/?q=vitepress&t=ffab&ia=web  
vitepress at DuckDuckGo  



https://duckduckgo.com/?t=ffab&q=vuepress+edit+content&ia=web  
vuepress edit content at DuckDuckGo  
https://vuepress.vuejs.org/config/#basic-config  
Config Reference | VuePress  
https://vuepress.vuejs.org/guide/  
Introduction | VuePress  
https://duckduckgo.com/?t=ffab&q=vuepress+vite&ia=web  
vuepress vite at DuckDuckGo  
https://vitepress.vuejs.org/  
What is VitePress? | VitePress  
https://vitepress.vuejs.org/guide/getting-started.html  
Getting Started | VitePress  
https://vitepress.vuejs.org/guide/deploy.html#github-pages  
Deploying | VitePress  
https://vitepress.vuejs.org/guide/using-vue.html#using-components  
Using Vue in Markdown | VitePress  
https://vitepress.vuejs.org/guide/theming.html  
Theming | VitePress  
https://vitepress.vuejs.org/guide/frontmatter.html#head  
Frontmatter | VitePress  
https://github.com/jonschlinkert/gray-matter  
GitHub - jonschlinkert/gray-matter: Smarter YAML front matter parser, used by metalsmith, Gatsby, Netlify, Assemble, mapbox-gl, phenomic, and many others. Simple to use, and battle tested. Parses YAML by default but can also parse JSON Front Matter, Coffee Front Matter, TOML Front Matter, and has support for custom parsers.  
https://duckduckgo.com/?t=ffab&q=TypeError%3A+Cannot+destructure+property+%27locales%27+of+%27siteData.themeConfig%27+as+it+is+undefined.&ia=web  
TypeError: Cannot destructure property 'locales' of 'siteData.themeConfig' as it is undefined. at DuckDuckGo  
https://github.com/vuejs/vitepress/issues/379  
Cannot destructure property 'locales' of 'siteData.themeConfig' as it is undefined. · Issue #379 · vuejs/vitepress · GitHub  
https://vitejs.dev/config/#server-middlewaremode  
Configuring Vite | Vite  
  

## VVT

I love how minimal this one is. Doesn't get in the way if you want to include other things or customize. 

No need to include full git repo history though

https://github.com/web2033/vite-vue3-tailwind-starter  
💤 web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter) ⚡  

like the approach.

```
yarn run dev
yarn run v1.22.5
$ vite --host

  vite v2.5.0-beta.3 dev server running at:

  > Local:    http://localhost:3000/
  > Network:  http://192.168.48.2:3000/

  ready in 321ms.

events.js:292
      throw er; // Unhandled 'error' event
      ^

Error: spawn xdg-open ENOENT
    at Process.ChildProcess._handle.onexit (internal/child_process.js:269:19)
    at onErrorNT (internal/child_process.js:465:16)
    at processTicksAndRejections (internal/process/task_queues.js:80:21)
Emitted 'error' event on ChildProcess instance at:
    at Process.ChildProcess._handle.onexit (internal/child_process.js:275:12)
    at onErrorNT (internal/child_process.js:465:16)
    at processTicksAndRejections (internal/process/task_queues.js:80:21) {
  errno: -2,
  code: 'ENOENT',
  syscall: 'spawn xdg-open',
  path: 'xdg-open',
  spawnargs: [ 'http://localhost:3000/' ]
}
error Command failed with exit code 1.
info Visit https://yarnpkg.com/en/docs/cli/run for documentation about this command.

```

https://github.com/web2033/vite-vue3-tailwind-starter  
GitHub - web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter) ⚡  
https://github.com/web2033/vite-vue3-tailwind-starter/commit/2503518694988fc447d5c2b3f9c03daab87110e3  
Prefer `<script setup>` way · web2033/vite-vue3-tailwind-starter@2503518 · GitHub  
https://github.com/web2033/vite-vue3-tailwind-starter/discussions/112  
Enabling https on localhost · Discussion #112 · web2033/vite-vue3-tailwind-starter · GitHub  
https://github.com/rsms/inter  
GitHub - rsms/inter: The Inter font family  
https://headlessui.dev/vue/disclosure  
Headless UI – Unstyled, fully accessible UI components  
  
https://github.com/web2033/vite-vue3-tailwind-starter  
💤 web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter) ⚡  
  




## Vitesse

https://github.com/antfu/vitesse

For working on the repo  
git clone git@github.com:antfu/vitesse.git

https://github.com/antfu/vitesse  
antfu/vitesse: 🏕 Opinionated Vite Starter Template  

TODO:  
vitesse with a vue-3-chakra-ui   

https://github.com/antfu/vitesse  
GitHub - antfu/vitesse: 🏕 Opinionated Vite Starter Template  
https://github.com/vuejs/rfcs/pull/227  
`<script setup>` by yyx990803 · Pull Request #227 · vuejs/rfcs · GitHub  
https://github.com/GoogleChromeLabs/critters  
GitHub - GoogleChromeLabs/critters: 🦔 A Webpack plugin to inline your critical CSS and lazy-load the rest.  
  
https://github.com/antfu/vitesse  
GitHub - antfu/vitesse: 🏕 Opinionated Vite Starter Template  
https://github.com/antfu/vitesse/tree/master/src/pages  
vitesse/src/pages at master · antfu/vitesse · GitHub  
https://github.com/antfu/vitesse/blob/master/vite.config.ts  
vitesse/vite.config.ts at master · antfu/vitesse · GitHub  
https://github.com/hannoeru/vite-plugin-pages  
GitHub - hannoeru/vite-plugin-pages: File system based route generator for ⚡️Vite  
https://duckduckgo.com/?t=ffab&q=%27vite-plugin-vue-layouts%27&ia=web  
'vite-plugin-vue-layouts' at DuckDuckGo  
https://www.npmjs.com/package/vite-plugin-vue-layouts  
vite-plugin-vue-layouts - npm  
https://github.com/JohnCampionJr/vite-plugin-vue-layouts  
JohnCampionJr/vite-plugin-vue-layouts: Vue layout plugin for Vite  
https://github.com/hannoeru/vite-plugin-pages  
hannoeru/vite-plugin-pages: File system based route generator for ⚡️Vite  
https://duckduckgo.com/?t=ffab&q=vite+assets&ia=web  
vite assets at DuckDuckGo  
https://vitejs.dev/guide/assets.html#importing-script-as-a-worker  
Static Asset Handling | Vite  



https://duckduckgo.com/?t=ffab&q=awesome+vite&ia=web  
awesome vite at DuckDuckGo  
https://github.com/vitejs/awesome-vite  
vitejs/awesome-vite: ⚡️ A curated list of awesome things related to Vite.js  
https://github.com/antfu/icones  
GitHub - antfu/icones: ⚡️ Icon Explorer with Instant searching, powered by Iconify  
https://icones.js.org/collection/all  
Icônes  
https://github.com/vuejs/vitepress  
vuejs/vitepress: Vite & Vue powered static site generator.  
https://vitepress.vuejs.org/guide/using-vue.html  
Using Vue in Markdown | VitePress  

https://github.com/antfu/vitesse  
💤 antfu/vitesse: 🏕 Opinionated Vite Starter Template  
https://github.com/Uninen/vite-ts-tailwind-starter  
Uninen/vite-ts-tailwind-starter: Opinionated Vite + Vue 3 + TypeScript + Tailwind CSS starter.  
https://github.com/antfu/vitesse/issues/77  
💤 Suggest implement ready-to-use testing system · Issue #77 · antfu/vitesse  
https://github.com/antfu/vite-ssg/issues  
💤 Issues · antfu/vite-ssg  
  
  


## Boilerplates / Templates Continued

Roughly sorted by number of stars, but that's subject to change.

https://github.com/troisjs/trois  
GitHub - troisjs/trois: ✨ ThreeJS + VueJS 3 + ViteJS ⚡  
https://github.com/slidevjs/slidev  
GitHub - slidevjs/slidev: Presentation Slides for Developers (Beta)  
  

https://github.com/frandiox/vite-ssr  
frandiox/vite-ssr: Use Vite for server side rendering in Node  
https://github.com/frandiox/vitedge  
frandiox/vitedge: Edge-side rendering and fullstack Vite framework  
  
https://github.com/xiaoxian521/vue-pure-admin  
💤 xiaoxian521/vue-pure-admin: 🔥 ✨✨ ✨ Vue3.0+TypeScript+Vite2.0+Element-Plus编写的一套后台管理系统  
  

https://github.com/boussadjra/vite-wind  
💤 boussadjra/vite-wind: Vue 3 boilerplate starter based on Vite and Tailwindcss 2  
https://github.com/d2-projects/d2-advance  
💤 d2-projects/d2-advance: （重构中） 🧗 Advanced, colorful front-end integration practice. be inspired by D2Admin --- D2 探索版，追求更好的前端工程实践，探索中后台及其以外的更多应用场景 --- 学习交流 qq 群 633692947，微信群请联系微信号 mxxxjia 备注加群  
https://github.com/pohunchn/vite-ts-quick  
💤 pohunchn/vite-ts-quick  
https://github.com/fast-crud/fast-crud  
💤 fast-crud/fast-crud: 面向配置的crud框架，开发crud 快如闪电；Options-oriented crud framework, develop crud as fast as lightning；based on vue3,  
https://github.com/logustra/vivu  
💤 logustra/vivu: A highly scalable vue boilerplate using vite  
https://github.com/yooneskh/vite-neutralinojs-template  
💤 yooneskh/vite-neutralinojs-template: Starter template for Neutralino.js with Vite.js and Vue.js  
  
https://duckduckgo.com/?t=ffab&q=vue3+tailwind+component+library&ia=web  
💤 vue3 tailwind component library at DuckDuckGo  
  
https://github.com/posva/vite-tailwind-starter  
💤 posva/vite-tailwind-starter: Starter using Vite + Tailwind for super fast prototyping  
  
  
## Server Side Rendering

This is not necessarily the same thing as Static Site Generation, but it is usually a prerequisite

https://duckduckgo.com/?t=ffab&q=awesome+vue3&ia=web  
💤 awesome vue3 at DuckDuckGo  
https://github.com/vuesomedev/awesome-vue-3  
💤 vuesomedev/awesome-vue-3: A curated list of awesome things related to Vue 3  
https://vueschool.io/articles/vuejs-tutorials/nuxt-composition-api/  
💤 Nuxt Composition API - Vue.js Tutorials  
https://duckduckgo.com/?t=ffab&q=SSR+vue3&ia=web  
💤 SSR vue3 at DuckDuckGo  
https://v3.vuejs.org/guide/ssr.html#quasar-framework-ssr-pwa  
💤 Server-Side Rendering | Vue.js  
https://vitejs.dev/guide/ssr.html  
💤 Server-Side Rendering | Vite  
https://duckduckgo.com/?t=ffab&q=awesome+vite&ia=web  
awesome vite at DuckDuckGo  

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

