# Boilerplates

## Vitesse

https://github.com/antfu/vitesse

For working on the repo  
git clone git@github.com:antfu/vitesse.git

https://github.com/antfu/vitesse  
antfu/vitesse:  Opinionated Vite Starter Template  

TODO:  
vitesse with a vue-3-chakra-ui   

https://github.com/antfu/vitesse  
GitHub - antfu/vitesse:  Opinionated Vite Starter Template  
https://github.com/vuejs/rfcs/pull/227  
`<script setup>` by yyx990803 · Pull Request #227 · vuejs/rfcs · GitHub  
https://github.com/GoogleChromeLabs/critters  
GitHub - GoogleChromeLabs/critters:  A Webpack plugin to inline your critical CSS and lazy-load the rest.  
  
https://github.com/antfu/vitesse  
GitHub - antfu/vitesse:  Opinionated Vite Starter Template  
https://github.com/antfu/vitesse/tree/master/src/pages  
vitesse/src/pages at master · antfu/vitesse · GitHub  
https://github.com/antfu/vitesse/blob/master/vite.config.ts  
vitesse/vite.config.ts at master · antfu/vitesse · GitHub  
https://github.com/hannoeru/vite-plugin-pages  
GitHub - hannoeru/vite-plugin-pages: File system based route generator for Vite  
https://duckduckgo.com/?t=ffab&q=%27vite-plugin-vue-layouts%27&ia=web  
'vite-plugin-vue-layouts' at DuckDuckGo  
https://www.npmjs.com/package/vite-plugin-vue-layouts  
vite-plugin-vue-layouts - npm  
https://github.com/JohnCampionJr/vite-plugin-vue-layouts  
JohnCampionJr/vite-plugin-vue-layouts: Vue layout plugin for Vite  
https://github.com/hannoeru/vite-plugin-pages  
hannoeru/vite-plugin-pages: File system based route generator for Vite  
https://duckduckgo.com/?t=ffab&q=vite+assets&ia=web  
Static Asset Handling | Vite  



https://duckduckgo.com/?t=ffab&q=awesome+vite&ia=web  
awesome vite at DuckDuckGo  
https://github.com/vitejs/awesome-vite  
vitejs/awesome-vite:  A curated list of awesome things related to Vite.js  
https://github.com/antfu/icones  
GitHub - antfu/icones:  Icon Explorer with Instant searching, powered by Iconify  
https://icones.js.org/collection/all  
Icônes  
https://github.com/vuejs/vitepress  
vuejs/vitepress: Vite & Vue powered static site generator.  
https://vitepress.vuejs.org/guide/using-vue.html  
Using Vue in Markdown | VitePress  

https://github.com/antfu/vitesse  
 antfu/vitesse: Opinionated Vite Starter Template  
https://github.com/Uninen/vite-ts-tailwind-starter  
Uninen/vite-ts-tailwind-starter: Opinionated Vite + Vue 3 + TypeScript + Tailwind CSS starter.  
https://github.com/antfu/vitesse/issues/77  
 Suggest implement ready-to-use testing system · Issue #77 · antfu/vitesse  
https://github.com/antfu/vite-ssg/issues  
 Issues · antfu/vite-ssg  
  
  

## VVT

I've been very happy with:

https://github.com/web2033/vite-vue3-tailwind-starter  
web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter)  

I love how minimal this one is. 

All the necessities and that's it.

Doesn't get in the way if you want to include other things or customize. 

The main downside (IMO) is no automatic route generation for pages. 


No need to include full git repo history

```
npx degit web2033/vite-vue3-tailwind-starter vvt-app
cd vvt-app
```

If running under docker, don't forget to disable automatic vscode launching 

ui/vite.config.js

```
  server: {
    // open: true,
  },
  
```

Otherwise you may see an error like the following:

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
GitHub - web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter)   
https://github.com/web2033/vite-vue3-tailwind-starter/commit/2503518694988fc447d5c2b3f9c03daab87110e3  
Prefer `<script setup>` way · web2033/vite-vue3-tailwind-starter@2503518 · GitHub  
https://github.com/web2033/vite-vue3-tailwind-starter/discussions/112  
Enabling https on localhost · Discussion #112 · web2033/vite-vue3-tailwind-starter · GitHub  
https://github.com/rsms/inter  
GitHub - rsms/inter: The Inter font family  
https://headlessui.dev/vue/disclosure  
Headless UI – Unstyled, fully accessible UI components  
  
https://github.com/web2033/vite-vue3-tailwind-starter  
 web2033/vite-vue3-tailwind-starter: Vite 2.x + Vue 3.x + Tailwind 2.x (starter)   
  


## Vitepress

Not a true boilerplate, but a very useful related tool:

[Vitepress](vitepress.md)


## Boilerplates / Templates Continued

Roughly sorted by number of stars, but that's subject to change.

https://github.com/troisjs/trois  
GitHub - troisjs/trois:  ThreeJS + VueJS 3 + ViteJS   
https://github.com/slidevjs/slidev  
GitHub - slidevjs/slidev: Presentation Slides for Developers (Beta)  
  

https://github.com/frandiox/vite-ssr  
frandiox/vite-ssr: Use Vite for server side rendering in Node  
https://github.com/frandiox/vitedge  
frandiox/vitedge: Edge-side rendering and fullstack Vite framework  
  
https://github.com/xiaoxian521/vue-pure-admin  
 xiaoxian521/vue-pure-admin:    Vue3.0+TypeScript+Vite2.0+Element-Plus编写的一套后台管理系统  
  

https://github.com/boussadjra/vite-wind  
 boussadjra/vite-wind: Vue 3 boilerplate starter based on Vite and Tailwindcss 2  
https://github.com/d2-projects/d2-advance  
 d2-projects/d2-advance: （重构中）  Advanced, colorful front-end integration practice. be inspired by D2Admin --- D2 探索版，追求更好的前端工程实践，探索中后台及其以外的更多应用场景 --- 学习交流 qq 群 633692947，微信群请联系微信号 mxxxjia 备注加群  
https://github.com/pohunchn/vite-ts-quick  
 pohunchn/vite-ts-quick  
https://github.com/fast-crud/fast-crud  
 fast-crud/fast-crud: 面向配置的crud框架，开发crud 快如闪电；Options-oriented crud framework, develop crud as fast as lightning；based on vue3,  
https://github.com/logustra/vivu  
 logustra/vivu: A highly scalable vue boilerplate using vite  
https://github.com/yooneskh/vite-neutralinojs-template  
 yooneskh/vite-neutralinojs-template: Starter template for Neutralino.js with Vite.js and Vue.js  
  
https://duckduckgo.com/?t=ffab&q=vue3+tailwind+component+library&ia=web  
 vue3 tailwind component library at DuckDuckGo  
  
https://github.com/posva/vite-tailwind-starter  
 posva/vite-tailwind-starter: Starter using Vite + Tailwind for super fast prototyping  
  
  
