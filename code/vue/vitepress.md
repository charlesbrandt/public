# Vitepress

The next incarnation of vuepress powered by vite! 

A great system for writing documentation! The refresh is so fast -- it's a live version of the application running!

https://github.com/vuejs/vitepress  
GitHub - vuejs/vitepress: Vite & Vue powered static site generator.  


## Frontmatter

TODO: document Frontmatter options that are available. (Not all sites need a "Call to Action" Button.

```
actionText: Get Started
actionLink: /README
```


## Publishing

When it's time to deploy, it include Server Side Generation (SSG). Gitlab and Github will host the output from this for you. 

Overview of the process for Gitlab

https://docs.gitlab.com/ee/user/project/pages/getting_started/pages_from_scratch.html

Adapting from:

https://gitlab.com/pages/vuepress/-/blob/master/.gitlab-ci.yml

Using a submodule, so be sure to enable those:


https://docs.gitlab.com/ee/ci/git_submodules.html


```
image: node:lts

variables:
  GIT_SUBMODULE_STRATEGY: recursive
  
pages:
  cache:
    paths:
    - node_modules/
  script:
  - yarn install
  - yarn run build
  artifacts:
    paths:
    - public
  rules:
    - if: $CI_COMMIT_REF_NAME == $CI_DEFAULT_BRANCH
```

Then, once you commit and push the `.gitlab-ci.yml`, you can log in to gitlab to see the status of the job

https://docs.gitlab.com/ee/ci/jobs/

### Configuration

Vitepress leverages vite for the build. [Vite configurations](vite.md) apply here as well.
  
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
  
