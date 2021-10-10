# Vitepress

A great system for writing documentation! The refresh is so fast -- it's a live version of the application running!

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

## Links

https://duckduckgo.com/?t=ffab&q=vitepress&ia=web
💤 vitepress at DuckDuckGo
https://vitepress.vuejs.org/guide/getting-started.html
💤 Getting Started | VitePress
https://vitepress.vuejs.org/config/basics.html
💤 App Config: Basics | VitePress
https://github.com/vuejs/vitepress/blob/master/src/node/config.ts#L15
💤 vitepress/config.ts at master · vuejs/vitepress · GitHub
https://vitepress.vuejs.org/config/homepage.html
Theme Config: Homepage | VitePress
https://duckduckgo.com/?t=ffab&q=vuepress+navigation&ia=web
vuepress navigation at DuckDuckGo
https://stackoverflow.com/questions/66531421/how-to-make-vuepress-dynamically-create-the-side-navigation
vue.js - How to make vuepress dynamically create the side navigation? - Stack Overflow
https://github.com/vuejs/vuepress/issues/613
Option to automatically list sub-directory in the sidebar · Issue #613 · vuejs/vuepress · GitHub
https://github.com/ozum/vuepress-bar
GitHub - ozum/vuepress-bar: VuePress sidebar and navbar generator based on file and directory structure. Focus your documents, not sidebar or navbar.
https://duckduckgo.com/?t=ffab&q=vitepress+customize+default+layout&ia=web
vitepress customize default layout at DuckDuckGo
https://vitepress.vuejs.org/guide/theming.html#using-a-custom-theme
Theming | VitePress
https://vitepress.vuejs.org/guide/using-vue.html#browser-api-access-restrictions
Using Vue in Markdown | VitePress
https://duckduckgo.com/?t=ffab&q=vitepress+boilerplate&ia=web
vitepress boilerplate at DuckDuckGo
https://duckduckgo.com/?t=ffab&q=vitepress+theme+example&ia=web
vitepress theme example at DuckDuckGo
https://duckduckgo.com/?t=ffab&q=vuepress+theme&ia=web
vuepress theme at DuckDuckGo
https://vuepress-theme-mix.netlify.app/
Home | VuePress Mix Theme
https://github.com/gavinliu6/vuepress-theme-mix
GitHub - gavinliu6/vuepress-theme-mix: VuePress Theme Mix for VuePress 2
https://github.com/topics/vuepress-theme
vuepress-theme · GitHub Topics · GitHub
https://github.com/vuepress-reco/vuepress-theme-reco
GitHub - vuepress-reco/vuepress-theme-reco: 💥 A simple and beautiful vuepress Blog & Doc theme.
https://duckduckgo.com/?t=ffab&q=vitepress+features&ia=web
vitepress features at DuckDuckGo
https://learnvue.co/2021/01/write-beautiful-documentation-quickly-with-vitepress/
Write Beautiful Documentation Quickly with Vitepress
https://github.com/jonschlinkert/gray-matter
GitHub - jonschlinkert/gray-matter: Smarter YAML front matter parser, used by metalsmith, Gatsby, Netlify, Assemble, mapbox-gl, phenomic, and many others. Simple to use, and battle tested. Parses YAML by default but can also parse JSON Front Matter, Coffee Front Matter, TOML Front Matter, and has support for custom parsers.
https://vitepress.vuejs.org/guide/frontmatter.html
Frontmatter | VitePress
https://vitepress.vuejs.org/guide/assets.html
Asset Handling | VitePress
https://vitepress.vuejs.org/guide/deploy.html#github-pages
💤 Deploying | VitePress
https://duckduckgo.com/?t=ffab&q=vitepress+use+current+directory+instead+of+docs+subdirectory&ia=web
💤 vitepress use current directory instead of docs subdirectory at DuckDuckGo
https://duckduckgo.com/?t=ffab&q=vitepress+build+configure+output+directory&ia=web
💤 vitepress build configure output directory at DuckDuckGo
https://vuepress.vuejs.org/config/#temp
💤 Config Reference | VuePress
https://vitepress.vuejs.org/guide/deploy.html
Deploying | VitePress
