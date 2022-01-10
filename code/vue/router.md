# Routing

https://router.vuejs.org/guide/#html  
Getting Started | Vue Router  


Internal links within an application use `router-link`

```
<router-link to="/foo">Go to Foo</router-link>
```

To include the current view within a template, use the special slot:

```
<router-view></router-view>
```


## Nested Routes


https://router.vuejs.org/guide/essentials/nested-routes.html

## Data Fetching

https://router.vuejs.org/guide/advanced/data-fetching.html#fetching-after-navigation

## File based routes

Typically route configurations are manually specified in a routes.js file. 

It is possible to automatically generate routes based on patterns in the way files are named on the file system. 

Vite-plugin-pages does this:

https://github.com/hannoeru/vite-plugin-pages

[Vitesse](https://github.com/hannoeru/vite-plugin-pages) uses this plugin to add file based routing. 

How do we access the route parameters?


## Resources

A really good talk at vueconf (2021) about some of the magic being done by vue-router. 

Great video by VueMastery -- should definitely check out their great videos and support the community while you're at it. They support Vue! :)

https://duckduckgo.com/?t=canonical&q=vue+router+dynamic+routes&ia=web  
vue router dynamic routes at DuckDuckGo  
https://next.router.vuejs.org/guide/essentials/dynamic-matching.html#reacting-to-params-changes  
Dynamic Route Matching with Params | Vue Router  
https://stackoverflow.com/questions/57836601/how-to-make-dynamic-routes-on-the-vue-router  
vue.js - How to make dynamic routes on the vue router? - Stack Overflow  
https://router.vuejs.org/guide/essentials/navigation.html  
Programmatic Navigation | Vue Router  
https://next.router.vuejs.org/guide/advanced/dynamic-routing.html#adding-routes  
Dynamic Routing | Vue Router  

