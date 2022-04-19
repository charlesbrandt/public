// https://vitepress.vuejs.org/guide/theming.html#using-a-custom-theme

// interface Theme {
//   Layout: Component // Vue 3 component
//   NotFound?: Component
//   enhanceApp?: (ctx: EnhanceAppContext) => void
// }

// interface EnhanceAppContext {
//   app: App // Vue 3 app instance
//   router: Router // VitePress router instance
//   siteData: Ref<SiteData>
// }

import Layout from './layout.vue'

import DefaultTheme from 'vitepress/theme'
// import DarkTheme from 'vitepress-dark-theme/index.js'
// export default { ...DarkTheme }

export default {
   ...DefaultTheme,
  //...DarkTheme,
  enhanceApp({ app }) {
    // register global components
    app.component('MyGlobalComponent' /* ... */)
  }
}

/*
export default {
  Layout,
  NotFound: () => 'custom 404', // <- this is a Vue 3 functional component
  enhanceApp({ app, router, siteData }) {
    // app is the Vue 3 app instance from `createApp()`.
    // router is VitePress' custom router.
    `siteData`` is a `ref`` of current site-level metadata.
  }
}
*/
