---
title: Getting started
description: 'Empower your NuxtJS application with @nuxt/content module: write in a content/ directory and fetch your Markdown, JSON, YAML and CSV files through a MongoDB like API, acting as a Git-based Headless CMS.'
---

Empower your NuxtJS application with `@nuxtjs/content` module: write in a `content/` directory and fetch your Markdown, JSON, YAML and CSV files through a MongoDB like API, acting as a **Git-based Headless CMS**.

## Fetching content

The content module is injected globally via `this.$content`. (For plugins, asyncData, nuxtServerInit and Middleware, it's available via `context.$content`.)

The main `$content` method accepts two parameters: `(path, options?)`

`path` is the string path to match in your content directory. 

`options = { deep: true }` ensures that subdirectories are checked for content

`options = { text: true }` returns the original markdown content in a `text` attribute. 

A number of other modifier methods can be chained on to the main one to filter and shape the data. 


.sortBy('createdAt', 'asc')

```
    const [prev, next] = await $content('articles')
      .only(['title', 'slug'])
      .sortBy('createdAt', 'asc')
      .surround(params.slug)
      .fetch()
```

Default Injected variables

The nuxt content module gives us access to injected variables that we can access and show in our template. Let's take a look at the default variables that are injected into our document:

    body: body text
    dir: directory
    extension: file extension (.md in this example)
    path: the file path
    slug: the file slug
    toc: an array containing our table of contents
    createdAt: the file creation date
    updatedAt: the date of the last file update

https://nuxtjs.org/blog/creating-blog-with-nuxt-content

https://content.nuxtjs.org/fetching.

### TODO Errors

The `error` in the snippet below does not exist. Not sure how to call or where example came from. Maybe Nuxt handles on its own? 

      .catch((err) => {
        console.log(err) // eslint-disable-line
        error({ statusCode: 404, message: 'Page not found' })
      })
    // console.log('Available Content: ', this.everything)




## Writing content

Learn how to write your `content/`, supporting Markdown, YAML, CSV and JSON: https://content.nuxtjs.org/writing.

## Displaying content

Learn how to display your Markdown content with the `<nuxt-content>` component directly in your template: https://content.nuxtjs.org/displaying.



## Other links

Useful resources

https://nuxtjs.org/blog/creating-blog-with-nuxt-content
Create a Blog with Nuxt Content - NuxtJS

https://content.nuxtjs.org/fetching/
Fetching content - Nuxt Content

https://content.nuxtjs.org/snippets/
Snippets - Nuxt Content
