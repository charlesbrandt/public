# Fetching Data

Most front-end applications need a way to get data from the back end services they leverage.

## Axios

[Axios seems to be the way to go](https://github.com/axios/axios)

    npm add axios

then use it:

```
<script>
  import axios from 'axios'

  export default {
    async asyncData ({ params }) {
      let { data } = await axios.get(`http://localhost:3000/index.json`)
      return { items: data }
    },
```

Be sure to enable requests on the back-end server:  
https://stackoverflow.com/questions/17262170/bottle-py-enabling-cors-for-jquery-ajax-requests

https://axios.nuxtjs.org/usage/
Usage - Axios Module
https://github.com/axios/axios
axios/axios: Promise based HTTP client for the browser and node.js

## GET vs POST

GET requests expect a 'params' option


```
        axios
          .get(`${apiUrl}/verify`, {
          params: {
            ticket: ticket,
            service: this.$store.state.globals.Return,
            }
          })
```

POST requests pass the second attribute to the API

```
        axios
          .post(`${apiUrl}/authentication`, {
            ticket: ticket,
            service: this.$store.state.globals.Return,
          })
```


## Errors

You can still catch errors with await calls by chaining the calls to `catch`, similar to the way you would with promises.

https://duckduckgo.com/?t=canonical&q=how+to+catch+error+with+await&ia=web
how to catch error with await at DuckDuckGo
https://dev.to/sobiodarlington/better-error-handling-with-async-await-2e5m
Better error handling with async/await - DEV
https://stackoverflow.com/questions/33562284/how-do-i-catch-thrown-errors-with-async-await
javascript - How do I catch thrown errors with async / await? - Stack Overflow

## Nuxt

### Remote calls

In a method, you can make remote calls with something like

    const ip = await this.$axios.$get('http://icanhazip.com')

https://axios.nuxtjs.org/usage/

Nuxt comes with special functions for handling axios requests when called from a server side rendering context.

### fetch

https://nuxtjs.org/blog/understanding-how-fetch-works-in-nuxt-2-12

Fetch is an improved method for retrieving data.

The component context is available. Assignments to data in the local context can be made directly with this approach (via `this`).

### asyncData

The component context is not available directly in asyncData, so a context must be passed in. The return value will eventually get merged in with the component's data.

https://nuxtjs.org/guide/async-data/

### Gotchas

If you get an error like:

    ECONNREFUSED 127.0.0.1:8888at TCPConnectWrap.afterConnect

The nuxt server may be initiating the axios call for server side rendering. If you're using a containerized setup with nginx acting as a proxy, the source address may be different relative to the server. To get around this, specify a different source based on context (server vs client). You can also use a method that gets called on mounted so it only runs on the client. (skip server rendering)

Wrap request in `if (process.server)` within the retrieval methods of the page

```
    let urlPrefix
    if (process.server) {
      urlPrefix = context.$config.apiUrlSSR
    } else {
      urlPrefix = context.$config.apiUrl
    }

    const personData = await context.$axios.get(
      urlPrefix + 'user/details/' + context.params.id
    )
    return {
      person: personData.data,
    }
```

https://stackoverflow.com/questions/61354470/nuxt-fetching-data-only-on-server-side

More configuration options

https://axios.nuxtjs.org/options/
