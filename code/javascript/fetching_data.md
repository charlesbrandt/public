# Fetching Data

Most front-end applications need a way to get data from the back end services they leverage.

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


[See also nuxt notes](nuxt.md)

https://axios.nuxtjs.org/usage/
Usage - Axios Module
https://github.com/axios/axios
axios/axios: Promise based HTTP client for the browser and node.js


## Errors

You can still catch errors with await calls by chaining the calls to `catch`, similar to the way you would with promises. 

https://duckduckgo.com/?t=canonical&q=how+to+catch+error+with+await&ia=web
how to catch error with await at DuckDuckGo
https://dev.to/sobiodarlington/better-error-handling-with-async-await-2e5m
Better error handling with async/await - DEV
https://stackoverflow.com/questions/33562284/how-do-i-catch-thrown-errors-with-async-await
javascript - How do I catch thrown errors with async / await? - Stack Overflow
