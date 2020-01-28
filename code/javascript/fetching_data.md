# Fetching Data

Most front-end applications need a way to get data from the back end services they leverage.

[Axios seems to be the way to go](https://github.com/axios/axios)

    yarn add axios

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


