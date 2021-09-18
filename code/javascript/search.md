# Search 

Many different search libraries exist for javascript. 

Fuse seems to be the one that is most popular. 

Lunr.js is one that has come across my path in the past. 

It's possible to filter a collection of objects manually, for example:

https://stackoverflow.com/questions/37241191/how-to-filter-a-complex-json-object-using-javascript  
jquery - How to Filter a complex json object using javascript? - Stack Overflow

A small library like fuse helps standardize the process. 

## Fuse

Fuse.js is a powerful, lightweight fuzzy-search library, with zero dependencies.

https://fusejs.io/

### Installation

docker-compose exec ui bash

```
yarn add fuse.js

```

### Basic Usage 

```js
import Fuse from 'fuse.js'

const options = { keys: ['title', 'author.firstName'] }

// Create the Fuse index
const myIndex = Fuse.createIndex(options.keys, books)
// initialize Fuse with the index
const fuse = new Fuse(books, options, myIndex)

```

https://fusejs.io/api/indexing.html

```
const options = {
  includeScore: true,
  // Search in `author` and in `tags` array
  keys: ['author', 'tags']
}

const fuse = new Fuse(list, options)

const result = fuse.search('tion')

```

https://fusejs.io/examples.html#search-object-array


### Vue Web Component

```
<template>
    <div>Search: <input v-model="search" /></div>
    <section class="container px-1 py-4 mx-auto">
      <div class="item-list grid gap-6 mb-8 md:grid-cols-2 lg:grid-cols-4">
        <item-card
          v-for="element in filterItems"
          :key="element._id"
          :item="element"
        />
      </div>
    </section>
</template>

<script>
import Fuse from 'fuse.js'

export default {
  data() {
    return {
      items: [],
      search: '',
      fuseOptions: {
        keys: [
          'name',
        ],
        includeScore: true,
        shouldSort: true,
      },
      fuseIndex: null,
      fuse: null,
    }
  },

  computed: {
    filterItems() {
      if (this.fuse !== null && this.search) {
        const matches = this.fuse.search(this.search)
        // console.log('Matches after filter', matches)
        const itemsOnly = matches.map((match) => {
          return match.item
        })
        // console.log('items only', itemsOnly)
        return itemsOnly
      } else {
        return this.items
      }
    },
  },
  
  methods: {
    getData() {
      this.$axios
        .get(this.$config.apiUrl + 'items')
        .then((res) => {
          this.items = res.data

          // Create the Fuse index
          this.fuseIndex = Fuse.createIndex(this.fuseOptions.keys, this.items)
          // initialize Fuse with the index
          this.fuse = new Fuse(this.items, this.fuseOptions, this.fuseIndex)

        })
        .catch((err) => {
          console.log(err)
        })
    }
  }
</script>
```


