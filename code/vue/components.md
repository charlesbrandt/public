# Vue Components

Ideally, wrap native HTML elements with the minimum functionality. Maximize accessibility. 

## Date picker

Native date picker expects strings to be "YYYY-MM-DD". Convert back and forth to native `Date` objects as needed. 

```html
        <v-input label="Date Created" prepend-icon="mdi-calendar">
          <input class="ml-4" v-model="_date_retired" type="date" id="created" name="created" />
        </v-input>
```


```js
    _date_created: {
      get() {
        let current = new Date(this.form.date_created)
        return current.toISOString().substring(0, 10)
      },
      set(newVal) {
        this.form.date_created = new Date(newVal)
        console.log("Created date updated:", this.form.date_created)
      }
    },
```

See also [Date Handling](/code/javascript/dates.md)  
