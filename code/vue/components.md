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


## File Uploader

Not Vue per-se, but should work well with vue. Looks to have a robust set of features:

https://github.com/transloadit/uppy  
transloadit/uppy: The next open source file uploader for web browsers  
https://uppy.io/  
Uppy  
https://uppy.io/docs/  
Getting Started — Uppy  
https://uppy.io/docs/golden-retriever/  
Golden Retriever — Uppy  

https://github.com/topics/tus  
tus · GitHub Topics  
https://tus.io/  
tus - resumable file uploads  

As found via:

https://duckduckgo.com/?t=ffab&q=uploading+large+files+via+browser&ia=web  
uploading large files via browser at DuckDuckGo  
https://stackoverflow.com/questions/26257525/large-file-upload-through-browser-100-gb  
php - Large file upload through Browser (100 GB) - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=html5+uploader&ia=software  
html5 uploader at DuckDuckGo  
https://github.com/mpangrazzi/html5-uploader  
mpangrazzi/html5-uploader: A pure HTML5 file uploader  
https://github.com/topics/html5-uploader  
html5-uploader · GitHub Topics  
https://github.com/topics/uploader  
uploader · GitHub Topics  

https://github.com/lian-yue/vue-upload-component  
lian-yue/vue-upload-component: Vue.js file upload component, Multi-file upload, Upload directory, Drag upload, Drag the directory, Upload multiple files at the same time, html4 (IE 9), `PUT` method, Customize the filter  

### 
https://duckduckgo.com/?q=vue3+filebrowser+component&t=ffab&ia=web  
vue3 filebrowser component at DuckDuckGo  
https://www.smashingmagazine.com/2022/03/drag-drop-file-uploader-vuejs-3/  
How To Make A Drag-and-Drop File Uploader With Vue.js 3 — Smashing Magazine  
https://github.com/n1crack/vuefinder  
n1crack/vuefinder: Vuefinder is a file manager component for vuejs  
https://github.com/n1crack/vuefinder/tree/master/src/components/modals  
vuefinder/src/components/modals at master · n1crack/vuefinder · GitHub  
https://vuejsexamples.com/5-best-file-manager-for-vue-js/  
5 Best File Manager For Vue.js  
https://github.com/ashishdoneriya/vuedisk  
GitHub - ashishdoneriya/vuedisk: File Manager using vuejs, element ui as frontend and php as backend  
https://github.com/cyon/vue3-file-selector  
GitHub - cyon/vue3-file-selector: A Vue 3 headless file selector component  
https://github.com/semeniuk/vuetify-file-browser  
semeniuk/vuetify-file-browser: Open source file manager component for web (Vue.js, Vuetify, Node.js)  

