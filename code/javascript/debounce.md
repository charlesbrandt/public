# Debounce

Using a library is not necessary:

https://www.freecodecamp.org/news/javascript-debounce-example/

```
function debounce(func, timeout = 300){
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => { func.apply(this, args); }, timeout);
  };
}
function saveInput(){
  console.log('Saving data');
}
const processChange = debounce(() => saveInput());
```


## Library

```
cd ui/

yarn add debounce
```

then in the component you want to use it with, in the script block:


```
import debounce from 'debounce'
```

debounce is only available in the script block, so you can wrap any methods that need to be debounced in the debounce call



https://duckduckgo.com/?t=canonical&q=vue+debounce+method&ia=web
vue debounce method at DuckDuckGo
https://stackoverflow.com/questions/42199956/how-to-implement-debounce-in-vue2
vue.js - How to implement debounce in Vue2? - Stack Overflow
https://www.npmjs.com/package/debounce
debounce - npm
https://github.com/component/debounce/blob/master/test.js
debounce/test.js at master · component/debounce
