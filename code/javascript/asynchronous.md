# Asynchronous Programming

Javascript is single threaded. Avoid using operations that block while they complete so the thread is free to do other work. 

The language has created a few different patterns to handle this situation. 

Originally there were callback functions... a function to call once the work was complete. 

Now there are Promises.

`async` and `await` is syntatic sugar to make using Promises easier. They create Promises behind the scenes. 

For more about this evolution... 
[The Evolution of Asynchronous JavaScript | RisingStack](https://blog.risingstack.com/asynchronous-javascript/)


## forEach loops

Before we even get into other details... IMPORTANT:

async/await does not work well with standard forEach loops. If you expect the loop to wait for the internal `await` commands, you'll be surprised to see things happening out of order. 

I feel like this issue trips me up frequently:

https://codeburst.io/javascript-async-await-with-foreach-b6ba62bbf404  
JavaScript: async/await with forEach() | by Sebastien Chopin | codeburst  

The solution is to make an asynch/await compatible forEach function:

```
async function asyncForEach(array, callback) {
  for (let index = 0; index < array.length; index++) {
    await callback(array[index], index, array);
  }
}
```

### Promise.all()

It's also possible to group promises and wait for them all to complete

https://duckduckgo.com/?t=ffab&q=javascript+promises+all&ia=web  
javascript promises all at DuckDuckGo  
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all  
Promise.all() - JavaScript | MDN  


## Async / Await

https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await

```
async function hello() { return "Hello" };
hello();
```

The async keyword is added to functions to tell them to return a promise rather than directly returning the value.

Then, in an `async` function, you can use `await` to ensure subsequent actions don't happen until after `await` completes


## Promises

Modern libraries return promises. Even with async/await being available, it's useful to understand what is happening behind the scenes. 

Promises are a way to chain actions/calls/functions without using callbacks.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise

https://stackoverflow.com/questions/31413749/node-js-promise-all-and-foreach

As long as each `.then()` returns a promise, you can chain multiple `.then` steps and finish with a single .catch

```
const axios = require(‘axios’);
axios.get(‘http://www.somepage.com')
.then(function (response) { // Reponse being the result of the first request
    // Returns another promise to the next .then(..) in the chain
    return axios.get(`http://www.somepage.com/${response.someValue}`);
})
.then(function response { // Reponse being the result of the second request
    // Handle response
})
.catch(function (error) {
    // Handle error.
});
```


## Arrow Functions

Arrow functions allow access to the local (containing parent) scope when writing functions. 

For some reason, I always associate arrow functions with promises and asynchronous coding styles in javascript. 

"Two factors influenced the introduction of arrow functions: the need for shorter functions and the behavior of the `this` keyword."

"An arrow function does not have its own `this`. The `this` value of the enclosing lexical scope is used; arrow functions follow the normal variable lookup rules. So while searching for `this` which is not present in current scope, an arrow function ends up finding the `this` from its enclosing scope."

via:

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions  
Arrow function expressions - JavaScript | MDN  

https://www.google.com/search?client=ubuntu&channel=fs&q=js+arrow+functions&ie=utf-8&oe=utf-8  
js arrow functions - Google Search  


## See Also

Library to help with asynchronous patterns

https://caolan.github.io/async/v3/  
Home - Documentation  
