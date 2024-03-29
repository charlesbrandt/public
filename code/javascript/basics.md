# Javascript Basics

Coming from another language, it can be difficult to get up to speed in a new ecosystem. 

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference

## Interpreters

There's one in your web browser. 

Or there's [node](node.md) that you can install locally and run interactively. 

## Dates

``` js
Date.now()
```

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date

See also: [dates](dates.md)


## Type Casting

String to a Number:

``` js
var x = Number("1000")
```

parseInt is another option

``` js
var x = parseInt("1000", 10); 
```

## Strings 

### Remove whitespace

``` js
myString = myString.trim();
```

https://stackoverflow.com/questions/10032024/how-to-remove-leading-and-trailing-white-spaces-from-a-given-html-string

In python, this is equivalent to `.strip()`

### Replace string

``` js
p.replace('original', 'new')
```

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace

### Sanitize string

Use a regular expression to remove all non-alphanumeric characters from a string:

```js
input.replace(/\W/g, '')
```

https://stackoverflow.com/questions/9364400/remove-not-alphanumeric-characters-from-string

### Split String

``` js
const parts = original.split('/')
```

Suppose you just want the YYYY-MM-DD part of a string...

``` js
var fulldate = "2021-04-30T18:06:15.625Z";
var day = fulldate.substring(0, 10);
```

Looks like an alternative is `fulldate.substr(2, 2)` -- is there a difference? 

`substring` doesn't accept negative indexes like python:

``` js
cwd = line.substring(0, -1)  // won't work!
```

Instead, use

``` js
cwd = line.substring(0, line.length - 1)
```

### Join Array into String

``` js
console.log(elements.join('-'));
```

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join

### Backticks / Template Literals

Template literals can be used to represent multi-line strings and may use "interpolation" to insert variables:

``` js
var a = 123, str = `---
   a is: ${a}
---`;
console.log(str);
```

https://stackoverflow.com/questions/27678052/usage-of-the-backtick-character-in-javascript
Usage of the backtick character (`) in JavaScript - Stack Overflow

### Does one string contain another string

``` js
const string = "foo";
const substring = "oo";

console.log(string.includes(substring)); // true
```

https://stackoverflow.com/questions/1789945/how-to-check-whether-a-string-contains-a-substring-in-javascript

### Formatting Numbers


``` js
const str1 = '5';
console.log(str1.padStart(2, '0'));
```

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/padStart  
https://duckduckgo.com/?q=javascript+format+number+leading+zeros&ia=web  
  
## Arrays

### Length

``` js
items.length
```

Note: It's a property, not a method

### Negative Indexing

Cannot use a negative index to get an item a certain distance from the end of an array. Instead, use the array length property

```js
items[items.length - 2]
```

### Check for item in array

For frequently checking if an item is part of a group, and object is more efficient. If an array is necessary / easier, `includes()` is a good option:

``` js
[1, 2, 3].includes(1) => true
```

https://stackoverflow.com/questions/7378228/check-if-an-element-is-present-in-an-array

`indexOf` is the most widely compatible method:

``` js
[1, 2, 3].indexOf(1) => 0
```

Important: to test for existence in an array, check if the result == -1:

``` js
if ([1, 2, 3].indexOf(4) === -1) {
   console.log("does not exist")
}
```

Sometimes, with arrays of complex objects, `indexOf()` or `includes()` doesn't work. In that case, a raw iteration over the array of elements is the next best option. 

``` js
array.forEach((item, index) => {
  console.log(item)
}
```

Once you know what you want to compare, update the check:

``` js
this.otherProjects = []
this.projects.forEach((item, index) => {
  if (item._id !== this.project._id) {
    this.otherProjects.push(item)
  } else {
    // console.log('skipping current project', item)
  }
})
```

There is also `array.some()`. The `some()` method tests whether at least one element in the array passes the test implemented by the provided function. It returns a Boolean value. 

``` js
const array = [1, 2, 3, 4, 5];

// checks whether an element is even
const even = (element) => element % 2 === 0;

console.log(array.some(even));
// expected output: true
```

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some

### Modify Arrays

To append a value to the end of an array:

``` js
array.push(item);
```

To insert a value at the beginning of an array:

``` js
array.unshift(item);
```

https://stackoverflow.com/questions/8073673/how-can-i-add-new-array-elements-at-the-beginning-of-an-array-in-javascript

splice() changes the original array 
slice() preserves the original array
both return the modified array

``` js 
//splice
var array=[1,2,3,4,5];
console.log(array.splice(2));

//slice
var array2=[1,2,3,4,5]
console.log(array2.slice(2));

console.log("----after-----");
console.log(array);
console.log(array2);
```

https://stackoverflow.com/questions/37601282/javascript-array-splice-vs-slice

### Remove item from array

To remove an element from an array:

``` js
var index = array.indexOf(item);
if (index !== -1) array.splice(index, 1);
```

https://stackoverflow.com/questions/3954438/how-to-remove-item-from-array-by-value

### Map, Filter, Reduce

Higher level functions for working with items in an array

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map

``` js
// Arrow function
map((element) => { ... })
map((element, index) => { ... })
map((element, index, array) => { ... })
```


## Copy Objects / Arrays

For a quick deep copy: `JSON.parse(JSON.stringify(jsonObject))`

For a shallow copy, the spread syntax may be enough:

``` js
var A3 = {...A1};  // Spread Syntax
```

https://stackoverflow.com/questions/122102/what-is-the-most-efficient-way-to-deep-clone-an-object-in-javascript

https://stackoverflow.com/questions/728360/how-do-i-correctly-clone-a-javascript-object

The slice() array method can be used to copy arrays by not passing any arguments `arr1 = arr0.slice()`

### Spread Operator 

'...' Three dots in front of an array expands it out. Can be used to copy. 

https://tenmilesquare.com/5-uses-for-the-spread-operator/


## `for` Loops

Can always use the basic format of

``` js
for (let i=0, i<10, i++) {
   console.log(i);
}
```

That's a bit verbose in most cases. Usually want to iterate over _something_, just be sure to use the right version:

https://stackoverflow.com/questions/3010840/loop-through-an-array-in-javascript

`for - of` works for iterating items in a list

``` js
for (const x of xs) { console.log(x); }
```

`for - in` is used to enumerate object properties 
NOT GOOD FOR LISTS!

There is also

``` js 
xs.forEach((x, i) => console.log(x));
```

Loop over an object's keys in ES6

``` js
Object.keys(items).forEach((x, i) => {
  console.log(x);
  console.log(items[x]);
});
```

### Loop & Remove

If you want to remove items from an array that you are looping over, work backwards from the end:

``` js
for (var i = items.length - 1; i >= 0; i--) {
    if (items[i].label == next.label) { 
        items.splice(i, 1);
    }
}
```

https://stackoverflow.com/questions/9882284/looping-through-array-and-removing-items-without-breaking-for-loop



## Objects

### Show attributes of object

```js
console.log(Object.keys(app));
```

very similar to python dir() command


### Check for item in object 

If you want to frequently test if an item exists, using an Object / Hash / Dictionary / Map should perform better than an array.

    myObj.hasOwnProperty('myKey');
    
Note: when doing this in a Vue application with eslint enabled, you may get an error like:

    error    Do not access Object.prototype method 'hasOwnProperty' from target object  no-prototype-builtins

To get around that, you can use the following pattern:

    Object.prototype.hasOwnProperty.call(myObj, 'myKey')

### Group Items by Key

Handy when you want to know how many items share the same key, but you also want to keep track of the list of the original items

```js
let groups = {}

originalList.forEach(item => {
    const curName = item.attributeOfInterest?.toUpperCase()
    // console.log("Looking for existing group:", curName);

    if (Object.prototype.hasOwnProperty.call(groups, curName)) {
      groups[curName].push(item)
    } else {
      groups[curName] = [item]
    }
})
```

### Remove a key from an object

```
delete exampleObject["key"];
```

https://stackoverflow.com/questions/3455405/how-do-i-remove-a-key-from-a-javascript-object


## Regular Expressions

`let re = /ab+c/;`

`match()` 	Returns an array containing all of the matches, including capturing groups, or null if no match is found.
`matchAll()`	Returns an iterator containing all of the matches, including capturing groups.
`search()` 	Tests for a match in a string. It returns the index of the match, or -1 if the search fails.
`replace()` 	Executes a search for a match in a string, and replaces the matched substring with a replacement substring.
`replaceAll()` 	Executes a search for all matches in a string, and replaces the matched substrings with a replacement substring.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions

Remember, if using `search()` be sure to check if the position === -1.
Just using `if-search` will always be true (because -1 is not false in javascript). 


## Time

Basic way is to use `setTimeout` with a callback function:

```js
setTimeout(() => {
    console.log("Called after 1 second");
}, 1000);
```

A more promise friendly approach

```js
function delay(milliseconds){
    return new Promise(resolve => {
        setTimeout(resolve, milliseconds);
    });
}

async function stuff(){
    console.log("Called immediately");

    await delay(1000);

    console.log("Called after 1 second delay");
}

stuff();

```



https://duckduckgo.com/?q=javascript+wait+1+second&t=ffab&ia=web  
javascript wait 1 second at DuckDuckGo  
https://alvarotrigo.com/blog/wait-1-second-javascript/  
Force to Wait 1 Second [JavaScript - 2022]  


## Range

In python, there is a great function for range.

Can use something like lodash to add these functions in

People seem to get upset about extra libraries like lodash or jquery or ... saying that vanilla javascript is up to the task. 

Sure. 

    Array.from(new Array(20), (x, i) => i + *lowerBound*);
    
Is a native vanilla range operation. 

But `range(1,12)` is super easy to remember. Just have to remember if that upper limit is inclusive, or do you need to bump it up by one to get the range you want. Try it. See. 

``` js
    range(start = 0, end = undefined, step = 1) {
      console.log('range called with', start, end, step)
      start = parseInt(start)
      end = parseInt(end)
      step = parseInt(step)

      let result = []
      while (start < end) {
        start += step
        if (start <= end) {
          result.push(start)
        }
      }
      return result
    },

```

    console.log([...range(2, 5, 2)])

Not memory efficient for large ranges!! 

Adapted from:

https://dev.to/ycmjason/how-to-create-range-in-javascript-539i



https://duckduckgo.com/?t=canonical&q=javascript+range&ia=web  
javascript range at DuckDuckGo  
https://stackoverflow.com/questions/3895478/does-javascript-have-a-method-like-range-to-generate-a-range-within-the-supp  
arrays - Does JavaScript have a method like "range()" to generate a range within the supplied bounds? - Stack Overflow  
https://2ality.com/2014/05/es6-array-methods.html  
ECMAScript 6’s new array methods  
https://dev.to/ycmjason/how-to-create-range-in-javascript-539i  
JavaScript Range: How to create range in Javascript - DEV Community  
https://duckduckgo.com/?t=canonical&q=javascript+array+slice&ia=web  
javascript array slice at DuckDuckGo  
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice  
Array.prototype.slice() - JavaScript | MDN  
