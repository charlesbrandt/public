# Javascript Basics

Coming from another language, sometimes it can be difficult to get up to speed in a new ecosystem. 

## Interpreters

There's on in your web browser. 

Or there's [node](node.md) that you can install locally and run interactively. 

## Date Handling

See [dates](dates.md)

## Range

In python, there is a great function for range.

Can use something like lodash to add these functions in

People seem to get upset about extra libraries like lodash or jquery or ... saying that vanilla javascript is up to the task. 

Sure. 

    Array.from(new Array(20), (x, i) => i + *lowerBound*);
    
Is a native vanilla range operation. 

But `range(1,12)` is super easy to remember. Just have to remember if that upper limit is inclusive, or do you need to bump it up by one to get the range you want. Try it. See. 


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
