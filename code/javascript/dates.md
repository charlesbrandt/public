# Date Handling

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date

## String Parsing

Usually start with a string as a source. Convert that to a Date object in Javascript

https://stackoverflow.com/questions/5619202/converting-a-string-to-a-date-in-javascript

Within the native Date library documentation, there are many warnings: 

> Note: Parsing of date strings with the Date constructor (and Date.parse, they are equivalent) is strongly discouraged due to browser differences and inconsistencies.

Some browsers assume UTC, others assume local time. This is where libraries can help. 

Otherwise

```
Date.parse('01 Jan 1970 00:00:00 GMT');
```

## Formatting

Good options exist natively in newer versions of Javascript

https://stackoverflow.com/questions/3552461/how-to-format-a-javascript-date

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat

```
      const start = new Date(Date.parse(value))
      const options = { month: 'long', year: 'numeric' }
      return start.toLocaleDateString('en-US', options)
```

https://stackoverflow.com/questions/45724975/date-tolocaledatestring-is-not-a-function


## Adjustments

https://stackoverflow.com/questions/674721/how-do-i-subtract-minutes-from-a-date-in-javascript

```
var endDate = somedate;
var startdate = new Date(endDate);
var durationInMinutes = 20;
startdate.setMinutes(endDate.getMinutes() - durationInMinutes);
```

```
let stale = new Date();
stale.setMinutes(stale.getMinutes() - 5);
```

## Libraries

Both Day.js and date-fns stand out as good alternatives that should minimal impact to bundle size. 

### Date-fns

https://date-fns.org/
date-fns - modern JavaScript date utility library

https://github.com/you-dont-need/You-Dont-Need-Momentjs
you-dont-need/You-Dont-Need-Momentjs: List of functions which you can use to replace moment.js + ESLint Plugin

to use in vue

```
yarn add date-fns
```

```
import { format, parseISO } from "date-fns";

export default {
  data() {
    return {
      format,
      parseISO
    };
  },

```

### Luxon

Wraps Intl library to modernize Moment JS:

https://moment.github.io/luxon/

### Day JS

This was my second pick. API is similar to Moment JS. Not as feature complete

https://github.com/iamkun/dayjs
iamkun/dayjs: ⏰ Day.js 2KB immutable date library alternative to Moment.js with the same modern API

### Moment JS

Moment JS is a popular solution (as of 2020) for date handling in Javascript. 

However, for what it does, Moment.js is pretty big in size. A lot of that comes from locales. 

It may be possible to optimize what is included with webpack, but it is still likely to be large. 

https://www.google.com/search?client=ubuntu&channel=fs&q=will+webpack+ignore+parts+of+momentjs+not+used%3F&ie=utf-8&oe=utf-8
will webpack ignore parts of momentjs not used? - Google Search
https://github.com/jmblog/how-to-optimize-momentjs-with-webpack
jmblog/how-to-optimize-momentjs-with-webpack: Explaining how to optimize the large bundle size of moment.js with webpack

https://www.google.com/search?client=ubuntu&channel=fs&q=size+of+moment+js&ie=utf-8&oe=utf-8
size of moment js - Google Search

https://medium.jonasbandi.net/angular-cli-and-moment-js-a-recipe-for-disaster-and-how-to-fix-it-163a79180173
Angular CLI and Moment.js: A recipe for disaster … and how to fix it.
https://www.google.com/search?client=ubuntu&channel=fs&q=vue+date+filter&ie=utf-8&oe=utf-8
vue date filter - Google Search
https://forum.vuejs.org/t/how-to-format-date-for-display/3586
How to format date for display - General Discussion - Vue Forum
https://www.npmjs.com/package/vue-moment
vue-moment - npm
https://github.com/brockpetrie/vue-moment/blob/master/vue-moment.js
vue-moment/vue-moment.js at master · brockpetrie/vue-moment

## YAML

https://duckduckgo.com/?t=canonical&q=YAML+date-time&ia=web
YAML date-time at DuckDuckGo
https://stackoverflow.com/questions/2656196/ruby-how-do-i-define-a-datetime-field-in-yaml
Ruby: How do I define a datetime field in YAML? - Stack Overflow
https://yaml.org/type/timestamp.html
Timestamp Language-Independent Type for YAML™ Version 1.1
