# Express

A minimalist foundation for starting a web API in [Javascript](../javascript/index.md). 

https://expressjs.com/en/starter/installing.html  
Installing Express  

For debugging system configurations, it helps to spin up a known working server. "Hello world" is always a good place to start:

```js
const express = require('express')
const app = express()
const port = 3000

app.get('/', (req, res) => res.send('Hello World!'))

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
```

https://expressjs.com/en/starter/hello-world.html  
Express "Hello World" example  

## Requests 

When express receives a request, it parses the request and makes attributes available in the `req` value. 

req.params contains route parameters (in the path portion of the URL)

req.query contains the URL query parameters (after the ? in the URL).

req.body contains anything in the request body. Typically this is used on PUT and POST requests.

http://expressjs.com/en/4x/api.html#req.body

http://expressjs.com/en/4x/api.html#req.query

## Routing

Once a project grows beyond "Hello World", collect routes in various .js files. 

In the app, these can be included with:

```
var birds = require('./birds')

// ...

app.use('/birds', birds)
```

and may look something like:

```
var express = require('express')
var router = express.Router()

// middleware that is specific to this router
router.use(function timeLog (req, res, next) {
  console.log('Time: ', Date.now())
  next()
})
// define the home page route
router.get('/', function (req, res) {
  res.send('Birds home page')
})
// define the about route
router.get('/about', function (req, res) {
  res.send('About birds')
})

module.exports = router

```

It is acceptable to provide multiple functions in the route. There are different ways to format these, including as an array of functions to call:

See "Route handlers" on 

https://expressjs.com/en/guide/routing.html  
Express routing  

https://expressjs.com/en/starter/basic-routing.html  
Express basic routing  

### Nested Routes

http://katieleonard.ca/blog/2016/nested-routes-with-expressjs/

## Development

When developing an API, it can be cumbersome to have to manually restart the server every time there is a code change. Want some type of watch mode. 

Be sure to disable this when deploying the API to production.

Depending on what you use to manage the node processes, your options may vary

### Docker

Nodemon may work here

https://github.com/remy/nodemon

```
npm install --save-dev nodemon 
```

then run with 
```
npx nodemon
```

An example for `docker-compose.yml`

```
  api:
    # image: node:14
    build:
      context: ./api
      dockerfile: Dockerfile
    container_name: boilerplate_api
    volumes:
      - .:/srv/boilerplate/
      - boilerplate_api_modules:/srv/boilerplate/api/node_modules
    ports:
      - 127.0.0.1:3030:3030
    working_dir: /srv/boilerplate/api

    # development
    command: sh -c "npm install && npx nodemon boilerplate-api.js"

    # in production, no need to run with a watcher
    # command: sh -c "npm install && npm run start"
    # DEBUG=express:* node ./api/boilerplate-api.js
    # entrypoint: ["tail", "-f", "/dev/null"]
```

### PM2

PM2 watch mode



### Debugging

To see all the internal logs used in Express, set the DEBUG environment variable to express:* when launching your app.

    DEBUG=express:* node index.js

https://expressjs.com/en/guide/debugging.html

You can also use `curl` or `wget` to test specific routes that are available. 


## Logging

Morgan makes it easy to log all requests made to the service. 

https://github.com/expressjs/morgan  
GitHub - expressjs/morgan: HTTP request logger middleware for node.js  

Winston allows logging custom messages. 

https://github.com/winstonjs/winston  
GitHub - winstonjs/winston: A logger for just about everything.  

https://duckduckgo.com/?t=ffab&q=morgan+js+logger&ia=software  
💤 morgan js logger at DuckDuckGo  
https://www.npmjs.com/package/morgan  
💤 morgan - npm  
https://duckduckgo.com/?t=ffab&q=express+morgan+winston&ia=web  
💤 express morgan winston at DuckDuckGo  
https://jojozhuang.github.io/tutorial/express-combine-morgan-and-winston/  
Combine Morgan & Winston @ https://jojozhuang.github.io  
https://stackoverflow.com/questions/27906551/node-js-logging-use-morgan-and-winston  
Node.js - logging / Use morgan and winston - Stack Overflow  
http://tostring.it/2014/06/23/advanced-logging-with-nodejs/  
Advanced logging with NodeJs | Ugo Lattanzi's tech world  
https://dev.to/vassalloandrea/better-logs-for-expressjs-using-winston-and-morgan-with-typescript-516n  
Better logs for ExpressJS using Winston and Morgan with Typescript - DEV Community  


## Sessions

For more details see [Auth](auth.md)

Session management in Express

http://www.passportjs.org/  
Passport.js  

https://www.duckduckgo.com/?q=node+express+session+management  
node express session management - Google Search  
https://codeforgeek.com/manage-session-using-node-js-express-4/  
How to Manage Session using Node.js and Express - Codeforgeek  


## Links

https://www.robinwieruch.de/node-express-server-rest-api  
How to create a REST API with Express.js in Node.js - RWieruch  

## Boilerplates

Boilerplates are tricky -- it's rare to find one that sets things up just the way you want. However, they're a great way to learn about new tools and techniques, and to see where a community is headed. 

This is a nice one that focuses on **just** the server / backend:

https://github.com/sahat/hackathon-starter  
GitHub - sahat/hackathon-starter: A boilerplate for Node.js web applications  
https://github.com/sahat/hackathon-starter/blob/master/package.json  
hackathon-starter/package.json at master · sahat/hackathon-starter · GitHub  
https://github.com/sahat/hackathon-starter/blob/master/controllers/user.js  
hackathon-starter/user.js at master · sahat/hackathon-starter · GitHub  

I like the structure of this one for **both** client and server (but not sure that I agree with all of the dependencies):

https://github.com/icebob/vue-express-mongo-boilerplate  
GitHub - icebob/vue-express-mongo-boilerplate: MEVN Full stack JS web app boilerplate with NodeJS, Express, Mongo and VueJS  
https://github.com/icebob/vue-express-mongo-boilerplate/tree/master/server  
vue-express-mongo-boilerplate/server at master · icebob/vue-express-mongo-boilerplate · GitHub  

https://www.google.com/search?q=express+backend+boilerplate&client=ubuntu&hs=RfG&channel=fs&source=lnt&tbs=qdr:y&sa=X&ved=2ahUKEwjJ--b-4a7pAhVZHc0KHfkqA_gQpwV6BAgPEB0&biw=960&bih=942  
express backend boilerplate - Google Search  
https://www.google.com/search?client=ubuntu&channel=fs&biw=960&bih=942&tbs=qdr:y&q=express+passport+boilerplate&sa=X&ved=2ahUKEwjI4L6h667pAhXXZc0KHTEQCkwQ1QIoB3oECA0QCA  
express passport boilerplate - Google Search  
  
https://dev.to/sm0ke/nodejs-starter-javascript-boilerplates-to-start-fast-1024  
Nodejs Starter - Javascript Boilerplates to start fast - DEV  
https://medium.com/better-programming/best-node-js-boilerplate-to-speed-up-your-project-development-a9eca7b07f90  
The Best Node.js Boilerplate to Speed Up Your Project Development  
https://github.com/topics/express-boilerplate?o=desc&s=stars  
express-boilerplate · GitHub Topics · GitHub  
https://github.com/app-generator/nodejs-starter  
GitHub - app-generator/nodejs-starter: Nodejs Starter - Open-Source Javascript Boilerplate | AppSeed  
https://hackernoon.com/express-js-boilerplate-with-user-authentication-ch5032a3  
Express-js Boilerplate with User Authentication | Hacker Noon  
https://blog.codeminer42.com/nodejs-and-good-practices-354e7d763626/  
NodeJS and Good Practices – The Miners  
http://wiki.c2.com/?FourLayerArchitecture  
Four Layer Architecture  
https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html  
Clean Coder Blog  
  
