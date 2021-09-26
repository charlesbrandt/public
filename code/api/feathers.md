# Feathers JS

[Feathers](http://feathersjs.com)
An open source web framework for building modern real-time applications and REST APIs.

[docs.feathersjs.com](http://docs.feathersjs.com).

https://docs.feathersjs.com/guides/basics/starting.html


## New Application

```
feathers generate app
```


What kind of service is it? 
For an overview of the database services available, see here

https://docs.feathersjs.com/api/databases/adapters.html


## Scaffolding

Feathers has a command line interface for generating scaffolding. Here are a few things it can do:

```
$ feathers generate service               # Generate a new Service
$ feathers generate hook                  # Generate a new Hook
$ feathers help                           # Show all commands
```


https://docs.feathersjs.com/guides/basics/generator.html
Generating an app | FeathersJS


## API (aka Services)

Feathers makes it a snap to set up CRUD (Create, Read, Update, Delete) style APIs. Accessing the API follows a standard pattern (which is one of the benefits of using an API framework). 

https://docs.feathersjs.com/api/services.html

### Services

"Services" are the heart of every Feathers application. Services are JavaScript objects (or instances of [ES6 classes](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Classes)) that implement [certain methods](#service-methods). Feathers itself will also add some [additional methods and functionality](#feathers-functionality) to its services.


Service methods are pre-defined [CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete) methods that your service object can implement (or that have already been implemented by one of the database adapters. Below is an example of a Feathers service using [async/await](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function) as a JavaScript object and a [JavaScript or Typescript class](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Classes)


```
const myService = {
  async find(params) {
    return [];
  },
  async get(id, params) {},
  async create(data, params) {},
  async update(id, data, params) {},
  async patch(id, data, params) {},
  async remove(id, params) {},
  setup(app, path) {}
}

app.use('/my-service', myService);
```


Service methods must use [async/await](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function) or return a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) and have the following parameters:

- `id` — The identifier for the resource. A resource is the data identified by a unique id.
- `data` — The resource data.
- `params` - Additional parameters for the method call (see [params](#params))

> **Important:** This section describes the general usage of service methods and how to implement them. They are already implemented by the official Feathers database adapters. For specifics on how to use the database adapters, see the [database adapters common API](./databases/common.md).

https://github.com/feathersjs/docs/blob/crow/api/databases/common.md

[Section adapted from](https://raw.githubusercontent.com/feathersjs/docs/crow/api/services.md)

## Database / Models / Persistence

Decide how you want to store your data and then set up models accordingly. 

would it help to use the API tests to test these? 
TODO: is there a way to use Cypress at that level? e.g. instantiate ORM instance and make calls directly to it. (or is that what the binding provides?)

Sequelize

Mongo

If you need to work with the underlying system directly, you can always define your own CRUD actions in a service. 


## Hooks

Important for triggering necessary actions for a given Service. 

I needed to use hooks to look up associated elements when returning results from a FIND or a GET. 


## Calling Methods

Once you have an API configured and running, it helps to be able to call it from the clients you use. Clients vary wildly -- from web browsers to CLI scripts.

### REST

For many projects that I work on it's important to leverage the REST API.

https://docs.feathersjs.com/api/express.html#express-rest

Calls get mapped map as follows

.find()  	GET 	/item
.create() 	POST 	/item
.get()  	GET 	/item/1
.update() 	PUT 	/item/1
.patch() 	PATCH 	/item/1
.remove() 	DELETE 	/item/1

### Feathers Client

https://docs.feathersjs.com/api/client.html

With Feathers you have the option to use a client that they've built for easy access to the API. 

Set this up to be available in the api container? or just install as part of API application? 


npm install @feathersjs/feathers @feathersjs/socketio-client socket.io-client --save

yarn add -D @feathersjs/feathers @feathersjs/socketio-client socket.io-client

Note, as of 2021.08, @feathersjs/socketio-client requires socket.io-client "^2.3.1"

yarn add -D @feathersjs/feathers @feathersjs/socketio-client socket.io-client@2.3.1

following along and creating a script works in this context


#### Querying

https://docs.feathersjs.com/api/databases/querying.html

```
      this.$axios
        // https://docs.feathersjs.com/api/databases/querying.html#limit
        // when pagination is enabled, data will be empty
        // .get(this.$config.apiUrl + 'project?$limit=0')
        // if you want everything, disable pagination on the api
        .get(this.$config.apiUrl + 'items')
        .then((res) => {
          console.log('Response from server:', res.data)
          // if pagination enabled, data will be in a parameter of response
          // this.items = res.data.data
          this.items = res.data
        
```

## Error Handling

It's important to catch errors. 

Ideally these are caught with every promise call. 

It is also possible to configure the server to catch unhandled exceptions:

https://docs.feathersjs.com/api/errors.html#error-handling

If you don't see the output you expect in your server logs, be sure these hooks are configured correctly. 

https://docs.feathersjs.com/api/errors.html#server-side-errors
Errors | FeathersJS



https://duckduckgo.com/?t=ffab&q=UnhandledPromiseRejectionWarning%3A+GeneralError%3A+Unknown+column&ia=web
💤 UnhandledPromiseRejectionWarning: GeneralError: Unknown column at DuckDuckGo
https://stackoverflow.com/questions/65570523/unhandledpromiserejectionwarning-sequelizedatabaseerror-unknown-column-model
💤 node.js - UnhandledPromiseRejectionWarning: SequelizeDatabaseError: Unknown column 'model.coulmn_name' in 'where clause' - Stack Overflow



## Authentication

https://docs.feathersjs.com/api/authentication/
Overview | FeathersJS
https://docs.feathersjs.com/cookbook/authentication/anonymous.html
Anonymous authentication | FeathersJS
https://docs.feathersjs.com/guides/basics/authentication.html
Authentication | FeathersJS
https://docs.feathersjs.com/guides/migrating.html#authentication
Migrating | FeathersJS


https://docs.feathersjs.com/guides/basics/authentication.html#get-a-token

```
context("Network Requests", () => {
  beforeEach(() => {
    cy.request("POST", "/authentication", {
      strategy: "local",
      email: "test@test.com",
      password: "password",
    }).then((response) => {
      cy.wrap(response).as("jwtresponse");
      // window.localStorage.setItem("jwt", response.body.accessToken);
      console.log("The POST response was: ", response);
      // return response;
    });
  });

  it("cy.request() with query parameters", () => {
    // will execute request
    // https://jsonplaceholder.cypress.io/comments?postId=1&id=3
    cy.get("@jwtresponse").then((jwtresponse) => {
      const jwt = "Bearer " + jwtresponse.body.accessToken;
      console.log("Still have jwt?", jwt);
      cy.request({
        url: "/posts/1",
        headers: { Authorization: jwt },
        //   qs: {
        //     postId: 1,
        //     id: 3,
        //   },
      })
        .then((response) => {
          console.log("The response was: ", response);
          return response;
        })
        .its("body")
        .should("be.have", "comments")
        .and("have.length", 16) 
        .its("0") // yields first element of the array
        .should("contain", {
          postId: 1,
          id: 3,
        });
    });
  });
});
```

## Getting Started

Getting up and running is as easy as 1, 2, 3.

1. Make sure you have [NodeJS](https://nodejs.org/) and [yarn](https://yarnpkg.com/) installed.
2. Install your dependencies

    ```
    cd path/to/boilerplate-api
    yarn install
    ```

3. Start your app

    ```
    yarn start
    ```


## Testing

Simply run `yarn test` and all your tests in the `test/` directory will be run.


## Access

after `yarn install` and `node index.js` in the container, should be able to go:

https://localhost:8888/api/people



## Resources / Links

https://duckduckgo.com/?t=ffab&q=feathers+filesystem+api&ia=web
feathers filesystem api at DuckDuckGo
https://docs.feathersjs.com/guides/basics/services.html#registering-services
Services | FeathersJS



## Help


