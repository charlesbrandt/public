# Feathers JS

[Feathers](http://feathersjs.com)
An open source web framework for building modern real-time applications.

https://docs.feathersjs.com/guides/basics/starting.html

https://docs.feathersjs.com/
Feathers | A framework for real-time applications and REST APIs | FeathersJS

Connect to the API container to run these commands

    docker-compose exec api bash


## Container customizations

I prefer to install requirements in the container. In this case, it's helpful to have access to the cli helper:


``` api/Dockerfile
FROM node:lts

# Set the working directory.
WORKDIR /srv

RUN yarn install @feathersjs/cli -g
```


## New Application

```
feathers generate app
```

What kind of service is it? 
For an overview of the database services available, see here

https://docs.feathersjs.com/api/databases/adapters.html


## Scaffolding

Feathers has a powerful command line interface. Here are a few things it can do:

```
$ feathers generate service               # Generate a new Service
$ feathers generate hook                  # Generate a new Hook
$ feathers help                           # Show all commands
```

https://docs.feathersjs.com/guides/basics/generator.html
Generating an app | FeathersJS

## API (aka Services)

Accessing the API follows a standard pattern (which is one of the benefits of using an API framework). 

https://docs.feathersjs.com/api/services.html

### Services

Adapting the following from:
https://raw.githubusercontent.com/feathersjs/docs/crow/api/services.md

"Services" are the heart of every Feathers application. Services are JavaScript objects (or instances of [ES6 classes](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Classes)) that implement [certain methods](#service-methods). Feathers itself will also add some [additional methods and functionality](#feathers-functionality) to its services.


Service methods are pre-defined [CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete) methods that your service object can implement (or that have already been implemented by one of the [database adapters](./databases/common.md)). Below is an example of a Feathers service using [async/await](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function) as a JavaScript object and a [JavaScript or Typescript class](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Classes)


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

### Calling Methods

REST vs Realtime

With Feathers you have the option to use a ui client that they've built for easy access to the API. This is certainly an interesting feature to explore in the future. 

However, for many projects that I work on it's important to leverage the REST API, especially for getting started. 

Here is how those service methods get mapped to REST:

https://docs.feathersjs.com/api/express.html#express-rest

Service methods include




## Authentication

https://docs.feathersjs.com/api/authentication/
Overview | FeathersJS
https://docs.feathersjs.com/cookbook/authentication/anonymous.html
Anonymous authentication | FeathersJS
https://docs.feathersjs.com/guides/basics/authentication.html
Authentication | FeathersJS
https://docs.feathersjs.com/guides/migrating.html#authentication
Migrating | FeathersJS


## Resources / Links

https://duckduckgo.com/?t=ffab&q=feathers+filesystem+api&ia=web
feathers filesystem api at DuckDuckGo
https://docs.feathersjs.com/guides/basics/services.html#registering-services
Services | FeathersJS



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


## Help

For more information on all the things you can do with Feathers visit [docs.feathersjs.com](http://docs.feathersjs.com).

