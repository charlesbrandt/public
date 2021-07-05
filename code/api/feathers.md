# Feathers JS

[Feathers](http://feathersjs.com)
An open source web framework for building modern real-time applications.

https://docs.feathersjs.com/guides/basics/starting.html

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


## Resources / Links

https://docs.feathersjs.com/
Feathers | A framework for real-time applications and REST APIs | FeathersJS
https://docs.feathersjs.com/api/authentication/
Overview | FeathersJS
https://docs.feathersjs.com/cookbook/authentication/anonymous.html
Anonymous authentication | FeathersJS
https://docs.feathersjs.com/guides/basics/authentication.html
Authentication | FeathersJS
https://docs.feathersjs.com/guides/migrating.html#authentication
Migrating | FeathersJS


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

