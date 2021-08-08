# Cypress

Tests are a form of documentation. Like configuration management, tests are runnable documentation. 

Using containers works. Decide if you want to run it headless (CI) or with a GUI (dev). web-ui-api-db has been configured to handle either scenario. Uncomment the one you want and start it up. 

https://gitlab.com/fern-seed/web-ui-api-db

See also [docker with development](../../system/virtualization/docker-compose.md)


## Introduction

Component Testing with Vite, Vue, and Cypress:
https://www.youtube.com/watch?v=Abwi_X107GY&t=0


## Writing Tests

Once you have Cypress running (see below), it's time to write some tests. If none exist, Cypress will scaffold out some examples for you. It can be instructive to read through those and follow along how the tests were implemented. 

### cy.visit

The first step is usually to visit the site that you're testing. Do that with a `cy.visit` call:

```
    cy.visit('https://duckduckgo.com')
    cy.visit('https://google.com')
    // aha! only one main site is allowed...
    // good to know
```


TODO: next steps?


### API testing

use cy.request instead of cy.visit

https://www.mariedrake.com/post/api-testing-with-cypress

To show the json result of a cy.request() in the test runner, use console.log() to show it in the console, or `cy.api`

```

context('Network Requests', () => {
    beforeEach(() => {
      cy.visit('http://boilerplate_api_1:3030')
    })
  
  it('cy.request() with query parameters', () => {
    // will execute request
    // https://jsonplaceholder.cypress.io/comments?postId=1&id=3
    cy.request({
      url: 'http://boilerplate_api_1:3030/projects/',
    //   qs: {
    //     postId: 1,
    //     id: 3,
    //   },
    })
    .its('body')
    .should('be.an', 'array')
    .and('have.length', 16) // currently 16 items -- this will change over time
    .its('0') // yields first element of the array
    .then((element) => {
      console.log("The matching element is: ", element);
      return element;
    })
    .should('contain', {
      postId: 1,
      id: 3,
    })
  })
})
```

#### cy.api

Only try this if you have a local instance of Cypress running

https://github.com/bahmutov/cy-api

Installing this in the cypress included container is a challenge. May be better if you are running cypress directly on a local host machine. 

```
FROM cypress/included:7.7.0

WORKDIR /root/.cache/Cypress/7.7.0/Cypress/resources/app/packages/server/

RUN rm -rf /root/.cache/Cypress/7.7.0/Cypress/resources/app/packages/server/node_modules/trash

# https://www.mariedrake.com/post/api-testing-with-cypress
# https://github.com/bahmutov/cy-api
RUN npm i --D --force @bahmutov/cy-api

WORKDIR /e2e/
```

/root/.cache/Cypress/7.7.0/Cypress/resources/app/packages/server/

/root/.cache/Cypress/7.7.0/Cypress/resources/app/packages/server/node_modules/trash/node_modules/.bin/uuid

## Login

Sometimes it's possible to test the login process. If possible, this is a good idea! 

However, I have also hit some walls with some of the more advanced login systems (especially 2-factor-authentication). In these cases, transfer cookies from an existing logged in session to circumvent and test features that require an active session. 

```
// start-here.spec.js created with Cypress
//
// Start writing your Cypress tests below!
// If you're unfamiliar with how Cypress works,
// check out the link below and learn how to write your first test:
// https://on.cypress.io/writing-first-test

beforeEach(() => {
  localStorage.setItem("uid", Cypress.env("username"));
  // these need to be updated when sessions expire
  localStorage.setItem("jwt", Cypress.env("jwt"));
  localStorage.setItem("jwt_exp", Cypress.env("jwt_exp"));
});
```

as configured in a local `cypress.env.json` file

https://docs.cypress.io/guides/guides/environment-variables#Option-2-cypress-env-json

~/path-to/cypress.env.json

```
{
  "username": "account",
  "password": "",
  "jwt": "jwt-goes-here",
  "jwt_exp": "1623705332.542"
}

```


## Setup / Installation

[Cypress Installation Documentation](https://docs.cypress.io/guides/getting-started/installing-cypress#System-requirements)

### Docker

It is possible to launch Cypress from within your docker setup.

This is the guide that ultimately got me where I wanted to go:

https://www.cypress.io/blog/2019/05/02/run-cypress-with-a-single-docker-command/

On the host run `xhost local:root` so the container is allowed to connect to the local X server

via: https://github.com/cypress-io/cypress-docker-images/issues/29

> if you get this error No protocol specified you just run this in your host machine xhost local:root 


## About

That's where something like Cypress is appealing, especially if it works out of the box once it is configured.

So is cypress kind of like a streamlined selenium solution? Yes.
There is still a binary needed to run...
Seems to be an electron app that launches browsers to handle running the tests.


## Links

https://github.com/cypress-io/cypress-docker-images

https://www.cypress.io/blog/2019/05/02/run-cypress-with-a-single-docker-command/

https://mtlynch.io/painless-web-app-testing/

https://docs.cypress.io/examples/examples/docker

https://github.com/bahmutov/cypress-open-from-docker-compose

