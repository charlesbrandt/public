# Cypress

Tests are a form of documentation. Describe what your application does and have a way to confirm that it still does what you think it does. Like configuration management, tests are runnable documentation. 

Using containers works. Decide if you want to run it headless (CI) or with a GUI (dev). web-ui-api-db has been configured to handle either scenario. Uncomment the one you want and start it up. 

https://gitlab.com/fern-seed/web-ui-api-data

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

From here use `cy.get` or `cy.find` to locate elements on the page, and check for content using the `.should()` function. 


### cy.request 

For API testing use cy.request instead of cy.visit

https://www.mariedrake.com/post/api-testing-with-cypress

To show the json result of a cy.request() in the test runner, use console.log() and find the result in the test browser's console, or use `cy.api`

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

The associated objects should be returned as part of a GET request (as seen in API tests), so be sure to check for their existence too. 

#### cy.api

`cy.api` can help by displaying json results in the cypress test browser (instead of using `console.log()` calls. Only try this if you have a local instance of Cypress running; my last attempt at installing in the cypress included docker container was a challenge. 

https://github.com/bahmutov/cy-api

## Base Urls

If you're not testing your UI and API with a proxy web server that normalizes the base URL (Testing UI directly may eliminate a noticeable refresh from HMR...), then it may be necessary to handle

cypress how to handle two baseUrls for different types of tests


define CYPRESS_API_URL in the environment

```
    environment:
      - CYPRESS_API_URL=http://phi_mdd_api_1:3030
```

Then utilize it explicitly in API tests

```
    cy.request({
      url: Cypress.env("API_URL") + "/datafile",
    })
```





## Assertions

Visiting a page or requesting an object is a good test in and of itself. From there it may make sense to confirm that certain attributes or elements exist on the response. 

Rather than check for existence with something like:

```
      .should("contain", "data")
      .and("have.key", "data") // not sure that 'have.key' is even a real thing!
```

Just use `.its()` to yield the item

```
.its("data") // yields the object with key 'data'
```

`.should("contain", ...` is better when you want to check for specific values

```
      .should("contain", {
        project_name: "Super Cool",
        name: "Awesome Sauce",
      })
```

https://docs.cypress.io/guides/core-concepts/introduction-to-cypress#List-of-Assertions


## Authentication

Handling authentication is one of the trickier parts of testing. 

Ideally, you can run through the authentication process once using the UI and then keep the results (Cookies, LocalStorage, etc) for future requests that need to be authenticated. 

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

~/public/code/api/feathers.md

### Existing sessions

Cypress uses stored cookie / localstorage value for running tests

Though not a recommended approach, this works if a valid login is initiated elsewhere: 

```
beforeEach(() => {
  localStorage.setItem("uid", Cypress.env("username"));
  localStorage.setItem("roles", Cypress.env("roles"));
  localStorage.setItem("role", Cypress.env("role"));
  // these need to be updated when sessions expire
  localStorage.setItem("jwt", Cypress.env("jwt"));
  localStorage.setItem("jwt_exp", Cypress.env("jwt_exp"));
});
```

For some challenging systems with lots of iframes and 2FA, this may be as good as it gets if you need a real authenticated session. 

The alternative is to bypass a global Single Sign On (SSO) identity provider for your organization and just use the local hooks in your application to provide an authenticated session for testing. (However, be sure these mechanisms are not available in production!)


## Setup / Installation

[Cypress Installation Documentation](https://docs.cypress.io/guides/getting-started/installing-cypress#System-requirements)

### Docker

It is possible to launch Cypress from within your docker setup.

This is the guide that ultimately got me where I wanted to go:

https://www.cypress.io/blog/2019/05/02/run-cypress-with-a-single-docker-command/

On the host run `xhost local:root` so the container is allowed to connect to the local X server

via: https://github.com/cypress-io/cypress-docker-images/issues/29

> if you get this error No protocol specified you just run this in your host machine xhost local:root 

#### Docker Setup Resources

https://github.com/cypress-io/cypress-docker-images

https://mtlynch.io/painless-web-app-testing/

https://docs.cypress.io/examples/examples/docker

https://github.com/bahmutov/cypress-open-from-docker-compose


## See Also

https://docs.cypress.io/guides/getting-started/testing-your-app#Stubbing-the-server
