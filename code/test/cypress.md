# Cypress

Tests are a form of documentation. Describe what your application should do and have a way to confirm that it still does what you think it does. Like configuration management, tests are runnable documentation. 

Run Cypress wherever you run your UI. You can still test the API by accessing the API in the same way that the UI does. 

Using containers works. Decide if you want to run it headless (CI) or with a GUI (dev). web-ui-api-db has been configured to handle either scenario. Uncomment the one you want and start it up. 

https://gitlab.com/fern-seed/web-ui-api-data

See also [docker with development](../../system/virtualization/docker-compose.md)


## Making Calls

Once you have [Cypress running](cypress.md#setup-installation), it's time to write some tests. If none exist, Cypress will scaffold out some examples for you. It can be instructive to read through those and follow along how the tests were implemented. 

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

https://docs.cypress.io/api/commands/request

https://www.mariedrake.com/post/api-testing-with-cypress

To show the json result of a cy.request() in the test runner, use console.log() and find the result in the test browser's console.

```js
context('API Requests', () => {
    beforeEach(() => {
      cy.visit('http://boilerplate_api_1:3030')
    cy.request(Cypress.env("API_URL") + "/health")
      .then((response) => {
        console.log("The API health status is: ", response);
        return response;
      })
      .its("body")
      .should("be.an", "object")
      .should("contain", {
        status: "ok",
        mode: "development",
      });
      
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

The associated JSON objects should be returned as part of a GET request. Use them to check for an expected attribute to exist.


### Base Urls

Cypress how to handle two baseUrls for different types of tests (e.g. UI & API tests)

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

## Writing Tests

Now that the resource has been loaded, it's time to write some tests.

### Naming Conventions

Start the name of the spec to match the route being tested. Sometimes the route name is the same for the UI and the API, so it helps to append `ui` or `api` accordingly. I also like to specify if the tests are using authentication with `auth`. So a complete example could be: `route.api.auth.spec.js`

Within a test file, tests can be grouped with `context` or `describe` functions. These do the same thing; personal preference which you use. In the statement, I like to mimic the filename (consider: just use the filename?) so it's easier to identify the source of failing tests (when running more than one):

```
context("Import CSV data via the UI as an authenticated user", () => {
```

### Checks & Assertions

Visiting a page or requesting an object is a good test in and of itself. From there it often makes sense to confirm that certain attributes or elements exist on the response content. 

Most of these calls are chainable. 

```
cy.get('.main')
``` 

will get page elements with class `.main`


```
cy.contains("Hello World")
``` 

Will look for content with a matching string


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


### Actions

Once you have an element, you can simulate taking actions on it:

`.click()` and `.type()` are probably the two most common. 

There is also `.clear()`, `.check()` and `.select()`

https://docs.cypress.io/guides/core-concepts/introduction-to-cypress#Interacting-With-Elements


### Variables and Aliases

https://docs.cypress.io/guides/core-concepts/variables-and-aliases#Aliases

Define in a `beforeEach` hook with `.as('item')` and then use with `this.item`. However `this.*` won't work in arrow functions. `cy.get('@item')` is preferred:

```
cy.get('@users').then((users) => {
```



## Authentication

For related topics, see [Auth Guide](/code/api/auth.md#testing)

Handling authentication is one of the trickier parts of testing. 

Ideally, you can run through the authentication process once using the UI and then keep the results (Cookies, LocalStorage, etc) for future requests that need to be authenticated. 

``` js
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

The above example works with a [Feathers API](/code/api/feathers.md)


### Create a special route

If the UI authentication uses an external service that is difficult to automate (e.g. 2FA), it may be necessary to create a shortcut on the API.

This is a development only route on the API that grants valid tokens to the test client. 

This way, all subsequent requests to the API act the same way any other session would. 

The danger here is that this route gets exposed in a production environment and becomes a security vulnerability. 

TODO: set up checks to ensure it is not being called? Via application monitoring. 

```js
// Avoid commiting to version control
// TODO: way to test if deploying to prod in development mode?
router.get("/verify-test", function (req, res, next) {
  console.log("request parameters", req.query.user);
  if (config.site.mode === "development") {
    db.user
      .findOne({
        where: { username: req.query.user },
      })
      .then(async (user) => {
        if (user) {
          common.issue_jwt(user, user.id, function (err, jwt, exp) {
            if (err) return next(err);
            console.log("issued token", jwt);
            res.json({
              jwt: jwt,
              uid: req.query.user,
              user_id: user.id,
              role: user.primary_role,
              roles: JSON.parse(user.roles),
              jwt_exp: exp,
            });
          });
          console.log("USER FOUND");
        } else {
          logger.error("No user matched");
          res.sendStatus("403"); //Is 403:Forbidden appropriate return code?
        }
      })
      .catch(function (thrown) {
        console.log("Error creating test token");
        return next(thrown);
      });
  }
});
```

Then use it in tests with:

```js
  it("POSTS a new user after authenticating first", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      // Sometimes may need a prefix
      // const jwt = "Bearer " + jwtresponse.body.jwt;
      const jwt = jwtresponse.body.jwt;
      console.log("Still have jwt?", jwt);
      cy.request({
        method: "POST",
        headers: { Authorization: jwt },
        url: Cypress.env("API_URL") + "/user/",
        // failOnStatusCode: false,
        body: {
          name: "Test User",
          email: "tester@example.com",
          role: "admin",
        },
      }).then((response) => {
        // expect(response.status).to.eq(403);
        console.log("The POST response was: ", response);
        return response;
      });
    });
  });
```

TODO:  
Ways to mitigate risk of an accidental commit.  
Consider placing it in a file that is set to be ignored by git. Then, can be manually place in a development environment without concern for accidentally adding it.  
Don't want to import a file that doesn't exist. Still have to worry about import statement getting committed to version control in this scenario.

### Existing sessions

Cypress uses stored cookie / localstorage value for running tests

Though not a recommended approach, this works if a valid login is initiated elsewhere: 

```js
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

[The alternative is to bypass a global Single Sign On (SSO) identity provider for your organization and just use the local hooks in your application to provide an authenticated session for testing. (However, be sure these mechanisms are not available in production!)](#create-a-special-route)

### Make sure a route is protected

It's a good idea to confirm that a route is set to block unauthenticated responses

```js
  it("POSTS a new user without prior auth and fails", () => {
    cy.request({
      method: "POST",
      url: Cypress.env("API_URL") + "/user/",
      failOnStatusCode: false,
      body: {
        name: "Test User",
        email: "tester@example.org",
        role: "admin",
      },
    }).then(response => {
      expect(response.status).to.eq(403);
      console.log("The POST response was: ", response);
      return response;
    });
  });

```

https://stackoverflow.com/questions/66727106/cypress-cy-request-expect-to-throw-fail


## File Uploads

I like the approach outlined in:
https://stackoverflow.com/questions/54889492/how-to-test-file-upload-functionality-in-cypress

Define an `uploadFile()` command:

``` js
Cypress.Commands.add(
  "uploadFile",
  (fileNamePath, fileName, fileType = " ", selector) => {
    cy.get(selector).then((subject) => {
      cy.fixture(fileNamePath, "base64")
        .then(Cypress.Blob.base64StringToBlob)
        .then((blob) => {
          const el = subject[0];
          const testFile = new File([blob], fileName, {
            type: fileType,
          });
          const dataTransfer = new DataTransfer();
          dataTransfer.items.add(testFile);
          el.files = dataTransfer.files;
        });
    });
  }
);
```

Then use it with

``` js
    cy.uploadFile(
      "example.csv",
      "example.csv",
      "text/csv",
      ".custom-file-input"
    );
    // seems like the uploadFile would trigger this, but it didn't
    cy.get(".custom-file-input").trigger("change");
```

For a complete example of handling file uploads in the UI with Vue, see [CSV Files](/code/javascript/csv.md#cypress-testing-template)

References

https://docs.cypress.io/api/commands/fixture#Syntax  
fixture | Cypress Documentation  
https://docs.cypress.io/api/utilities/blob#Syntax  
Cypress.Blob | Cypress Documentation  
https://docs.cypress.io/api/commands/trigger#Syntax  
trigger | Cypress Documentation  


I prefer the local function approach over the often cited: https://www.npmjs.com/package/cypress-file-upload -- it should be more portable


## Testing Single File Components (WIP)

Component Testing with Vite, Vue, and Cypress:  
https://www.youtube.com/watch?v=Abwi_X107GY&t=0

28:15 

TODO: functional example template? Any existing?

https://docs.cypress.io/guides/component-testing/framework-configuration#Vite-Based-Projects-Vue-React

Vue 3 & Vite

```
yarn add cypress @cypress/vue@next @cypress/vite-dev-server --dev
```

Should be run where ever you run your development server. 

Component testing is configured as a Cypress plugin. This means you need to create a plugins file. e.g. `tests/plugins/index.js`. 

The plugin file needs to be in the same path as the `ui/node_modules` directory, otherwise it won't be able to find the dependencies. e.g.

```
 Error: Cannot find module '@cypress/vite-dev-server'
```

In the plugins file, register the `dev-server:start` event

``` js
const path = require('path')
const { startDevServer } = require('@cypress/vite-dev-server')

module.exports = (on, config) => {
  on('dev-server:start', (options) => {
    return startDevServer({
      options,
      viteConfig: {
        configFile: path.resolve(__dirname, '..', '..', 'vite.config.js'),
      },
    })
  })
}
```

Finally, tell Cypress where and how to find our tests via cypress.json. 

``` json
{
  "component": {
    "componentFolder": "src",
    "testFiles": "**/*.spec.js"
  }
}
```

What is it relative to? cypress.json?

Create a test file next to the component:

``` js
import { mount } from '@cypress/vue'
import HelloWorld from './hello-world.vue'

describe('HelloWorld', () => {
  it('renders a message', () => {
    const msg = 'Hello Cypress Component Testing!'
    mount(HelloWorld, {
      propsData: {
        msg
      }
    })

    cy.get('h1').should('have.text', msg)
  })
})
```

Run the Component Test Runner (different than the Test Runner)

```
yarn cypress open-ct
```

Original draft for documentation?

https://www.cypress.io/blog/2021/04/06/getting-start-with-cypress-component-testing-vue-2-3/







## Setup / Installation

[Cypress Installation Documentation](https://docs.cypress.io/guides/getting-started/installing-cypress#System-requirements)

### Local

Even if your application is running in a container, you can still run Cypress locally. 

There is already a `package.json` and `node_modules` in ui

```
cd ui
pnpm i -D cypress # yarn add cypress --dev
```

will need a cypress.json file that points to the right test source locations

```
{
  "chromeWebSecurity": false,
  "supportFile": "tests/support/index.js",
  "pluginsFile": "tests/plugins/index.js",
  "fixturesFolder": "tests/fixtures",
  "integrationFolder": "tests/integration",
  "screenshotsFolder": "tests/screenshots",
  "videosFolder": "tests/videos",
  "downloadsFolder": "tests/downloads"
}
```

I've started keeping the tests in the same directory as the ui code. This way cypress can leverage the UI's `node_modules` directory. 

Be sure to set the BASE_URL in the shell

```
export CYPRESS_BASE_URL=https://localhost
export CYPRESS_API_URL=https://localhost/api
```

Then run

```
npx cypress open
```

This only works with yarn, not other node package managers, so I recommend the npx approach.

```
yarn run cypress open
```


### Docker

It is possible to launch Cypress from within your docker setup.

This is the guide that ultimately enabled me to get this working:

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

## Ideas / Habits

Consider leaving each day with a broken test  
it's like having something on your todo list  
the next day -- fix the bug  



## Example tests

For making authenticated API calls, get the jwt first:

```js
/// <reference types="cypress" />

context("Authenticated API Tests", () => {
  beforeEach(() => {
    cy.request(Cypress.env("API_URL") + "/verify-test?user=boilerplate-user").then(
      (response) => {
        cy.wrap(response).as("jwtresponse");
        // window.localStorage.setItem("jwt", response.body.accessToken);
        console.log("The VERIFY response was: ", response);
        // return response;
      }
    );
  });

  it("POSTS a new user after authenticating first", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      // Sometimes may need a prefix
      // const jwt = "Bearer " + jwtresponse.body.jwt;
      const jwt = jwtresponse.body.jwt;
      console.log("Still have jwt?", jwt);
      cy.request({
        method: "POST",
        headers: { Authorization: jwt },
        url: Cypress.env("API_URL") + "/user/",
        // failOnStatusCode: false,
        body: {
          fullname: "Test User",
          email: "tester@boilerplate.com",
          role: "admin",
        },
      })
        .then((response) => {
          // expect(response.status).to.eq(403);
          console.log("The POST response was: ", response);
          return response;
        })
        .its("body")
        .should("contain", {
          fullname: "Test User",
        });
    });
  });

  // should be protected
  it("GETS some user objects", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      // will execute request
      // https://jsonplaceholder.cypress.io/comments?postId=1&id=3
      const jwt = jwtresponse.body.jwt;
      cy.request({
        url: Cypress.env("API_URL") + "/user/",
        headers: { Authorization: jwt },
        //   qs: {
        //     postId: 1,
        //     id: 3,
        //   },
      })
        .then((response) => {
          console.log("The matching GET response is: ", response);
          return response;
        })
        .its("body")
        .should("be.an", "array")
        .and("have.length", 1) // current count -- this will change over time
        .its("0") // yields first element of the array
        .should("contain", {
          username: "boilerplate-user",
        });
    });
  });

  it("GETS one existing user", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      const jwt = jwtresponse.body.jwt;
      cy.request({
        method: "POST",
        headers: { Authorization: jwt },
        url: Cypress.env("API_URL") + "/user/search",
        body: {
          username: "tester",
        },
      })
        .its("body")
        .then((element) => {
          console.log("The matching element is: ", element);

          cy.request({
            method: "GET",
            headers: { Authorization: jwt },
            url: Cypress.env("API_URL") + "/user/" + element[0].id,
          })
            .then((response) => {
              // TODO check for expected data
              console.log("The user GET response was: ", response);
              return response;
            })
            .its("body")
            .should("contain", {
              username: "tester",
            });
        });
    });
  });

  it("UPDATES one existing user", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      const jwt = jwtresponse.body.jwt;
      cy.request({
        method: "POST",
        headers: { Authorization: jwt },
        url: Cypress.env("API_URL") + "/user/search",
        body: {
          username: "tester",
        },
      })
        .its("body")
        .then((element) => {
          console.log("The matching element for UPDATE is: ", element);

          cy.request({
            method: "PATCH",
            headers: { Authorization: jwt },
            url: Cypress.env("API_URL") + "/user/" + element[0].id,
            body: {
              fullname: "Updated Test User",
            },
          })
            .then((response) => {
              // TODO check for expected data
              console.log("The user GET response was: ", response);
              return response;
            })
            .its("body")
            .should("contain", {
              fullname: "Updated Test User",
            });
        });
    });
  });

  it("DELETES an existing user", () => {
    cy.get("@jwtresponse").then((jwtresponse) => {
      const jwt = jwtresponse.body.jwt;
      cy.request({
        method: "POST",
        headers: { Authorization: jwt },
        url: Cypress.env("API_URL") + "/user/search",
        body: {
          username: "tester",
        },
      })
        .its("body")
        .then((element) => {
          console.log("The matching element is: ", element);

          cy.request({
            method: "DELETE",
            headers: { Authorization: jwt },
            url: Cypress.env("API_URL") + "/user/" + element[0].id,
          }).then((response) => {
            console.log("The DELETE response was: ", response);
            return response;
          });
        });
    });
  });
});

```
