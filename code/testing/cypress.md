# Cypress

Tests are a form of documentation. Describe what your application should do and have a way to confirm that it still does what you think it does. Like configuration management, tests are runnable documentation. 

Run Cypress wherever you run your UI. You can still test the API by accessing the API in the same way that the UI does. 

Using containers works. Decide if you want to run it headless (CI) or with a GUI (dev). web-ui-api-db has been configured to handle either scenario. Uncomment the one you want and start it up. 

https://gitlab.com/fern-seed/web-ui-api-data

See also [docker with development](../../system/virtualization/docker-compose.md)


## Writing Tests

Once you have Cypress running, it's time to write some tests. If none exist, Cypress will scaffold out some examples for you. It can be instructive to read through those and follow along how the tests were implemented. 

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

## Checking for JS console errors on the client

check for js console error

[via](https://stackoverflow.com/questions/53898085/check-if-an-error-has-been-written-to-the-console)

Because the window is re-created with each cy.visit, Cypress recommends stubbing as a part of the cy.visit command.

```
cy.visit('/', {
  onBeforeLoad(win) {
    cy.stub(win.console, 'log').as('consoleLog')
    cy.stub(win.console, 'error').as('consoleError')
  }
})

//...
cy.get('@consoleLog').should('be.calledWith', 'Hello World!')
cy.get('@consoleError').should('be.calledOnce')

```

For more details see the official FAQ for stubbing out the console: https://docs.cypress.io/faq/questions/using-cypress-faq.html#How-do-I-spy-on-console-log

And the recipe repository: https://github.com/cypress-io/cypress-example-recipes/tree/master/examples/stubbing-spying__console




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


## Variables and Aliases

https://docs.cypress.io/guides/core-concepts/variables-and-aliases#Aliases

Define in a `beforeEach` hook with `.as('item')` and then use with `this.item`. However `this.*` won't work in arrow functions. `cy.get('@item')` is preferred:

```
cy.get('@users').then((users) => {
```

## Authentication

For more details, see [Auth Guide](/code/api/auth.md#testing)

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

WIP: Use a development only route on the API that grants valid tokens to the test client. 

This way all subsequent requests to the API act the same way any other session would. Less fiddly than using a manually created session. 

The danger here is that this route gets exposed in a production environment and becomes a security vulnerability. 

Consider placing it in a file that is set to be ignored by git. Then, can be manually place in a development environment without concern for accidentally adding it.

TODO: set up checks to ensure it is not being called? Via application monitoring. 

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

### Local

There is already a `package.json` and `node_modules` in ui

```
cd ui
pnpm i -D cypress # yarn add cypress --dev
```

will need a cypress.json file that points to the right test source locations

```
{
  "chromeWebSecurity": false,
  "supportFile": "../tests/support/index.js",
  "pluginsFile": "../tests/plugins/index.js",
  "fixturesFolder": "../tests/fixtures",
  "integrationFolder": "../tests/integration",
  "screenshotsFolder": "../tests/screenshots",
  "videosFolder": "../tests/videos",
  "downloadsFolder": "../tests/downloads"
}
```

Be sure to set the BASE_URL in the shell

export CYPRESS_BASE_URL=https://localhost
export CYPRESS_API_URL=https://localhost/api


```
npx cypress open
```

This only works with yarn, not other node package managers
```
yarn run cypress open
```


## See Also

https://docs.cypress.io/guides/getting-started/testing-your-app#Stubbing-the-server
