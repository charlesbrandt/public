# Testing

Download the stand alone version. 

https://download.cypress.io/desktop

Extract, 

open a terminal, 

cd snap/Cypress/
and run `./Cypress

I know, it's not the recommended approach. 
But I think it's actually easier when working in a dockerized environment to start here. Just get the test runner running in a browser. Then navigate to the web application however/where ever it is running. 

https://docs.cypress.io/guides/getting-started/installing-cypress#System-requirements

After getting installed, 
and once the cypress application is running,

go to a project you want to test. 

What service are you working on locally? 

If running in a container, do this step in the container: 

docker-compose exec ui bash

```
yarn add cypress
yarn run cypress open
```

TODO:
should cypress be a dev dependency or a core dependency? 
how does it get integrated in a nuxt application? 



## Cypress

That's where something like Cypress is appealing, especially if it works out of the box once it is configured.

Component Testing with Vite, Vue, and Cypress:
https://www.youtube.com/watch?v=Abwi_X107GY&t=0

So is cypress kind of like a streamlined selenium solution? Yes.
There is still a binary needed to run...
Seems to be an electron app that launches browsers to handle running the tests.


### Locally

When running locally, on Ubuntu, you may need

```
sudo apt-get install libgtk2.0-0 libgtk-3-0 libgbm-dev libnotify-dev libgconf-2-4 libnss3 libxss1 libasound2 libxtst6 xauth xvfb
```

then create a directory you want to use for cypress primarily
(anywhere -- it will use a central location for the binaries)

```
cd
mkdir cypress
cd cypress
npm init

npm install cypress --save-dev
yarn run cypress open
```

When launching cypress for the first time, it will stub out some sample tests for you in a `cypress` sub directory in your project.

Many great examples, but where to begin?

Begin by loading the site you want to test. Maybe create a file called `cypress/integration/start_here.spec.js`

```
context("Actions", () => {
  beforeEach(() => {
    cy.visit("https://duckduckgo.com/");
  });

  // https://on.cypress.io/interacting-with-elements

  it("looks at a page", () => {
  });
}),
```

https://docs.cypress.io/api/plugins/browser-launch-api#Usage

## Docker

https://www.mariedrake.com/post/using-docker-to-run-your-cypress-tests

When launching cypress in a docker container, the container may not be able to launch a browser running on the local machine. This is useful for headless tests.

If you're using docker to run your application, run it like usual.

As long as the service is available in a browser, the Cypress application / test runner will connect to it via a browser, just like you would do when testing manually.

All that Cypress needs access to is the various test specification files. It does not need to be installed along side the application. (??? TODO: confirm. Any other libraries that get injected when in development mode? TBD)

### Launch local version

The long way with the full path

    ./node_modules/.bin/cypress open

Or with the shortcut using npm bin

    $(npm bin)/cypress open

Or by using npx

note: npx is included with npm > v5.2 or can be installed separately.

    npx cypress open

Or by using yarn

    yarn run cypress open

After a moment, the Cypress Test Runner will launch.

### Docker

Running under docker is challenging...
maybe if you can forward the browser that gets launched by the container to the parent context?

Otherwise figure out how to run it locally for the purpose of testing.

Run cypress where ever you run your `node` project. Using docker there (e.g. web-ui-api-db), use docker for cypress. Running everything locally? Use cypress locally.

If you're running your projects in a docker container, then you'll want cypress in the container with the node process.

The suffix of the docker container corresponds to the version of `node`.

```
  ui:
    image: cypress/base:latest
    # if targeting a specific node version, use e.g.
    # image: cypress/base:14
```

So it's a drop in replacement for the `image: node:12` specification you may already be using.

For more details about the images:

https://github.com/cypress-io/cypress-docker-images

# 2020.07.07 16:27:14

add in testing? or is that at each individual level?? <--- the later

TODO: find documentation on this topic...
have written it before. (may still be specific to a given language)

### Cypress Links

https://duckduckgo.com/?t=canonical&q=nuxt+testing&ia=web
nuxt testing at DuckDuckGo
https://blog.logrocket.com/component-testing-in-nuxt-js/
Component testing in Nuxt.js - LogRocket Blog
https://docs.cypress.io/guides/core-concepts/test-runner#Overview
The Test Runner | Cypress Documentation
https://docs.cypress.io/guides/overview/why-cypress#Cypress-in-the-Real-World
Why Cypress? | Cypress Documentation
moz-extension://90067a19-e623-4fd1-9144-a97de15e1e62/index.html
New Tab
https://duckduckgo.com/?q=cypress+add+to+existing+project&t=canonical&ia=web
cypress add to existing project at DuckDuckGo
https://egghead.io/lessons/cypress-adding-cypress-to-an-existing-project-in-order-to-start-writing-e2e-tests
Adding cypress to an existing project in order to start writing e2e tests | egghead.io
https://duckduckgo.com/?t=canonical&q=cypress+handle+login&ia=web
cypress handle login at DuckDuckGo
https://sqa.stackexchange.com/questions/41960/is-there-a-way-to-automate-multiple-tabs-using-cypress
automated testing - Is there a way to automate multiple tabs using cypress? - Software Quality Assurance & Testing Stack Exchange
https://auth0.com/blog/end-to-end-testing-with-cypress-and-auth0/
End-to-End Testing with Cypress and Auth0
https://docs.cypress.io/guides/guides/web-security#Limitations
Web Security | Cypress Documentation
https://duckduckgo.com/?t=canonical&q=cypress+install+plugin+in+browser&ia=web
cypress install plugin in browser at DuckDuckGo
https://stackoverflow.com/questions/55400537/how-do-i-add-and-use-chrome-extensions-with-cypress-io
How do I add and use Chrome Extensions with Cypress.io? - Stack Overflow
https://docs.cypress.io/api/plugins/browser-launch-api#Usage
Browser Launch API | Cypress Documentation

