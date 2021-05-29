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

It is possible to launch Cypress from within your docker setup if that helps with access / repeatability.

This is the guide that ultimately got me where I wanted to go:

https://www.cypress.io/blog/2019/05/02/run-cypress-with-a-single-docker-command/



On the host you may need to run `xhost local:root` so the container is allowed to connect to the local X server

via: https://github.com/cypress-io/cypress-docker-images/issues/29

> if you get this error No protocol specified you just run this in your host machine xhost local:root 


#### Links

Other helpful resources

https://docs.cypress.io/guides/getting-started/installing-cypress#System-requirements
Installing Cypress | Cypress Documentation
https://github.com/cypress-io/cypress-docker-images
cypress-io/cypress-docker-images: Docker images with Cypress dependencies and browsers
https://github.com/cypress-io/cypress-docker-images/tree/master/included
cypress-docker-images/included at master · cypress-io/cypress-docker-images
https://docs.cypress.io/examples/examples/docker#Images
Docker | Cypress Documentation
https://github.com/cypress-io/cypress-example-docker-compose-included
cypress-io/cypress-example-docker-compose-included: Cypress example with docker-compose and cypress/included image
https://github.com/bahmutov/cypress-open-from-docker-compose
bahmutov/cypress-open-from-docker-compose: A simple example of using Cypress.io for end-to-end testing
https://mtlynch.io/painless-web-app-testing/
End-to-End Testing Web Apps: The Painless Way · mtlynch.io
