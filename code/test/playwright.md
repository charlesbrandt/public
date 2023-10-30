# Playwright

Web testing framework. Similar to Cypress. 

https://playwright.dev/  
Fast and reliable end-to-end testing for modern web apps | Playwright  

https://playwright.dev/docs/intro
Installation | Playwright

## Add to project

Or, to scaffold out some examples and settings: 

```
npm init playwright@latest
```

Edit the `playwright.config.js` file to include default settings as needed:

```
  use: {
    // All requests we send go to this endpoint.
    baseURL: 'http://192.168.1.2:3456',
    // extraHTTPHeaders: {
    //   // We set this header per GitHub guidelines.
    //   'Accept': 'application/vnd.github.v3+json',
    //   // Add authorization token to all requests.
    //   // Assuming personal access token available in the environment.
    //   'Authorization': `token ${process.env.API_TOKEN}`,
    // },
```


### Not needed? 

```
sudo npx playwright install-deps
```

This one didn't work

```
npm -i --save-dev @playwright/test
```

## VS Code

https://playwright.dev/docs/getting-started-vscode  
Getting started - VS Code | Playwright  

Nice integration with VS Code. Seems nice to have one less app open for testing purposes. 

Add the extension to VS Code

Name: Playwright Test for VSCode
Id: ms-playwright.playwright
Description: Run Playwright Test tests in Visual Studio Code.
Version: 1.0.15
Publisher: Microsoft
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=ms-playwright.playwright

### Running tests

https://playwright.dev/docs/running-tests
Running Tests | Playwright

Open the VS Code extension.

Play the `example.spec.ts` -- it should just work because it references external resources. 

Edit the example to reference your local code. (UI or API)

For example, to check for a json result in an API:

```
import { test, expect } from '@playwright/test';

test('get data', async ({ request }) => {
  const response = await request.get('/items/5?q=somequery');

  const data = await response.json()
  console.log(data)

  // Expect a title "to contain" a substring.
  await expect(data.q).toBe('somequery');
});
```


## Writing Tests

https://playwright.dev/docs/writing-tests
Writing Tests | Playwright

https://playwright.dev/docs/codegen-intro
Test Generator | Playwright
https://playwright.dev/docs/debug#browser-developer-tools
Debugging Tests | Playwright

## Docker

https://duckduckgo.com/?t=ffab&q=playwright+local+docker+&ia=web
playwright local docker at DuckDuckGo
https://playwright.dev/docs/docker
Docker | Playwright
https://playwright.dev/docs/testing-library
Migrating from Testing Library | Playwright
https://playwright.dev/docs/ci
Continuous Integration | Playwright

## Vue Template

Playwright can also be added via vue cli scaffolding system. 

https://github.com/mugi-uno/playwright-vite-vrt-sample  
mugi-uno/playwright-vite-vrt-sample: Visual Regression Test (playwright & vite)  

https://duckduckgo.com/?t=ffab&q=playwright+vite&atb=v343-1&ia=web  
playwright vite at DuckDuckGo  

## Compared to Cypress

https://duckduckgo.com/?t=ffab&q=playwright+testing+vs+cypress&ia=web  
playwright testing vs cypress at DuckDuckGo  
https://www.browserstack.com/guide/playwright-vs-cypress  
Playwright vs Cypress : Core Differences | BrowserStack  
https://medium.com/geekculture/is-playwright-better-than-cypress-playwright-vs-cypress-151bd65a224f  
Is Playwright better than Cypress? Playwright vs Cypress | by Ganesh Hegde | Geek Culture | Medium  
