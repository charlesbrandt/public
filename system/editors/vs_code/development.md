# Extension Development

Generate a boilerplate for a new extension. 

https://code.visualstudio.com/api/get-started/your-first-extension
Your First Extension | Visual Studio Code Extension API

https://code.visualstudio.com/api
Extension API | Visual Studio Code Extension API

## Scaffolding with Yeoman

Currently this requires yeoman. If you don't have that package installed, it can take some time to install. 


Any way to do this with `npx`?

https://duckduckgo.com/?t=canonical&q=npx+vscode+create+extension&ia=web
npx vscode create extension at DuckDuckGo

Didn't see an obvious solution. 


Once you've created a new extension, add a new function. 

** Be sure to add the new function in all of the many places needed to register ** (documentation didn't mention this that we saw. Supposed to be self evident?)

  - TODO: confirm
  - 2 x in package.json
  - in file directly
  - others?

## Extension Installation

After checking out the extension, for development you'll need to install dependencies

```
yarn
yarn install
```

## Reloadings

TODO: document
F5
Ctrl-R ? 


