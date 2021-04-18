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

# Extension Installation

After checking out the extension, for development you'll need to install dependencies

```
yarn
yarn install
```

## Reloadings

TODO: document
F5
Ctrl-R ? 


## Yeoman in Docker

Exploring using Yeoman under docker
not as straightforward as I'd hoped

https://www.freecodecamp.org/news/making-vscode-extension/
How to Make Your Own VS Code Extension
https://duckduckgo.com/?q=docker+node+npm+install+-g&t=canonical&ia=web
docker node npm install -g at DuckDuckGo
https://stackoverflow.com/questions/46111738/how-to-install-global-module-in-docker
npm - How to install global module in docker? - Stack Overflow
https://hub.docker.com/_/node/
node
https://github.com/nodejs/docker-node/blob/main/README.md#how-to-use-this-image
docker-node/README.md at main · nodejs/docker-node
https://duckduckgo.com/?t=canonical&q=run+yeoman+as+root+in+docker&ia=web
run yeoman as root in docker at DuckDuckGo
https://www.octobot.io/blog/2016-02-25-running-yeoman-in-a-development-instance-in-docker/
octobot.io/blog/2016-02-25-running-yeoman-in-a-development-instance-in-docker/
