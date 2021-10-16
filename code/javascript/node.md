# Node

https://nodejs.org/en/  
https://github.com/nodejs/node  

Node.js® is a JavaScript runtime built on Chrome's V8 JavaScript engine.

## Install

### Docker

[Docker containers](../../system/virtualization/docker.md) are a great way to ensure you're running the node environment that you think you're running. 

Containers for Node applications are maintained here

https://hub.docker.com/_/node/

In `docker-compose.yml`, this is a good place to start

```
image: node:lts
```

If you're using a container that does not have Node installed (e.g. Centos), installing from nodesource.com seems like the best option

```
RUN curl -sL https://rpm.nodesource.com/setup_10.x | bash # for node version 10.x
RUN yum -y install nodejs
RUN node --version # optional to check that it worked
RUN npm --version # optional to check that it worked
```

NVM is an alternative, but it's tricky to use NVM in a container:

```
# nvm environment variables
ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 12.16.2

# Install NVM for installing node
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash

RUN source $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

```


#### NPM Packages & Docker

May be possible to minimize the number of npm packages pulled down during an image build:

https://itnext.io/npm-install-with-cache-in-docker-4bb85283fa12

Looks like Seth has another tactic for this here:

https://github.com/City-of-Bloomington/myBloomington/blob/master/Dockerfile


### NVM

Node Version Manager - Simple bash script to manage multiple active node.js versions
https://github.com/creationix/nvm

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash

Open a new shell and verify with:

    command -v nvm

Clean up old (non-nvm) node version(s):

    npm ls -gp --depth=0 | awk -F/ '/node_modules/ && !/\/npm$/ {print $NF}' | sudo xargs npm -g rm

Install Node via NVM:

    nvm install node

via:
http://stackoverflow.com/a/24404451/1480391

### Nodesource

Nodesource is another popular way to install node

https://github.com/nodesource/distributions/blob/master/README.md

## Modules

Make sure you've actually installed the module if you get a message like `MODULE_NOT_FOUND`

https://nodejs.org/api/modules.html#modules_all_together

https://gist.github.com/MattGoldwater/78f89ea93b9f1dfc19d3440e172cfa49

https://stackoverflow.com/questions/9023672/how-do-i-resolve-cannot-find-module-error-using-node-js

### Import vs Require

ES6 modules use the import syntax. Seems like the way to go moving forward.

https://stackoverflow.com/questions/46677752/the-difference-between-requirex-and-import-x  
node.js - The difference between "require(x)" and "import x" - Stack Overflow  

https://stackoverflow.com/questions/31354559/using-node-js-require-vs-es6-import-export  
javascript - Using Node.js require vs. ES6 import/export - Stack Overflow  

https://duckduckgo.com/?t=ffab&q=javascript+require+vs+import&ia=web  
javascript require vs import at DuckDuckGo  

## Local Development

https://duckduckgo.com/?t=ffab&q=work+on+npm+module+in+development&ia=web  
work on npm module in development at DuckDuckGo  
https://nodesource.com/blog/an-absolute-beginners-guide-to-using-npm/  
An Absolute Beginner's Guide to Using npm - NodeSource  
https://www.deadcoderising.com/how-to-smoothly-develop-node-modules-locally-using-npm-link/  
How to smoothly develop node modules locally using npm link  


## Package Managers

Package managers ensure that all of the modules that your application depends on are compatible and available to the local code base.

### Types of dependencies

https://classic.yarnpkg.com/en/docs/dependency-types#toc-dev-dependencies

> Dependencies serve many different purposes. Some dependencies are needed to build your project, others are needed when you’re running your program. As such there are a number of different types of dependencies that you can have (e.g. dependencies, devDependencies, and peerDependencies).


### Choosing a package manager

NPM vs Yarn vs PNPM

The answer? Use what your team is using. Be consistent there. Working on an open project? Use what the project is using.

DIY? Flip a coin.

All are good. No need to get hung up here.

### Yarn

It should be available in most Node containers.

Once you have `yarn` available, you can add packages as a requirement with:

    yarn add <name>

or as a dev dependency with:

    yarn add <package...> [--dev/-D]

https://classic.yarnpkg.com/en/docs/cli/add/  
https://classic.yarnpkg.com/en/docs/cli/add/#toc-yarn-add-dev-d  

To install a package globally, the order of the parameters is important:

    yarn global <add/bin/list/remove/upgrade> [--prefix]
    
To remove a dependency

     yarn remove <name>
     
Note: manually deleting from `package.json` removes the dependency from the project, but will not remove the files from `node_modules` of the local instance.

#### Reinstall Modules

Reinstalling a package after just deleting the node module works with:

    yarn install --check-files

[via](https://stackoverflow.com/questions/41864099/how-do-i-force-yarn-to-reinstall-a-package)

#### Install / update yarn:

    npm install -g yarn

or use a system level package manager

    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -

    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

then, with apt-get:

    sudo apt-get install --no-install-recommends yarn

via https://yarnpkg.com/lang/en/docs/install/

### Npm

NPM is the default package manager for Node.

Install everything as configured in package.json file

    npm install

Calls to

    npm install --save [package name]

or

    npm install --save-dev [package name]

or

    npm install --save-optional [package name]

will update the package.json to list your dependencies.

To remove a dependency:

     npm uninstall <name> --save

https://stackoverflow.com/questions/13066532/how-to-uninstall-npm-modules-in-node-js

## Lock files

Is it really a good idea to track lock files under version control?
Seems like it adds a lot of noise to the process

Yes, it can add noise if you manage it manually. However, without it you may run into situations where deployments to a different environment may not work due to different versions of modules being installed. It's better to test with a set of locked versions. In that case, lock files are necessary.

### Resolve yarn.lock conflicts

It's as easy as running `yarn` or `yarn install` to automatically resolve version control conflicts in a yarn.lock file. Sweet!

### Resolving package-lock.json conflicts

Occasionally, two separate calls to `npm install` will create package locks that cause merge conflicts in source control systems. As of npm@5.7.0, these conflicts can be resolved by manually fixing any package.json conflicts, and then running npm install [--package-lock-only] again. npm will automatically resolve any conflicts for you and write a merged package lock that includes all the dependencies from both branches in a reasonable tree. If --package-lock-only is provided, it will do this without also modifying your local node_modules/.

To make this process seamless on git, consider installing npm-merge-driver, which will teach git how to do this itself without any user interaction. In short:

    $ npx npm-merge-driver install -g

will let you do this, and even works with pre-npm@5.7.0 versions of npm 5, albeit a bit more noisily. Note that if package.json itself conflicts, you will have to resolve that by hand and run npm install manually, even with the merge driver.

[via](https://docs.npmjs.com/cli/v6/configuring-npm/package-locks)

See also
https://docs.npmjs.com/cli/v6/configuring-npm/package-lock-json


## Process Monitoring

If you have a service running live using a node based (e.g. [express](express.md)) server, a monitoring tool can make sure it stays up. Options include...


### Docker

[Docker](/system/virtualization/docker.md) is a great option.


### PM2

https://www.google.com/search?client=ubuntu&channel=fs&q=pm2&ie=utf-8&oe=utf-8  
pm2 - Google Search  
https://pm2.keymetrics.io/  
PM2 - Home  

### Nodemon

https://github.com/remy/nodemon  
GitHub - remy/nodemon: Monitor for any changes in your node.js application and automatically restart the server - perfect for development  
https://www.google.com/search?client=ubuntu&channel=fs&q=nodemon+vs+pm2&ie=utf-8&oe=utf-8  
nodemon vs pm2 - Google Search  


## Environment Variables (.env) dotenv

Variables set in a `.env` file are automatically loaded by node and made available via `process.env.*` variables. 

Try out a ui/.env file. Is the value available via process.env.whatever?

Many frameworks leverage these variables for configuration. 

See also: [nuxt configuration variables](../vue/nuxt.html#configuration-variables-env-dotenv)
