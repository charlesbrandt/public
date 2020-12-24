# Node

https://nodejs.org/en/
https://github.com/nodejs/node

Node.js® is a JavaScript runtime built on Chrome's V8 JavaScript engine.

## Install

Clean up old version(s):

    npm ls -gp --depth=0 | awk -F/ '/node_modules/ && !/\/npm$/ {print $NF}' | sudo xargs npm -g rm

### NVM

Node Version Manager - Simple bash script to manage multiple active node.js versions
https://github.com/creationix/nvm

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
    
Open a new shell and verify with:

    command -v nvm

### Node

Install Node via NVM:

    nvm install node

via:
http://stackoverflow.com/a/24404451/1480391

### Nodesource

Nodesource is another popular way to install node

https://github.com/nodesource/distributions/blob/master/README.md



## Package Managers

See also: [Package Manager](../package_management.md)

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

### Yarn

Yarn is a popular alternative to NPM. 

Install / update yarn:

    npm install -g yarn

or use a system level package manager 

    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -

    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

then, with apt-get:

    sudo apt-get install --no-install-recommends yarn


via https://yarnpkg.com/lang/en/docs/install/

Once you have `yarn` available, you can add packages as a requirement with:

    yarn add
    
or

    yarn add --dev


## Process Monitoring

If you have a service running live using a node based (e.g. [express](express.md)) server, a monitoring tool can make sure it stays up.

I've used PM2. 

https://www.google.com/search?client=ubuntu&channel=fs&q=pm2&ie=utf-8&oe=utf-8
pm2 - Google Search
https://pm2.keymetrics.io/
PM2 - Home

https://github.com/remy/nodemon
GitHub - remy/nodemon: Monitor for any changes in your node.js application and automatically restart the server - perfect for development
https://www.google.com/search?client=ubuntu&channel=fs&q=nodemon+vs+pm2&ie=utf-8&oe=utf-8
nodemon vs pm2 - Google Search


## package-lock.json

TODO:
is it really a good idea to track this under version control?
seems like it adds a lot of noise to the process

Yes, it can add noise if you manage it manually. However, without it you may run into situations where deployments to a different environment may not work due to different versions of modules being installed. It's better to test with a set of locked versions. In that case, package-lock.json is necessary. 

### Resolving lockfile conflicts

Occasionally, two separate calls to `npm install` will create package locks that cause merge conflicts in source control systems. As of npm@5.7.0, these conflicts can be resolved by manually fixing any package.json conflicts, and then running npm install [--package-lock-only] again. npm will automatically resolve any conflicts for you and write a merged package lock that includes all the dependencies from both branches in a reasonable tree. If --package-lock-only is provided, it will do this without also modifying your local node_modules/.

To make this process seamless on git, consider installing npm-merge-driver, which will teach git how to do this itself without any user interaction. In short: 

    $ npx npm-merge-driver install -g 
    
will let you do this, and even works with pre-npm@5.7.0 versions of npm 5, albeit a bit more noisily. Note that if package.json itself conflicts, you will have to resolve that by hand and run npm install manually, even with the merge driver.

[via](https://docs.npmjs.com/cli/v6/configuring-npm/package-locks)

See also
https://docs.npmjs.com/cli/v6/configuring-npm/package-lock-json
