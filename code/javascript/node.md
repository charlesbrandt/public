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

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.1/install.sh | bash
    
Open a new shell and verify with:

    command -v nvm

### Node

Install Node via NVM:

    nvm install node

via:
http://stackoverflow.com/a/24404451/1480391


## Package Managers

See also: [Package Manager](../package_management.md)

### Npm

NPM is the default package manager for Node. 

Calls to 

    npm install --save 

or 
  
    npm install --save-dev 
    
or 

    npm install --save-optional 

will update the package.json to list your dependencies.


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

