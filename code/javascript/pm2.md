# PM2 

> PM2 is a daemon process manager that will help you manage and keep your application online.

https://pm2.keymetrics.io

## Installation

Make sure [node](node.md) is installed on the system. 

```
npm install pm2@latest -g
```

## Basics

https://pm2.keymetrics.io/docs/usage/quick-start/

```
pm2 list
pm2 show app_name 

pm2 start app_name
pm2 restart app_name
pm2 reload app_name
pm2 stop app_name
pm2 delete app_name

pm2 save
```

`app_name` can either be the app or the configuration file


## Starting an app

The simplest way to start, daemonize and monitor your application is by using this command line:

```
$ pm2 start app.js
```

Or start any other application easily:

```
$ pm2 start bashscript.sh
$ pm2 start python-app.py --watch
$ pm2 start binary-file -- --port 1520
```

Some options you can pass to the CLI:

```
# Specify an app name
--name <app_name>

# Watch and Restart app when files change
--watch

# Set memory threshold for app reload
--max-memory-restart <200MB>

# Specify log file
--log <log_path>

# Pass extra arguments to the script
-- arg1 arg2 arg3

# Delay between automatic restarts
--restart-delay <delay in ms>

# Prefix logs with time
--time

# Do not auto restart app
--no-autorestart

# Specify cron for forced restart
--cron <cron_pattern>

# Attach to application log
--no-daemon
```

## Rename a process

```
pm2 restart id --name newName
```


## Configuration Files

`.yml` or `.json` files both work

```
apps:
  - script   :  runjob.py
    cwd      :  /srv/runjob
    name     : 'boilerplate_api'
    watch    : true
    exec_mode: fork
    out_file : /srv/var/log/boilerplate_api.log
    err_file : /srv/var/log/boilerplate_api.err
    combine_logs : false
```

load with the same `pm2 start` command

```
pm2 start pm2.yml
```

(if processes with same name exist, do a `pm2 delete all/process_name` first)

### Restart options

By default, pm2 will restart processes when they fail. If an api is unavailable, this may result in a surge in log messages. 

Exponential backoff is useful to slow restart attempts until the API is back online:

```
module.exports = {
  script: 'app.js',
  exp_backoff_restart_delay: 100
}
```

https://pm2.keymetrics.io/docs/usage/restart-strategies/

### Watching

In production, 'watch' is a bad idea. 


## System Setup 

Once things are working as desired, be sure to save the startup with:

```
pm2 save
```
    
Then generate the systemd registration with:

```
pm2 startup
```
    
It will generate something like: 

```
sudo env PATH=$PATH:/usr/bin /usr/lib/node_modules/pm2/bin/pm2 startup systemd -u account --hp /home/account
```

### Removing settings

When removing all entries, may be necessary to use

```
pm2 save --force
```

to commit the changes


## Log Rotation

https://github.com/keymetrics/pm2-logrotate

```
pm2 set pm2-logrotate:compress true
pm2 set pm2-logrotate:retain 8
pm2 set pm2-logrotate:rotateInterval '0 5 * * 1'
```

```
#!/bin/bash
pm2 set pm2-logrotate:max_size 500M
pm2 set pm2-logrotate:retain 26
pm2 set pm2-logrotate:compress true
pm2 set pm2-logrotate:rotateInterval '0 0 5 * * 7'
```

## Removing Jobs

If you really want to remove the processes, run `pm2 delete all` and then `pm2 save` 


## Environment Variables

https://duckduckgo.com/?q=pm2+specify+environment+variable&t=ffab&ia=web  
pm2 specify environment variable at DuckDuckGo  
https://pm2.io/docs/runtime/best-practices/environment-variables/  
PM2 - Environment Variables | Best Practices | PM2 Documentation  

## Node scripts

May need to reference the path to the script in node modules

example:

```
apps:
  - cwd      : /srv/boilerplate/ui
    script   : node_modules/nuxt/bin/nuxt.js
    args     : 'start'
    env: 
      NODE_ENV : "production"
      HOST     : "127.0.0.1"
      BASE_URL : 'https://example.com'
      API_URL  : 'https://example.com/api/'
    name     : 'example-ui'
    watch    : false
    exec_mode: fork
    out_file : /var/log/boilerplate_admin_ui.log
    err_file : /var/log/boilerplate_admin_ui.err
    combine_logs : false
```

https://duckduckgo.com/?t=ffab&q=pm2+pm2.yml+start+command+in+package.json&ia=web  
pm2 pm2.yml start command in package.json at DuckDuckGo  
https://pm2.keymetrics.io/docs/usage/quick-start/  
PM2 - Quick Start  
https://pm2.keymetrics.io/docs/usage/application-declaration/  
PM2 - Ecosystem File  
https://stackoverflow.com/questions/46008665/how-to-start-a-package-json-script-in-pm2  
node.js - How to start a package.json script in pm2 - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=pm2+yml+configuration&ia=web  
pm2 yml configuration at DuckDuckGo  

## Python Virtual Environments

If you activate the virtual environment eg. `source venv/Scripts/activate` then start your script via pm2 eg. `pm2 start main.py --name migration`, it will automatically use the environment you've activated.

Alternatively, pass in the interpreter:

```
pm2 start ./strain_to_db --interpreter ./py3env/bin/python
```

via: https://stackoverflow.com/questions/36090655/running-a-python-script-in-virtual-environment-with-node-js-pm2


## Docker

I had difficulty getting PM2 to run under Docker. It doesn't make much sense unless you're attempting to replicate a production environment in docker. 

If you need to run multiple python workers in the same container, look to `supervisord`

It's possible to use `pm2` in a container.

https://pm2.keymetrics.io/docs/usage/docker-pm2-nodejs/

