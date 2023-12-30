# Docker Compose

Docker Compose is included with docker by default.

Create a configuration file for the current project's docker container setup in `docker-compose.yml`.

## Basics

After editing the docker-compose.yml file for the services, launch them with:

```
docker compose up -d
```

Then, to bring everything down again:

```
docker compose down -v
```

See what is currently running:

```
docker compose ps
```

If the container is not running, it may not show up in just `docker ps`. You can still see the status in `docker compose ps`

To connect to a running container (or run any command inside it):

```
docker compose exec [name]
```

You can use the short name here, not potentially more verbose container_name

To see what has been happening in a container, check the logs

```
docker compose logs [name]
```

The above commands can get tiring to type every time you want to take action with a compose environment. Shortcuts help. See [Docker notes](docker.md)


## docker-compose.yml

The file that defines all containers to be used for the application. 

One parameter that is helpful is

```
    container_name: mycontainername
```
    
If `contianer_name` is not specified, the parent directory name is used to create a container name. The `container_name` parameter helps keep container names consistent regardless of where they are deployed. This, in turn, makes it easier to create other configuration files that work as expected within the docker network. 

### Base Image

Frequently, container images already exist that meet the requirements. 

Start with an existing docker image for the type of service your application runs in the container. For example, if you're running a node application, in `docker-compose.yml` start with:

``` yaml
  api:
    image: node:lts
```

### Multiple commands

In some cases it may help to run more than one command. You can separate these out into separate compose files (e.g. docker-compose-build.yml), or you could run multiple commands by chaining them together in a `sh` call:

```
command: bash -c "
    python manage.py migrate
    && python manage.py runserver 0.0.0.0:8000
  "
```

https://stackoverflow.com/questions/30063907/using-docker-compose-how-to-execute-multiple-commands

### Custom images (Dockerfile)

If you need to run multiple commands, build a custom image with a dedicated Dockerfile. 

The dockerfile can be specified in the `docker-compose.yml` file with:

```
container-name:
   build:
      context: .
      dockerfile: <Below_Directory>/Dockerfile

```

Reminder:  
Image for service was built because it did not already exist. To rebuild this image you must use `docker compose build` or `docker compose up --build`.


Eventually other additional utilities will need to be available within the container context. (e.g. when you run `docker compose exec api bash` to connect to the container). In that case, use a `Dockerfile` to make those adjustments so the changes persist across restarts. 

``` yaml
  api:
    build:
      context: ./api
      dockerfile: Dockerfile
```

Note: the `Dockerfile` path is relative to the value for `context`. In this case, the `Dockerfile` would be stored at `./api/Dockerfile`. 

The `image: node:lts` configuration from `docker-compose.yml` becomes `FROM node:lts` in a `Dockerfile` and you can add the rest of the configurations as needed. See also [docker#dockerfile](docker.md#dockerfile)


## Environment Variables

In `docker-compose.yml`, add a section like:

```yaml
    environment:
      POSTGRES_PASSWORD: example
```


It's also possible to put variables in a `.env` file and then reference those variables in the `docker-compose.yml` file

https://medium.com/@cybourgeoisie/docker-env-methods-for-passing-variables-through-docker-compose-801e6fdb4a75


## Volumes

How to persist data across container restarts. 

What is the difference between `external: true` and `external: false`? 

```
volumes:
  boilerplate_ui_modules:
    external: true

  boilerplate_api_modules:
    external: true
```


## Networking

To restrict to local machine only, edit the docker-compose.yml file and change to:

```
        ports:
            - "127.0.0.1:3000:3000"
```

To restrict a service so it's only available within the container network (not available on the host directly), then use `expose` instead:

```
    expose:
      - 3306
```

https://stackoverflow.com/questions/35429837/docker-compose-port-mapping

https://docs.docker.com/compose/compose-file/#ports

https://stackoverflow.com/questions/40801772/what-is-the-difference-between-docker-compose-ports-vs-expose

### Custom Network

Usually it's sufficient to use the default network in a compose file so all containers specified will have access to each other, but not to anything else. 

https://docs.docker.com/compose/networking/

If you want to run a number of different applications, but proxy them all behind the same nginx host (running in a different container), it could help to specify the network in the compose file:

```
networks:
  default:
    name: my-pre-existing-network
```


## Rebuilding

Sometimes the Dockerfile changes and you need to let compose know to rebuild everything. 

```
docker compose up -d --no-deps --build
```

via: 
https://stackoverflow.com/questions/36884991/how-to-rebuild-docker-container-in-docker-compose-yml


To rebuild, use:

```
docker compose build
```

Remove all old images

```
docker compose rm
```
    
then rebuild again.


### Rebuilding Images

Rebuild an image from scratch with docker compose 

```
docker compose build --no-cache
```

Useful in instances when, for example, you do a `RUN apt-get update` that rarely gets re-run and then package listings get out of date. 

However, sometimes if you run an action in a Dockerfile like installilng software, the original image still will not be rebuilt. Over time, this can lead to very crusty layers in your docker containers. 

To work around the situation, this seems pretty extreme (especially when there are potentially many other containers on the same system), but it does force docker to pull in all new images:

```
docker system prune -a
```

Heed the warning:

```
WARNING! This will remove:
  - all stopped containers
  - all networks not used by at least one container
  - all images without at least one container associated to them
  - all build cache
```

https://duckduckgo.com/?t=ffab&q=docker+compose+build+no+cache&ia=web
docker compose build no cache at DuckDuckGo
https://stackoverflow.com/questions/35594987/how-to-force-docker-for-a-clean-build-of-an-image
How to force Docker for a clean build of an image - Stack Overflow


## Troubleshooting 

For troubleshooting, you can add a command that is sure to run in the docker-compose.yml, e.g.:

```
    entrypoint: ["tail", "-f", "/dev/null"]

    entrypoint: ["sh", "-c", "sleep 2073600"]
```

then connect with:

```
docker compose exec SERVICE_NAME bash
```

via:  
https://vsupalov.com/debug-docker-compose-service/

Logging is available via docker directly:

```
docker logs repo_nginx_1
```
 
see also: [docker.md](docker.md)

```
docker network ls
```


## Parameters

### -f 

Specify a configuration file with a name other than `docker-compose.yml`

```
docker compose -f docker-compose.builder.yml 
```

### -p

Using a `container_name` setting in `docker-compose.yml` makes it easy to write configuration files that work as expected. In that case `-p` is rarely needed. 

If no `container_name` parameter is set in docker-compose.yml, by default, the docker project name is the parent directory name. This is usually the case for local development setups.

There are times, however, when it is useful to make directory names that are different than the project name. For example, working on a different branch, it may be easier to use the branch name instead of the project name for the parent directory.

If the parent directory is **not** equal to the project name, you'll want to pass the project name in to all of the above docker compose commands.

Using `-p boilerplate` allows the project name (and consequently the container name) to be consistent from one deployment to the next. That way containers are named with `boilerplate_[service]_1`.

This allows configuration files to be written ahead of time to work.

If you've checked out the repository to a directory named `boilerplate`, the project name `boilerplate` will be assumed by docker and the `-p` option may be omitted.


## Guides

Great article for using Docker for a local development environment:  
https://hackernoon.com/a-better-way-to-develop-node-js-with-docker-cd29d3a0093


## See Also

[Orchestration](orchestration.md)

[Kubernetes](kubernetes.md)


https://github.com/veggiemonk/awesome-docker#container-composition  
GitHub - veggiemonk/awesome-docker: A curated list of Docker resources and projects  
https://github.com/magicmark/composerize  
GitHub - magicmark/composerize: docker run asdlksjfksdf > docker-composerize up  
https://github.com/kubernetes/kompose  
GitHub - kubernetes/kompose: Go from Docker Compose to Kubernetes  
https://github.com/jwilder/dockerize  
GitHub - jwilder/dockerize: Utility to simplify running applications in docker containers  
https://github.com/jenssegers/captain  
GitHub - jenssegers/captain: E️asily start and stop docker compose projects  

## Example compose file

web ui api db

A useful pattern and a good reference:

```yaml
version: "3"
services:
  web:
    # https://hub.docker.com/_/nginx/
    # image: nginx:latest
    image: nginx:stable
    container_name: boilerplate_web
    # restart: unless-stopped
    volumes:
      - ./ui/dist:/srv/boilerplate/dist
      - ./web/static:/srv/boilerplate/static
      - ./web/ssl:/etc/nginx/ssl
      - ./web/default.conf:/etc/nginx/conf.d/default.conf
      # mount any static / public content for efficient serving directly
      # ./web/public;/srv/boilerplate/files/
    working_dir: /srv/boilerplate/
    ports:
      # for development
      - 127.0.0.1:8888:80
      # leave off '127.0.0.1' if you want to expose the service beyond localhost
      # (useful when you want to access dev instance of boilerplate remotely
      #  from e.g. a phone)
      # - 8888:80
      # for production
      # - 80:80
      # - 443:443
    # if the container doesn't run, there may be a problem with the nginx.conf
    # try running `docker compose log web` for clues
    # (usually SSL keys have not yet been generated in `web/ssl`)
    # keep the container running to debug or run interactively with
    #     docker compose exec web bash
    # entrypoint: ["tail", "-f", "/dev/null"]
    networks:
      default:
        aliases:
          - boilerplate.local

  ui:
    # https://hub.docker.com/_/node/
    # for production, it's a good idea to fix the version number
    # image: node:14
    # but to keep things fresh in development
    # image: node:lts

    # if you add packages to packages.json, be sure to run
    # docker compose up -d --build
    # so that modules are available in container
    build:
      context: ui/
      dockerfile: Dockerfile
    container_name: boilerplate_ui
    # restart: unless-stopped
    volumes:
      # - ./ui:/srv/boilerplate/ui
      - ./ui:/data
      # - boilerplate_ui_modules:/srv/boilerplate/ui/node_modules
    # ports:
    #   - 127.0.0.1:3000:3000
    working_dir: /srv/boilerplate/ui
    command: sh -c "yarn && yarn run build"
    # command: sh -c "yarn && yarn run dev"
    # entrypoint: ["tail", "-f", "/dev/null"]

  api:
    # image: node:lts
    build:
      context: api/
      dockerfile: Dockerfile
    container_name: boilerplate_api
    # restart: unless-stopped
    volumes:
      - ./api:/srv/boilerplate/api
      # - boilerplate_api_modules:/srv/boilerplate/api/node_modules
    # ports:
    #   - 127.0.0.1:3030:3030
    working_dir: /srv/boilerplate/api
    # command: sh -c "yarn && yarn run dev"
    # DEBUG=express:* node ./api/boilerplate.js
    entrypoint: ["tail", "-f", "/dev/null"]
    # An example of chaining multiple commands for the container
    # More complex customization is possible through a Dockerfile
    # command: sh -c "
    #   mkdir -p /srv/var/log
    #   && node ./app.js
    #   "

  # db:
  #   # https://hub.docker.com/_/postgres/
  #   image: postgres:latest
  #   container_name: boilerplate_db
  #   # restart: unless-stopped
  #   # ports:
  #   # helpful for using a GUI client like compass for troubleshooting
  #   # - 127.0.0.1:27017:27017
  #   environment:
  #     POSTGRES_PASSWORD: example
  #   volumes:
  #     - ./db:/data/db
  #     # for importing database files
  #     # - ./dump:/srv/dump

  # db:
  #   # https://hub.docker.com/_/mongo
  #   image: mongo:5
  #   container_name: boilerplate_db
  #   restart: unless-stopped
  #   # ports:
  #   # helpful for using a GUI client like compass for troubleshooting
  #   # - 127.0.0.1:27017:27017
  #   #environment:
  #   # MONGO_INITDB_ROOT_USERNAME: root
  #   # MONGO_INITDB_ROOT_PASSWORD: example
  #   volumes:
  #     - ./db:/data/db
  #     # for importing database files
  #     # - ./mongodump:/srv/mongodump

  # db:
  #   image: mariadb:10.5
  #   container_name: boilerplate_db
  #   # restart: unless-stopped
  #   expose:
  #     - 3306
  #   # helpful for using a GUI client like compass or sqlectron for troubleshooting
  #   ports:
  #     - 127.0.0.1:3306:3306
  #   environment:
  #     MYSQL_ROOT_PASSWORD: example
  #   volumes:
  #     - ./db:/var/lib/mysql
  #     # for importing database files
  #     - ./dbdump:/srv/dbdump

  # redis:
  #   container_name: boilerplate_redis
  #   image: redis:5.0-alpine
  #   restart: unless-stopped
  #   ports:
  #     - 6379:6379
  #   volumes:
  #     # - db_redis:/data
  #     - ./db_redis:/data

  # docs:
  #   image: node:lts
  #   container_name: boilerplate_docs
  #   restart: unless-stopped
  #   volumes:
  #     - ./:/srv/boilerplate
  #     # this is a bit redundant, but shows that docs could be anywhere
  #     - ./docs:/srv/boilerplate/docs
  #   ports:
  #     - 7777:7777
  #   working_dir: /srv/boilerplate
  #   command: sh -c "yarn && yarn docs:dev"
  #   # keep the container running
  #   # then connect directly to debug or run interactively
  #   # entrypoint: ["tail", "-f", "/dev/null"]

  # in a CI/CD situation, running in a container could be helpful
  # test:
  #   # Allows for running tests in a headless mode
  #   image: cypress/included:8.4.0
  #   container_name: boilerplate_test_1
  #   # restart: unless-stopped
  #   depends_on:
  #     - ui
  #   environment:
  #     - CYPRESS_BASE_URL=http://boilerplate_ui:3000
  #     # use in tests with `Cypress.env("API_URL")`
  #     # - CYPRESS_API_URL=http://boilerplate_api:3030
  #     - CYPRESS_API_URL=http://boilerplate.local/api
  #     - DEBUG=cypress:*
  #   working_dir: /e2e
  #   # share the current folder as volume to avoid copying
  #   volumes:
  #     - ./:/e2e

```



## Installation

It is no longer required to install Docker Compose separately. Modern `docker` installations include it by default.

### NOTE:
The final Compose v1 release (v1.29.2) was May 10, 2021. These packages haven’t received any security updates since then. Use at your own risk.
https://docs.docker.com/compose/migrate/

https://docs.docker.com/compose/


