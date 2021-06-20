# Docker Compose

Configuration file for current project is in `docker-compose.yml`.

## About

Docker Compose relies on [docker.](docker.md) Be sure to install that first.

https://docs.docker.com/compose/

If you want to run multiple containers to meet the requirements of a more complicated service, you can use Docker Compose to bring all of the containers up together. To install docker-compose:

    sudo apt-get install docker-compose -y
    
After editing the docker-compose.yml file for the services, launch them with:

    docker-compose up -d
    
    docker-compose up -d --no-deps --build

via: 
https://stackoverflow.com/questions/36884991/how-to-rebuild-docker-container-in-docker-compose-yml

Then, to bring everything down again:

    docker-compose down -v

To rebuild, use:

    docker-compose build

ERROR: for seafile-mysql no such image:
https://stackoverflow.com/questions/37454548/docker-compose-no-such-image

If the container is not running, it may not show up in just `docker ps`. You can see the status in `docker-compose ps`

Check for existing images: 

    docker-compose ps
    

Remove all old images

    docker-compose rm
    
then rebuild again.

## docker-compose.yml

This defines all containers to be used for the application. Ideally, there are existing images that meet the requirements. 

One parameter that is helpful is

    container_name: mycontainername
    
Typically the parent directory name is used to create a container name. The `container_name` parameter helps keep container names consistent regardless of where they are deployed. This, in turn, makes it easier to create other configuration files that work as expected within the docker network. 

In some cases it may help to run more than one command. You can separate these out into separate compose files (e.g. docker-compose-build.yml), or you could run multiple commands by chaining them together in a `sh` call:

```
command: bash -c "
    python manage.py migrate
    && python manage.py runserver 0.0.0.0:8000
  "
```

https://stackoverflow.com/questions/30063907/using-docker-compose-how-to-execute-multiple-commands

Beyond that, and you may want to consider building a custom image with a dedicated Dockerfile. 

The dockerfile can be specified in the docker-compose.yml file with:


```
container-name:
   build:
      context: .
      dockerfile: <Below_Directory>/Dockerfile

```

Reminder
Image for service was built because it did not already exist. To rebuild this image you must use `docker-compose build` or `docker-compose up --build`.

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


## Custom images (Dockerfile)

It's a good idea to start with an existing docker image for the type of service you want to use as a foundation for your container. For example, if you're running a node application, you could start with:

```docker-compose.yml
  api:
    image: node:14
```

Eventually you may want some other utilities to be available within the container context. (e.g. when you run `docker-compose exec api bash` to connect to the container). In that case, use a `Dockerfile` to make those adjustments so they persist across restarts. 

```docker-compose.yml
  api:
    build:
      context: ./api
      dockerfile: Dockerfile
```

Note that the dockerfile path is relative to the value for `context`

Then, in the Dockerfile, `image: node:14` becomes `FROM node:14` and you can add the rest of the configurations as needed. See also [docker.md](docker.md)


## Environment Variables

It's possible to put variables in a `.env` file and then reference those variables in the `docker-compose.yml` file

https://medium.com/@cybourgeoisie/docker-env-methods-for-passing-variables-through-docker-compose-801e6fdb4a75

## Troubleshooting 

For troubleshooting, you can add a command that is sure to run in the docker-compose.yml, e.g.:

    entrypoint: ["tail", "-f", "/dev/null"]

    entrypoint: ["sh", "-c", "sleep 2073600"]

then connect with:

    docker-compose exec SERVICE_NAME bash

via:  
https://vsupalov.com/debug-docker-compose-service/

Logging is available via docker directly:

    docker logs repo_nginx_1
    
see also: [docker.md](docker.md)

    docker network ls



## Guides

Straightforward guide for getting nginx running:  
https://dev.to/aminnairi/quick-web-server-with-nginx-on-docker-compose-43ol

Great article for using Docker for a local development environment:  
https://hackernoon.com/a-better-way-to-develop-node-js-with-docker-cd29d3a0093

    docker-compose -f docker-compose.builder.yml run --rm install

## Parameters

### -p

If no `container_name` parameter is set in docker-compose.yml, by default, the docker project name is the parent directory name. This is usually the case for local development setups.

There are times, however, when it is useful to make directory names that are different than the project name. For example, working on a different branch, it may be easier to use the branch name instead of the project name for the parent directory.

If the parent directory is **not** equal to the project name, you'll want to pass the project name in to all of the above docker-compose commands.

Using `-p boilerplate` allows the project name (and consequently the container name) to be consistent from one deployment to the next. That way containers are named with `boilerplate_[service]_1`.

This allows configuration files to be written ahead of time to work.

If you've checked out the repository to a directory named `boilerplate`, the project name `boilerplate` will be assumed by docker and the `-p` option may be omitted.


## See Also

orchestration.md
kubernetes.md
 
