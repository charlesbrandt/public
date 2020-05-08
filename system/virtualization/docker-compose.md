# Docker Compose

https://docs.docker.com/compose/

If you want to run multiple containers to meet the requirements of a more complicated service, you can use Docker Compose to bring all of the containers up together. To install docker-compose:

    apt-get install docker-compose -y
    
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

In some cases it may help to run more than one command. You can separate these out into separate compose files (e.g. docker-compose-build.yml), or you could run multiple commands by chaining them together in a `sh` call:

```
command: bash -c "
    python manage.py migrate
    && python manage.py runserver 0.0.0.0:8000
  "
```

https://stackoverflow.com/questions/30063907/using-docker-compose-how-to-execute-multiple-commands

Beyond that, and you may want to consider building a custom image with a dedicated [Dockerfile](docker.md). 


## Troubleshooting 

For troubleshooting, you can add a command that is sure to run in the docker-compose.yml, e.g.:

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

## See Also

orchestration.md
kubernetes.md
 
