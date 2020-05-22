# New Project Template

[Docker Compose](../system/virtualization/docker-compose.md) is a good tool for setting up a lightweight environment that documents dependencies without requiring a full dedicated VM. This guide assumes you have that installed already. 

Distilling and iterating on this article on using Docker for a local development environment:  
https://hackernoon.com/a-better-way-to-develop-node-js-with-docker-cd29d3a0093


## Architecture overview

Goal: 
    - A web server with SSL enabled for serving static files (it's good to dev, run and test with one!)
    - An app server of some type -- node in this case

Nginx is used to serve the generated markup and js files created by the js app node server.


## Configuration Files

### TODO

decide if ./config makes the most sense in a Nuxt project

What to call node container? node? js? ui? app? project_name? Varies from project to project. Something a bit more unique here? 

Many options to change and customize. 
find and replace `project` with your unique project name
find and replace `/path/to/serve/` with your root path (e.g. `srv` or `opt` or `public`)

### docker-compose.yml

``` yaml
version: '3'
services:

  nginx:
    image: nginx:stable
    volumes:
      - .:/path/to/serve/project
      - ./config/project.conf:/etc/nginx/conf.d/default.conf
      - ./config/ssl:/etc/nginx/ssl 
    ports:
      - 8000:80
      - 8001:443
    networks:
      - project-network
      
  node:
    image: node:stable
    volumes:
      - .:/path/to/serve/project
      - nodemodules:/path/to/serve/project/node_modules
    networks:
      - project-network
    working_dir: /path/to/serve/project
    # keep the container running
    entrypoint: ["tail", "-f", "/dev/null"]
    #command: npm run dev

volumes:
  nodemodules:
    external: true

networks:
  project-network:
    driver: bridge
```

### docker-compose-setup.yml

``` yaml
version: '2'
services:
  node:
    image: node:stable
    volumes:
      - .:/path/to/serve/project
      - nodemodules:/path/to/serve/project/node_modules
    working_dir: /path/to/serve/project
  install:
    extends:
      service: node
    command: npm i

volumes:
  nodemodules:
    external: true
```

### configs/project.conf

```nginx
server {
    listen 80;
    
    # server_name example.com
    server_name localhost;
    
    # Under docker, it's nice to have the default logs bubble up
    # e.g. `docker logs repo_nginx_1`
    # this will store them on the container itself
    #access_log /var/log/nginx/project-access.log;
    #error_log /var/log/nginx/project-error.log debug;
    
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    # server_name example.com;

    ssl_certificate /etc/nginx/ssl/nginx.crt;
    ssl_certificate_key /etc/nginx/ssl/nginx.key;

    ssl_session_cache shared:SSL:20m;
    ssl_session_timeout 60m;
 
    ssl_protocols TLSv1.2;
    ssl_prefer_server_ciphers on;
    ssl_ciphers ECDH+AESGCM:ECDH+AES256:ECDH+AES128:DHE+AES128:!ADH:!AECDH:!MD5;
 
    # ssl_stapling on;
    # ssl_stapling_verify on;

    add_header Content-Security-Policy "default-src https: wss: data: blob: 'unsafe-inline' 'unsafe-eval'" always;
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    # add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Xss-Protection "1; mode=block" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header Referrer-Policy "no-referrer-when-downgrade";

    proxy_redirect off;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto  https;

    gzip on;
    gzip_static on;
    gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;
    gzip_proxied  any;
    gzip_vary on;
    gzip_comp_level 6;
    gzip_buffers 16 8k;
    gzip_http_version 1.1;

    location / {
        root /path/to/serve/project/build;
   
        # kill cache
        add_header Last-Modified $date_gmt;
        add_header Cache-Control 'no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0';
        if_modified_since off;
        expires off;
        etag off;
    }
    
    location /api/ {
        proxy_http_version 1.1;
        proxy_set_header Connection "";
        proxy_buffering off;
        proxy_pass http://localhost:8888/;
    }
}

```

## Setup

Create a shared volume for build steps and serving steps (these are also available in the Makefile):

    docker volume create --name=nodemodules

Generate SSL keys with:

    sudo openssl req -subj '/CN=localhost' -x509 -nodes -days 365 -newkey rsa:2048 -keyout ./config/ssl/nginx.key -out ./config/ssl/nginx.crt

Install node packages for the UI with:

    docker-compose -f docker-compose-builder.yml run --rm install

Bring up the nginx and ui containers:

    docker-compose up -d

Make sure everything is running (no Exit 1 States):

    docker-compose ps

To run the build step interactively (to see if errors come up):

    docker-compose exec ui bash

    npm run dev


## See Also

~/public/system/virtualization/docker-compose.md
