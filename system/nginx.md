# Nginx

## Installation

### Docker 

With docker, you can install nginx with something like:

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
    # try running `docker-compose log web` for clues
    # (usually SSL keys have not yet been generated in `web/ssl`)
    # keep the container running to debug or run interactively with
    #     docker-compose exec web bash
    # entrypoint: ["tail", "-f", "/dev/null"]
    networks:
      default:
        aliases:
          - boilerplate.local
```

This is a good option to encapsulate all necessary settings for an application stack. 

### Local

On some servers with multiple services running (e.g. a server instead of a development machine), it may help to run one nginx server at the host level and configure that to proxy to other services as needed. 

```
sudo apt-get install nginx
```

## Configuration

http://nginx.org/en/docs/beginners_guide.html#conf_structure

https://www.digitalocean.com/community/tutorials/understanding-the-nginx-configuration-file-structure-and-configuration-contexts

The main config file is typically found:

    /etc/nginx/nginx.conf

On a fresh install, by default it is configured to point to :



Can have as many server contexts as needed:

```
# main context

http {

    # http context

    server {

        # first server context

    }

    server {

        # second server context

    }

}
```

Generally these are configured in separate files in 

    /etc/nginx/conf.d/*.conf

In:

```
sudo vi /etc/nginx/sites-enabled/default
```

You should look at the following URL's in order to grasp a solid understanding
of Nginx configuration files in order to fully unleash the power of Nginx.

https://www.nginx.com/resources/wiki/start/  
https://www.nginx.com/resources/wiki/start/topics/tutorials/config_pitfalls/  
https://wiki.debian.org/Nginx/DirectoryStructure  


Then set it to run on startup

```
sudo systemctl start nginx
sudo systemctl enable nginx
```

[See also: Startup Services](startup-services.md)

Disable the default site and add in the desired configuration:

```
sudo rm /etc/nginx/sites-enabled/default 
```

Ok to use either `/etc/nginx/conf.d/` or `/etc/nginx/sites-available` and link to the enabled ones in `/etc/nginx/sites-enabled`. Call the files `something.conf`

```
server {
  listen 80;
  listen [::]:80;

  server_name example.com;

  location / {
      proxy_pass http://localhost:3000/;
  }
}
```

## Alias vs Root

https://stackoverflow.com/questions/10631933/nginx-static-file-serving-confusion-with-root-alias

be sure to include trailing slash with `alias` directives!

## Static Content

When developing web applications, it's tempting to use the application server to serve static content:

https://expressjs.com/en/starter/static-files.html

This works for local development, but becomes a headache when it's time to deploy your application publicly. 

Using a dedicated webserver like nginx for serving static files removes that burden from the application server.

https://docs.nginx.com/nginx/admin-guide/web-server/serving-static-content/

### Auto Indexes

If you have static content that would benefit from a dynamic index, nginx can do that. 
For example, want to get a list that includes new files without manually knowing the new file path. 
Alternatively, an application server could navigate and iterate through the file system. 

```
    location /static {
        alias /srv/static/;
        autoindex on;
        autoindex_format json;
    }
```

https://fuzzyblog.io/blog/nginx/2020/06/15/configuring-nginx-to-serve-a-directory-listing.html
Configuring NGINX to Serve a Directory Listing
https://stackoverflow.com/questions/24387118/nginx-list-static-files-directories-as-xml-json
Nginx list static files/directories as XML/Json - Stack Overflow

https://duckduckgo.com/?q=nginx+list+directory+contents+as+json&t=ffab&ia=web
nginx list directory contents as json at DuckDuckGo



## Websockets

https://www.serverlab.ca/tutorials/linux/web-servers-linux/how-to-proxy-wss-websockets-with-nginx/



## HTTPS / SSL

### Self-signed

For local development environments, self signed is good enough. It makes sure that the application is configured correctly to work behind an HTTPS connection, but doesn't require an external certificate / local DNS entry. 

https://codewithhugo.com/docker-compose-local-https/

https://medium.com/@codingwithmanny/configure-self-signed-ssl-for-nginx-docker-from-a-scratch-7c2bcd5478c6

https://medium.com/@oliver.zampieri/self-signed-ssl-reverse-proxy-with-docker-dbfc78c05b41

https://www.ssls.com/knowledgebase/how-to-install-an-ssl-certificate-on-a-nginx-server/

Use OpenSSL to create a self signed certificate

    openssl req -subj '/CN=localhost' -x509 -newkey rsa:4096 -nodes -keyout key.pem -out cert.pem -days 365


### Let's Encrypt

Once it's time to publish your application, you'll need a domain name. If you have that ready to go, you can use Let's Encrypt for a certificate

https://medium.com/@pentacent/nginx-and-lets-encrypt-with-docker-in-less-than-5-minutes-b4b8a60d3a71


## Troubleshooting

Log files are typically stored in 

    /var/log/nginx

With containers

    docker-compose exec web bash

then

    nginx -t 

is super helpful for debugging nginx configurations

To see if the nginx process is running: 

    service nginx status
    
(On the nginx docker container, ps is not available)

To reload nginx without reloading all of the containers:

    docker-compose -p boilerplate exec web nginx -s reload

For netstat:

    apt-get update
    apt-get install net-tools

    netstat -pan | grep LISTEN

See also dedicated section for nginx

https://gitlab.com/charlesbrandt/public/-/blob/main/system/virtualization/docker.md#troubleshooting-nginx
