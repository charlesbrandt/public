# Ingress

Ingress controllers are the gateway to your network. They should be hardened devices that only pass on traffic that you explicitly want to allow. 

Common ingress controllers:

nginx
Kong

## nginx


### Troubleshooting nginx

Go inside the container

`docker-compose exec web bash`

Or to use docker directly:

`docker exec -it <container-id> /bin/bash` 

[Troubleshooting](../../web/troubleshooting.md)

```
curl http://boilerplate_ui_1:3000
curl -X GET http://boilerplate_ui_1:3000
```

If blank, may be best to connect to the ui directly via host to make sure it's working as expected. 

#### Nginx Logging

Logs are routed to stdout/stderr so that docker can pass them through. 

Verify with check the log location `ls -la /var/log/nginx/` you will see the following output:

```
lrwxrwxrwx 1 root root   11 Apr 30 23:05 access.log -> /dev/stdout
lrwxrwxrwx 1 root root   11 Apr 30 23:05 error.log -> /dev/stderr
```

Execute `cat access.log` inside the container and it doesn't show anything.

The way to get your logs is going outside the container and running

    docker logs <container-id>

https://stackoverflow.com/questions/30269672/unable-to-use-lt-when-running-nginx-docker-or-cat-logs


Straightforward guide for getting nginx running:  
https://dev.to/aminnairi/quick-web-server-with-nginx-on-docker-compose-43ol



## Kong

I believe that Kong is based on nginx. A well tuned configuration? 
