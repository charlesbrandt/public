# Nginx

http://nginx.org/en/docs/beginners_guide.html#conf_structure

https://www.digitalocean.com/community/tutorials/understanding-the-nginx-configuration-file-structure-and-configuration-contexts

The config file is typically found:

    /etc/nginx/nginx.conf

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


## Static Content

When developing web applications, it's tempting to use the application server to serve static content:

https://expressjs.com/en/starter/static-files.html

This works for local development, but becomes a headache when it's time to deploy your application publicly. 

Using a dedicated webserver like nginx for serving static files removes that burden from the application server.

https://docs.nginx.com/nginx/admin-guide/web-server/serving-static-content/

See also:

~/public/code/new_project.md
