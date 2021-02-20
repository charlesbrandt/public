# Web Servers

Closely related to [storage](../storage/storage.md)

Web Servers are the foundation for just about any application that is shared across many machines.

It does imply that the server serves as a central point of contact for the whole application. If that is not the case, explore concepts behind the decentralized web. [decentralized](decentralized.md)

## Nginx

https://duckduckgo.com/?q=serving+markdown+nginx&t=canonical&ia=web
serving markdown nginx at DuckDuckGo
https://www.nginx.com/resources/wiki/contributing/markdown/
Docs in Markdown | NGINX
https://gist.github.com/sixtyfive/c36a7cfb31205fab686e0a88284444b9
How to serve static Markdown directly through Nginx using php-fpm · GitHub
https://muetsch.io/caddy-a-modern-web-server-vs-nginx.html
Caddy - a modern web server (vs. nginx)
https://duckduckgo.com/?q=nginx+guide&t=canonical&ia=web
nginx guide at DuckDuckGo
https://nginx.org/en/docs/beginners_guide.html
Beginner’s Guide

See also:
~/music/stream/streaming.md

## Development / Application Server

SimpleHTTPServer is now available here:

    python3 -m http.server

Something else?

The trouble with using the Python webserver for production is that it is single threaded and doesn't scale well to multiple simultaneous requests.

You should have a webserver running, especially one in a container.
This makes it easier to deploy to external services.

## Apache

## 2019.11.23 16:03:54

\*2016.09.29 10:46:12
Go with whatever you can get running quickly

https://duckduckgo.com/?q=best+webserver+for+static+files&t=ffab&ia=qa
best webserver for static files at DuckDuckGo
http://serverfault.com/questions/219620/which-is-the-best-webserver-for-serving-static-content-and-load-balancing/219719
web server - which is the best webserver for serving static content and load balancing? - Server Fault
http://stackoverflow.com/questions/5050851/best-lightweight-web-server-only-static-content-for-windows
http - Best lightweight web server (only static content) for windows - Stack Overflow
https://duckduckgo.com/?q=nginx+vs+apache&t=ffab&ia=web
nginx vs apache at DuckDuckGo
https://www.digitalocean.com/community/tutorials/apache-vs-nginx-practical-considerations
Apache vs Nginx: Practical Considerations | DigitalOcean
