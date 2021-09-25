# Troubleshooting Web Connections

`curl` and `wget` are both useful tools. 

## Curl

1.3 What is cURL not?
Curl is *not* a wget clone even though that is a very common misconception.
Never, during curl's development, have we intended curl to replace wget or
compete on its market. Curl is targeted at single-shot file transfers.
_
Curl is not a web site mirroring program. If you wanna use curl to mirror
something: fine, go ahead and write a script that wraps around curl to make
it reality (like curlmirror.pl does).

    curl -X GET [options] [URL]

    curl -X POST [options] [URL]

The -X option specifies which HTTP request method will be used when communicating with the remote server.

One way to pass parameters to a POST request is to use the -d option. This causes curl to send the data using the application/x-www-form-urlencoded Content-Type.

    curl -X POST -d 'name=linuxize' -d 'email=linuxize@example.com' https://example.com/contact.php

!!! -d seems to be what node/express servers are expecting when processing POST requests

The alternative is -F. When the -F option is used, curl sends the data using the multipart/form-data Content-Type.

    curl -X POST -F 'name=linuxize' -F 'email=linuxize@example.com' https://example.com/contact.php

[via](https://linuxize.com/post/curl-post-request/)

To post json directly:

curl -X POST -H "Content-Type: application/json" \
 -d '{"username":"abc","password":"abc"}' \
 https://api.example.com/v2/login
 
https://tecadmin.net/post-json-data-with-curl-command/


### TLS / SSL 

With self signed certificates, it may be necessary to tell curl to connect even if it is not a validated certificate. 

https://curl.haxx.se/docs/sslcerts.html

With the curl command line tool, you disable this with -k/--insecure.



## See Also

`wget` is better suited for web scraping applications. 

[wget](wget.md)

[Cypress](/code/testing/cypress.md)
