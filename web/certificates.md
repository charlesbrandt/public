# SSL Certificates

Web certificates facilitate traffic between a web browser and a web server to be encrypted, and ensure that only the server can read the contents of the traffic. 

Lets Encrypt -- it's hard to beat the price: free

https://letsencrypt.org/
Let's Encrypt


## Local Certificates

During development, it helps to replicate encryption the way it will be used in production. However, for local services you won't have a certificate signed by a public certificate of authority provider (CA). 

Let's Encrypt recommends the following:

https://letsencrypt.org/docs/certificates-for-localhost/
Certificates for localhost - Let's Encrypt

Unfortunately, in Chrome browsers, this leads to all sorts of issues.

Even when importing the certificates
in Chrome -> Settings -> Security -> Manage Certificates
 
https://stackoverflow.com/questions/55947983/how-to-fix-certificate-import-error-the-private-key-for-this-client-certificat
linux - How to fix ‘Certificate Import Error: The Private Key for this Client Certificate is missing or invalid' error in the import certificate file - Stack Overflow

## Cypress

Testing with Cypress opens up a few additional challenges. 

So far I have not been able to test a local service with a self-signed certificate using Chrome browsers. Firefox works. 


I mounted the generated certificates so they are available within the Cypress container (in `docker-compose.yml`)

``` yaml
    volumes:
      - ./web/ssl:/etc/nginx/ssl

```

A long thread about getting Chrome to work -- wasn't able to get this working. 

https://github.com/cypress-io/cypress/issues/771
💤 cy.visit() results in ERR_SSL_VERSION_OR_CIPHER_MISMATCH · Issue #771 · cypress-io/cypress


