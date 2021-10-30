# Auth

Auth includes both Authentication ('authn') and Authorization ('authz').

Auth can be tricky because it touches all parts of the stack, including remote services that are only used for auth. Hang in there!

### Authentication ('authn') 

Who are you?

Associating a session with a person. (A 'user'.)

### Authorization ('authz')

What can you do? Do you have permission to look at that?

## Oauth2

A standard way of delegating authorization to a third-party. 

https://oauth.net/2/  
OAuth 2.0 — OAuth  

Diagram for describing the different flows:

https://blog.oauth.io/introduction-oauth2-flow-diagrams/  
OAuth2 Introduction Through Flow Diagrams in 5-minutes - OAuth.io Blog  

A description based on Google APIs  
The principles are the same for any other Oauth2 flow  
https://developers.google.com/identity/protocols/oauth2/javascript-implicit-flow  
OAuth 2.0 for Client-side Web Applications | Google Identity  


## Systems

Most [web frameworks](index.md#frameworks) provide auth out of the box, or a way to integrate auth. 

The exact system will vary from project to project and team to team.

The underlying concepts and core requirements stay the same.

The Auth system must run on a server operated by the service being protected. It will always be part of an API. 

## Terms

API == Application Programming Interface == server side == back end

Client == browser == ui == javascript 

SSO == Single Sign On == Authentication Server

## Process Overview

The process starts on the Client Side when the user requests to log in to start a new session. 

Client redirects to login service with a return route for where you want the service to call your API so you can validate the token in the service system you provide. 

'login' vs 'sign in' vs 'verify' vs 'user' vs 'person'

vs

'sign up' vs 'register' vs 'new account'

TODO - path to client side login component
```
ui/src/pages/login.vue
```

Initiates SSO requests by redirecting the browser to the centralized authentication server with a return service to call once the user authenticates. 

The SSO server redirects the browser back to the calling service with a token. The return token is passed as a GET parameter on the url. 

TODO - path to client side verification component (if different from login)
```
ui/src/pages/login.vue
```

Then, the client passes the token it got back from SSO down to the API (to initiate the session). 

[ Server request happens in here] 

API processes the token, and then works with the UI client to show relevant data to the user who authenticated with authentication server. 

The API also provides a JWT that gets passed back to the client. 

The location the JWT gets stored is handled by logic on the client side.

TODO - JWT is currently stored in

Inspector -> Storage -> Local Storage 

Inspector -> Storage -> Cookies

Removing the JWT will log out the session

## Schemas

Most auth systems assume you'll bring your own database. 

When setting up a new system, it's a good time to include migrations. 

TODO  
[Sequelize](sequelize.md)

```
user: {
  id:
  username
  name
  roles: []
}
```

Django has a well designed permission system.

## Testing

[Cypress Testing](/code/testing/cypress.md#authentication)

## Related processes

Auth touches on many related topics

### Redirects

If a user requests a protected resource, first they'll need to authenticate with the system. It's always best when the system remembers where they were headed and redirects back once the auth process completes. 

Make use of the client's local storage to remember where to return. 

Redirects are processed / handled by `ui/src/pages/login.vue`

But where should they be initiated (e.g. added to the URL)
On the API side when a token verification is made.

### Sessions

Sessions keep track of someone after they've logged in to the system. 

JWT form the foundation for most authenticated sessions these days. 

Cookies are another solution for browser based sessions. 

### Analytics


### Identity Access Management

Umbrella term for systems that manage permissions for organizations?

LDAP, SAML

https://www.openiam.com/  
Home - OpenIAM - Open Source Identity Governance & Administration, Web Access Management, MFA and CIAM Platform  
https://duckduckgo.com/?t=ffab&q=IAM+vs+LDAP&ia=web  
IAM vs LDAP at DuckDuckGo  
https://medium.com/@robert.broeckelmann/authentication-vs-federation-vs-sso-9586b06b1380  
Authentication vs. Federation vs. SSO | by Robert Broeckelmann | Medium  
https://stackoverflow.com/questions/43987531/difference-between-active-directory-and-identity-and-access-managment  
Difference between Active directory and Identity and Access managment - Stack Overflow  


## Solutions / Topics

A wealth of solutions available:

https://github.com/topics/oauth2  
oauth2 · GitHub Topics  
https://github.com/topics/authentication  
authentication · GitHub Topics  
https://github.com/topics/authorization  
authorization · GitHub Topics  
https://github.com/topics/sso-authentication  
sso-authentication · GitHub Topics  

https://github.com/topics/user-management
user-management · GitHub Topics · GitHub

https://github.com/topics/users
users · GitHub Topics · GitHub

https://github.com/topics/iam
iam · GitHub Topics

https://github.com/topics/identity
identity · GitHub Topics


## Environment Documentation

When documenting your environment, include references to which providers you are using. 

TODO   
SSO resources / links :

## Libraries

### Grant

https://github.com/simov/grant  
simov/grant: OAuth Proxy  

A very popular library that could be applied in any number of Javascript API contexts. 

[Feathers](feathers.md) uses this one under the hood. 

### Ory

Ory looks like a great hardened solution. Given the importance of getting auth correct, I'd prefer to make use of a robust system.

A good overview:

https://www.ory.sh/docs/ecosystem/projects/

Setting up Oauth2 provider with Ory:

https://www.ory.sh/run-oauth2-server-open-source-api-security/

Check for open ports

```
sudo ss -atuln | grep '9000\|9001\|9010\|9020'
```

This looks like a well configured container setup for Ory modules

https://github.com/radekg/ory-reference-compose  
radekg/ory-reference-compose: Reference ORY Docker Compose setup  
https://gruchalski.com/posts/2021-04-10-ory-reference-docker-compose-and-thoughts-on-the-platform/  
ORY reference Docker Compose and thoughts on the platform | gruchalski.com  
https://duckduckgo.com/?t=ffab&q=ory+docker+compose&ia=web  
ory docker compose at DuckDuckGo  
https://raw.githubusercontent.com/radekg/ory-reference-compose/master/compose/compose.yml  
raw.githubusercontent.com/radekg/ory-reference-compose/master/compose/compose.yml  

https://duckduckgo.com/?t=ffab&q=oauth2+ory&ia=web  
oauth2 ory at DuckDuckGo  
https://github.com/ory/hydra-login-consent-node  
ory/hydra-login-consent-node: This is an ExpressJS reference implementation for the ORY Hydra User Login and Consent interface written in TypeScript and ExpressJS.  


### Authelia

Container ready SSO provider with 2 Factor Authentication (2FA). Works behind reverse-proxies.

One potential gotcha:

> The only way Authelia can share information about the authenticated user currently is through the use of four HTTP headers: Remote-User, Remote-Name, Remote-Email and Remote-Groups. Those headers are returned by Authelia on requests to /api/verify and must be forwarded by the reverse proxy to the backends needing them.

https://www.authelia.com/docs/deployment/supported-proxies/  
Proxy Integration - Authelia  

Should be easy enough to consume those headers as needed. 

How to tie this in with an OAuth model? 

https://github.com/authelia/authelia  
authelia/authelia: The Single Sign-On Multi-Factor portal for web apps  
https://github.com/authelia/authelia/blob/master/examples/compose/lite/docker-compose.yml  
authelia/docker-compose.yml at master · authelia/authelia  
https://www.authelia.com/  
Authelia - Authentication server providing two-factor and SSO  
https://www.authelia.com/docs/  
Home - Authelia  
https://www.authelia.com/docs/features/  
Features - Authelia  
https://www.authelia.com/docs/home/architecture.html  
Architecture - Authelia  
https://www.authelia.com/docs/getting-started.html  
Getting Started - Authelia  
https://www.authelia.com/docs/deployment/  
Deployment - Authelia  
https://www.authelia.com/docs/deployment/deployment-lite.html  
Deployment - Lite - Authelia  

https://duckduckgo.com/?t=ffab&q=authelia+integrate+with+API&ia=web  
authelia integrate with API at DuckDuckGo  

https://www.authelia.com/docs/home/architecture.html  
Architecture - Authelia  
https://www.authelia.com/docs/configuration/session/  
Session - Authelia  
https://duckduckgo.com/?t=ffab&q=authelia+get+user+from+session+cookie&ia=web  
authelia get user from session cookie at DuckDuckGo  
https://www.reddit.com/r/selfhosted/comments/mcsuwq/authelia/  
Authelia ? : selfhosted  
https://medium.com/@findpritish/protect-your-application-on-kubernetes-with-authelia-4761c35d8ef4  
Protect your application on kubernetes with authelia | by Pritish Payaningal | Medium  
https://github.com/findpritish/k8s/blob/master/authelia/07_test_application_authelia.yaml  
k8s/07_test_application_authelia.yaml at master · findpritish/k8s  
https://github.com/findpritish/k8s  
findpritish/k8s: Kubernetes  
https://speakerdeck.com/thockin/  
Tim Hockin (@thockin) on Speaker Deck  
https://github.com/findpritish/k8s/tree/master/authelia  
k8s/authelia at master · findpritish/k8s  
https://www.authelia.com/docs/configuration/session/  
Session - Authelia  
https://www.authelia.com/docs/configuration/session/  
Session - Authelia  
https://www.google.com/search?channel=fs&client=ubuntu&q=authelia+get+user+from+session+cookie  
authelia get user from session cookie - Google Search  

### Keycloak

Seems to be a standard choice. 

https://www.keycloak.org/

Sounds big based on: 

https://gruchalski.com/posts/2021-04-10-ory-reference-docker-compose-and-thoughts-on-the-platform/  
ORY reference Docker Compose and thoughts on the platform | gruchalski.com  
