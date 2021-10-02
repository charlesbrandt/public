## Terms

API == Application Programming Interface == server side == back end

Client == browser == ui == javascript 

SSO == Single Sign On == Authentication Server

## Environment Description

TODO
SSO resources / links :


## Process Overview

The process starts on the Client Side when the user requests to log in to start a new session. 

Client redirects to login service with a redirect for where you want the service to call your API so you can validate the token in the service system you provide. 

'login' vs 'sign in' vs 'verify' vs 'user' vs 'person'

vs

'sign up' vs 'register' vs 'new account'

TODO - path to client side login component
```
ui/src/pages/login.vue
```

Initiates SSO requests by redirecting the browser to the centralized authentication server with a return service to call once the user authenticates. 

The SSO server redirects the browser back to the calling service with a token (TODO: where is that return token stored? as a GET parameter on the url?? )

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

Inspector -> Storage -> Cookies

Inspector -> Storage -> Local Storage 

Removing the JWT will log out the session


#### Redirects

Redirects are processed / handled by 
ui/src/views/Signin.vue

But where should they be initiated (e.g. added to the URL)
On the API side when a request for protected information is made? 

/opt/sca/docker/apps/cmg/ui/src/router/index.js

