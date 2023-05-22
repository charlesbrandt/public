# Supabase

## Server

For running locally, use docker.

```
# Get the code
git clone --depth 1 https://github.com/supabase/supabase

# Go to the docker folder
cd supabase/docker

# Copy the fake env vars
cp .env.example .env

micro .env

```

Generate unique keys using functionality on this page:

https://supabase.com/docs/guides/self-hosting#api-keys

replaceall localhost 192.168.1.2

Change:
```
STUDIO_DEFAULT_ORGANIZATION=Default Organization
STUDIO_DEFAULT_PROJECT=Default Project
```

Once you've edited your `.env` file, be sure to also edit your Kong config to use matching `anon` and `service_role` keys.

```
micro volumes/api/kong.yml
```

> This was a tricky point to find. Many thanks to this blog post for helping sort it out: 
> https://blog.devgenius.io/how-to-self-host-supabase-a-complete-guide-f4c68f449920


I'm not sure if this is necessary in `.env`, but it works:

```
ADDITIONAL_REDIRECT_URLS="http://localhost:5173/**"
```

https://supabase.com/docs/guides/auth#redirect-urls-and-wildcards

> WARNING: While the "globstar" (**) is useful for local development and preview URLs, we recommend setting the exact redirect URL path for your site URL in production.

http://localhost:3000/**



```
# Start
docker compose up
```

Go to the host you configured: 192.168.1.2:3000 for the studio
or configure your client to use the API 192.168.1.2:8000

## Client

https://supabase.com/docs/guides/getting-started/tutorials/with-vue-3

## Auth

Supabase uses GoTrue for auth in the stack. 


It may be possible to make use of Supertokens from a Supabase instance. 

https://duckduckgo.com/?t=ffab&q=supabase+and+supertokens&atb=v343-1&ia=web  
supabase and supertokens at DuckDuckGo  
https://supabase.com/docs/guides/integrations/supertokens  
SuperTokens | Supabase Docs  
https://duckduckgo.com/?q=supabase+docker+compose&t=ffab&atb=v343-1&ia=web  
## Prisma

https://duckduckgo.com/?t=ffab&q=supabase+and+prisma+&atb=v343-1&ia=web  
supabase and prisma at DuckDuckGo  
https://supabase.com/docs/guides/integrations/prisma  
Prisma | Supabase Docs  


## Links

https://supabase.com/docs  
Supabase Docs  
https://supabase.com/docs/guides/auth/auth-email  
Login With Email | Supabase Docs  
https://supabase.com/docs/guides/api  
Serverless APIs | Supabase Docs  
https://supabase.com/docs/guides/database/overview  
Database | Supabase Docs  


https://supabase.com/docs/guides/getting-started/quickstarts/vue  
Use Supabase with Vue | Supabase Docs  
https://supabase.com/docs/guides/getting-started/tutorials/with-vue-3  
Build a User Management App with Vue 3 | Supabase Docs  
https://github.com/supabase/supabase/tree/master/examples/user-management/vue3-user-management  
supabase/examples/user-management/vue3-user-management at master · supabase/supabase · GitHub  
https://supabase.com/docs/guides/getting-started/tutorials/with-vue-3#get-the-api-keys  
Build a User Management App with Vue 3 | Supabase Docs  
