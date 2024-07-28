# Supabase

Postgres based API server. 

A well architected, open-source, containerized stack for managing the back-end of your application. 

https://github.com/supabase/supabase  
GitHub - supabase/supabase: The open source Firebase alternative. Follow to stay updated about our public Beta.  

https://supabase.com/docs/guides/getting-started/architecture  
Architecture | Supabase Docs  

## Hosted

Nice to have something available publicly

https://supabase.com/dashboard/

First project is free! (!!)

Configure your organization. If it's just you for now, use your first name. You can rename the organization later. 

Configure your project

Note the database password somewhere secure (e.g. [keepass](/system/password-manager.md))



## Server

For running locally, use docker.

https://supabase.com/docs/guides/self-hosting/docker  
Self-Hosting with Docker | Supabase Docs  

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


I'm not sure if this is necessary in `.env`, but it works:

```
ADDITIONAL_REDIRECT_URLS="http://localhost:5173/**"
```

https://supabase.com/docs/guides/auth#redirect-urls-and-wildcards

> WARNING: While the "globstar" (**) is useful for local development and preview URLs, we recommend setting the exact redirect URL path for your site URL in production.



```
# Start
docker compose up
```

Go to the host you configured: 192.168.1.2:3000 for the studio  
or configure your client to use the API 192.168.1.2:8000  

https://supabase.com/docs/guides/self-hosting#api-keys  
Self-Hosting | Supabase Docs  


## New Project

Once you have access to a Supabase instance, it's time to set up your project. 



## Client (JS)

https://github.com/supabase/supabase-js  
GitHub - supabase/supabase-js: An isomorphic Javascript client for Supabase.

https://supabase.com/docs/guides/getting-started/tutorials/with-vue-3

https://supabase.com/docs/guides/getting-started/quickstarts/vue

Add library to your project

```
npm install @supabase/supabase-js
```

Configure your `.env` file to have the necessary details:

```
VITE_SUPABASE_URL=YOUR_SUPABASE_URL
VITE_SUPABASE_ANON_KEY=YOUR_SUPABASE_ANON_KEY
```

Create a `src/lib/supabaseClient.js` helper file to initialize the Supabase client. These variables are exposed on the browser, and that's completely fine since we have Row Level Security enabled on our Database.

```
import { createClient } from '@supabase/supabase-js'

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY

export const supabase = createClient(supabaseUrl, supabaseAnonKey)
```

From here, you can import the helper to make use of the supabase client.
Use the client in `App.vue` 

```js
<script setup>
import { ref, onMounted } from 'vue'
import { supabase } from './lib/supabaseClient'

const artists = ref([])

async function getArtists() {
  const { data } = await supabase.from('artist').select()
  artists.value = data
}

onMounted(() => {
  getArtists()
})
</script>

<template>
  <ul>
    <li v-for="artist in artists" :key="artist.id">{{ artist.name }}</li>
  </ul>

</template>
```


## Auth

There are many different ways to handle Authentication. I often like to start with the humble username / password to test things out. Use the admin interface to add the user, note the password someplace safe. 

Then initiate the client on the client side:

```
async function signInWithEmail() {
  const { data, error } = await supabase.auth.signInWithPassword({
    email: 'example@email.com',
    password: 'example-password',
  })
}
```

Reminder: you can always pass those parameters in from a web form as needed


https://supabase.com/docs/guides/auth/auth-email  
Login With Email | Supabase Docs  


Also good strategies for managing user data:

https://supabase.com/docs/guides/auth/managing-user-data


On the API side, Supabase uses GoTrue for auth in the stack. 


It may be possible to make use of Supertokens from a Supabase instance. 

https://duckduckgo.com/?t=ffab&q=supabase+and+supertokens&atb=v343-1&ia=web  
supabase and supertokens at DuckDuckGo  
https://supabase.com/docs/guides/integrations/supertokens  
SuperTokens | Supabase Docs  
https://duckduckgo.com/?q=supabase+docker+compose&t=ffab&atb=v343-1&ia=web  
## User management

https://supabase.com/docs/guides/getting-started/quickstarts/vue  
Use Supabase with Vue | Supabase Docs  
https://supabase.com/docs/guides/getting-started/tutorials/with-vue-3  
Build a User Management App with Vue 3 | Supabase Docs  
https://github.com/supabase/supabase/tree/master/examples/user-management/vue3-user-management  
supabase/examples/user-management/vue3-user-management at master · supabase/supabase · GitHub  
https://duckduckgo.com/?t=ffab&q=supabase+User+Management+Starter&atb=v343-1&ia=web  
supabase User Management Starter at DuckDuckGo  




## Client (Python)

Community supported library.

```
pipenv shell
pipenv install supabase
pipenv install python-dotenv
```

Test your connection with code like the following

```
from typing import Union
from fastapi import FastAPI
import os
from supabase import create_client, Client
from dotenv import load_dotenv

load_dotenv()

# https://stackoverflow.com/questions/40216311/reading-in-environment-variables-from-an-environment-file
url: str = os.environ.get("SUPABASE_URL")
key: str = os.environ.get("SUPABASE_KEY")
# for these, load values into the actual shell env to work:
#url: str = os.getenv("SUPABASE_URL")
#key: str = os.getenv("SUPABASE_KEY")

# print("Connecting to:", url, key)
supabase: Client = create_client(url, key)

app = FastAPI()

data = supabase.table("artist").select("*").execute()
print("Data", data)

@app.get("/")
def read_root():
    return {"Hello": "World"}
```

https://supabase.com/docs/reference/python/select

https://dev.to/jakewitcher/using-env-files-for-environment-variables-in-python-applications-55a1

## Database

Supabase makes heavy use of Postgresql. There is a reason it does not support a different database. And it doesn't need to. Postgresql does the heavy lifting here. See the years and years of documentation on the project. 

### Multiple schemas

In a new schema, in order to be able to create new tables, functions and sequences you need to grant access to the right roles / users. 

```
create schema if not exists reddit;
```
        
-- Grant access to default roles
grant usage on schema reddit to postgres, anon, authenticated, service_role;
```
alter default privileges in schema reddit grant all on tables to postgres, anon, authenticated, service_role;
alter default privileges in schema reddit grant all on functions to postgres, anon, authenticated, service_role;
alter default privileges in schema reddit grant all on sequences to postgres, anon, authenticated, service_role;
```
        
-- Grant access to default roles for anything new created by supabase admin
grant usage on schema reddit to postgres, anon, authenticated, service_role;
```
alter default privileges in schema reddit grant all on tables to postgres, anon, authenticated, service_role;
alter default privileges in schema reddit grant all on functions to postgres, anon, authenticated, service_role;
alter default privileges in schema reddit grant all on sequences to postgres, anon, authenticated, service_role;
```

An don't forget to expose the schemas at the end in the API settings of your supabase project.

Via:
https://www.reddit.com/r/Supabase/comments/rluwrs/multiple_schemas/ 

### Backups

Supabase configures a lot of "best practice" default settings for you in your database. 

TODO: Test exporting data and importing it into a newly created instance.
I believe that is the best path in a recovery situation.

```
pg_dump -U postgres supabase > dbexport.pgsql
```

See also [postgres](postgresql.md#backups) for a more complete example.

Running backups using `postgres` user results in:

```
account@system:~/supabase/docker$ ./backup-db.sh 
pg_dump: error: query failed: ERROR:  permission denied for table schema_migrations
pg_dump: detail: Query was: LOCK TABLE _realtime.schema_migrations IN ACCESS SHARE MODE
```

When looking at the configured users and roles, it's clear `postgres` doesn't have what it takes:

Here is the relevant output from the `\du` command:

```
postgres                   | Create role, Create DB, Replication, Bypass RLS            | {pg_monitor,anon,authenticated,service_role,supabase_auth_admin,supabase_functions_admin,supabase_storage_admin,pgsodium_keyiduser,pgsodium_keyholder,pgsodium_keymaker}
 service_role               | No inheritance, Cannot login, Bypass RLS                   | {}
 supabase_admin             | Superuser, Create role, Create DB, Replication, Bypass RLS | {}

```


Use the `supabase_admin` account to manage backups. The `postgres` user does not have full permissions (by design). To use `supabase_admin`, grant it a secure password


```
cd supabase/docker/
dce db bash
```

Switch to the PostgreSQL user:

Once you're in the Docker container's shell, switch to the postgres user, which will allow you to access the PostgreSQL instance without a password:

```
su - postgres
```

Change the supabase_admin password:
	
Access the PostgreSQL prompt:

``` bash
psql
```

And then set a new password for supabase_admin:

``` sql
ALTER USER supabase_admin WITH PASSWORD 'newpassword';
```

The hosted service offers backups automatically. (at least 7 days)



### Prisma

https://duckduckgo.com/?t=ffab&q=supabase+and+prisma+&atb=v343-1&ia=web  
supabase and prisma at DuckDuckGo  
https://supabase.com/docs/guides/integrations/prisma  
Prisma | Supabase Docs  


## Links

https://supabase.com/docs  
Supabase Docs  

https://supabase.com/docs/guides/storage  
Storage | Supabase Docs  
  


https://supabase.com/docs/guides/api  
Serverless APIs | Supabase Docs  
https://supabase.com/docs/guides/database/overview  
Database | Supabase Docs  

  
https://supabase.com/docs/guides/functions  
Edge Functions | Supabase Docs  
https://supabase.com/docs/guides/storage/image-transformations  
Storage Image Transformations | Supabase Docs  
https://supabase.com/docs/guides/auth/row-level-security  
Row Level Security | Supabase Docs  
  

