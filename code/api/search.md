# Search

## API

https://github.com/meilisearch/meilisearch

```
docker run -p 7700:7700 -v "$(pwd)/meili_data:/meili_data" getmeili/meilisearch
```

or in docker-compose

```
  search:
    # https://docs.meilisearch.com/learn/getting_started/quick_start.html#setup-and-installation
    image: getmeili/meilisearch:latest
    container_name: boilerplate_search
    restart: unless-stopped
    environment:
      MEILI_MASTER_KEY: MAIN_KEY
    volumes:
      - ./db_search:/srv/search
    ports:
      - 7700:7700
    working_dir: /srv/search
    command: meilisearch --env="development" 
```

should be able to see a successful start

```
dcl search
```

and access the API via a browser directly

```
http://localhost:7700
```

Now it's time to add documents.
Documents need to be JSON, NDJSON (json on one line), or CSV

https://docs.meilisearch.com/learn/core_concepts/documents.html#primary-field

https://docs.meilisearch.com/learn/getting_started/quick_start.html#add-documents

```
curl \
  -X POST 'http://localhost:7700/indexes/movies/documents' \
  -H 'Content-Type: application/json' \
  --data-binary @movies.json
```

The data being sent should be a list of objects (even if it's a list of one object)

Add in the API key:

```
curl -X POST 'http://localhost:7700/indexes/movies/documents' -H 'Content-Type: application/json' -H 'Authorization: Bearer MAIN_KEY' --data-binary @/home/account/Downloads/movies.json
```

## Client

Frequently ties in with a client side widget

[ui search](/code/javascript/search.md)
