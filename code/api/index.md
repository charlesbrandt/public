# APIs

Application Programming Interfaces

The place where you persist data (See Also: persistence, storage)

For mostly static binaries, use IPFS (fixed paths on the filesystem is ideal. Need a place to safely track metadata)

For documents that change, use git and .md

For relationships and metadata, use a real database. Postgres seems hard to beat for this.

## Frameworks

https://github.com/typicode/json-server#static-file-server  
typicode/json-server: Get a full fake REST API with zero coding in less than 30 seconds (seriously)  

https://github.com/typicode/lowdb  
typicode/lowdb: Simple to use local JSON database (supports Node, Electron and the browser)  

[Search](search.md)

[Express Server](express.md)  

[Feathers](feathers.md)

[Image Gallery](image-gallery.md)

[Sending Email](email-sending.md)

## Auth

[auth](auth.md)

## DBs 

DBs are Databases.

Databases tie in closely with the API.


### Schema & Models

[Schema](schema.md)


### Relational Databases

[Relational Databases](relational-db.md)

[sequelize](sequelize.md)


### Document Databases

aka object databases

[mongo](mongo.md)


### In Memory

[redis](redis.md)  


## Message Queues

[Message Queues](message-queue.md)


## Deployment

When it comes time to deploy databases, separate containers are ideal. See also system-architecture notes on web-ui-api-db:

https://gitlab.com/charlesbrandt/web-ui-api-db/ui/content/

## Data Files

Big files don't change often. 

Generate their id with IPFS or some other checksum strategy

[ipfs](ipfs.md)


