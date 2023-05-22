# APIs

Application Programming Interfaces

The place where you persist data (See Also: persistence, storage)

For relationships and metadata, use a real database. Postgres seems hard to beat for this.

For documents that change, use git and .md

## Supabase

[Supabase](supabase.md)


## Auth

[auth](auth.md)


## DBs 

DBs are Databases.

Databases tie in closely with the API.


### Schema & Models

[Schema](schema.md)

#### Schema Diagrams

AKA Entity Relationship Diagrams

##### Diagrams.net 

supports importing a sql file

https://stackoverflow.com/questions/4108816/entity-relationship-diagram-software  
database - Entity relationship diagram software - Stack Overflow  


## Relational Databases

### PostgreSQL

[PostgreSQL](postgresql.md)

### SQLite

[SQLite](sqlite.md)

### MySQL

[MySQL](mysql.md)


### ORM

[Prisma](prisma.md) for an ORM for working with a database from an application. 

### Manual Migrations

If you haven't configured an actual migration system, track changes using a simple text file to note the SQL commands that get applied

Use SQL commands to `ALTER` tables





## Document Databases

aka object databases

[mongo](mongo.md)


### In Memory

[redis](redis.md)  



## Message Queues

[Message Queues](message-queue.md)

## Frameworks

https://github.com/typicode/json-server#static-file-server  
typicode/json-server: Get a full fake REST API with zero coding in less than 30 seconds (seriously)  

https://github.com/typicode/lowdb  
typicode/lowdb: Simple to use local JSON database (supports Node, Electron and the browser)  

[Search](search.md)

[Express Server](express.md)  

[Sending Email](email-sending.md)

[Image Gallery](image-gallery.md)




## Data Files

Big files don't change often. 

Generate their id with IPFS or some other checksum strategy

[ipfs](ipfs.md)


