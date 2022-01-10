# APIs

Application Programming Interfaces

The place where you persist data (See Also: persistence, storage)

For mostly static binaries, use IPFS (fixed paths on the filesystem is ideal. Need a place to safely track metadata)

For documents that change, use git and .md

For relationships and metadata, use a real database. Postgres seems hard to beat for this.

## Schema & Models

[Schema](schema.md)

## DBs 

DBs are Databases.

Databases tie in closely to the API side.

### Relational Databases

[Relational Databases](relational-db.md)

[sequelize](sequelize.md)

### Document Databases


[mongo](mongo.md)

## Deployment

When it comes time to deploy databases, separate containers are ideal. See also system-architecture notes on web-ui-api-db:

https://gitlab.com/charlesbrandt/web-ui-api-db/ui/content/


## Auth

[auth](auth.md)


## Frameworks

[feathers](feathers.md)

[image gallery](image-gallery.md)

[ipfs](ipfs.md)

[search](search.md)

[express](express.md)  
[Express Server](express.md)  


### File Browser

[File Browser](file-browser.md)


