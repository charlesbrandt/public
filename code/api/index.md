# APIs

Application Programming Interfaces

The place where you persist data (See Also: persistence, storage)

For mostly static binaries, use IPFS (fixed paths on the filesystem is ideal. Need a place to safely track metadata)

For documents that change, use git and .md

For relationships and metadata, use a real database. Postgres seems hard to beat for this.

## DBs 

DBs are Databases.

Databases often tie in closely to the API side.

[databases](databases.md)

[mongo](mongo.md)

[sequelize](sequelize.md)


## Auth

[auth](auth.md)


## Frameworks

[feathers](feathers.md)

[image gallery](image-gallery.md)

[ipfs](ipfs.md)

[search](search.md)


### File Browser

[File Browser](file-browser.md)


## Documenting

Document them.

Try Open API (previously known as Swagger). 

Or just text.

The best way to ensure that your API documentation is current and accurate is to embed it within your API implementation and then generate the documentation using literate programming techniques, or a framework such as http://apidocjs.com/, http://swagger.io/, or http://raml.org/index.html.

via:
https://apiguide.readthedocs.io/en/latest/build_and_publish/documentation.html
Document your API — API Design Guide 0.1 documentation

https://swagger.io/  
The Best APIs are Built with Swagger Tools | Swagger  
https://swagger.io/resources/open-api/  
API Resources | Swagger  
https://swagger.io/docs/specification/about/  
About Swagger Specification | Documentation | Swagger | Swagger  
http://editor.swagger.io/?_ga=2.216992021.268614673.1581966607-50034279.1581346793  
Swagger Editor  
https://raml.org/index.html  
Welcome | RAML  
