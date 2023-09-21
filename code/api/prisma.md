# Prisma

A nice, concise syntax for representing a model in javascript (and python!). Way to go!

https://www.prisma.io/docs/concepts/components/prisma-schema  
Prisma schema (Reference)  
https://www.prisma.io/docs/concepts/components/prisma-client  
Prisma Client  

https://www.prisma.io/docs/concepts/components/prisma-client/crud#create-multiple-records  
CRUD (Reference)  
https://www.prisma.io/docs  
Prisma Documentation | Concepts, Guides, and Reference  

https://github.com/prisma  
Prisma  
https://www.prisma.io/  
Prisma | Next-generation ORM for Node.js & TypeScript  



##   
https://github.com/prisma/prisma  
GitHub - prisma/prisma: Next-generation ORM for Node.js & TypeScript | PostgreSQL, MySQL, MariaDB, SQL Server, SQLite, MongoDB and CockroachDB  

https://github.com/prisma/studio  
GitHub - prisma/studio: 🎙️ The easiest way to explore and manipulate your data in all of your Prisma projects.  

## Introspection

Get an existing schema out of a previously created database

https://www.prisma.io/docs/getting-started/setup-prisma/add-to-existing-project/mongodb/introspection-typescript-mongodb

```
npx prisma init --url mongodb://root:example@boilerplate_mongo:27017/db?authSource=admin&readPreference=primary&ssl=false&directConnection=true
```

then pull

```
npx prisma db pull
```

