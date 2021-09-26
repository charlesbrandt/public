# Sequelize

As far as Javascript ORMs go, this seems to be the one most similar to other ORMs I've used in the past with Python. 

https://sequelize.org/
Sequelize ORM
https://sequelize.org/master/
Manual | Sequelize
https://github.com/sequelize/sequelize/
sequelize/sequelize: An easy-to-use multi SQL dialect ORM tool for Node.js
https://sequelize.org/master/manual/model-basics.html#numbers
Manual | Sequelize
https://sequelize.org/master/manual/migrations.html
Manual | Sequelize

https://github.com/feathersjs-ecosystem/feathers-sequelize

## Schema Generation

Sequelize Auto can help generate schemas from an existing database

https://github.com/sequelize/sequelize-auto
sequelize/sequelize-auto: Automatically generate bare sequelize models from your database.

TODO: how to incorporate these in with a feathers application? 
Any similarities / differences to note?


## Relations / Associations

Getting Sequelize to populate associated fields (relations) can be tricky, especially within the context of Feathers. With sequelize the behavior of populating a related object is called `include`.

http://docs.sequelizejs.com/en/latest/docs/associations/

In Feathers, associations are defined on the Models. These will then be initialized by sequelize via `api/src/sequelize.js`. 

Make use of "references" parameter to indicate the parent relationship, e.g.

```
    project_id: {
      type: DataTypes.STRING(255),
      allowNull: true,
      references: {
        model: 'research_project',
        key: 'project_id'
      }
    },

```

On both related models define the associations:

```
  project.associate = function (models) {
    // Define associations here
    // See http://docs.sequelizejs.com/en/latest/docs/associations/
    project.hasMany(models.datafile, {
      as: "files",
      foreignKey: "project_id",
    });

```

Depending on the association type, the foreignKey property may refer to the local object or the child object. 

`hasMany` will refer to a foreignKey on the child object
`belongsTo` will refer to a foreignKey on the local object




Then, when `find` or `get` is used to return results from the database, in Feathers it is important to create a hook that will `include` associated results. 

in `api/src/hooks/get-samples.js`

``` 
// eslint-disable-next-line no-unused-vars
module.exports = (options = {}) => {
  return async (context) => {
    const sequelize = context.app.get("sequelizeClient");
    const { sample } = sequelize.models;
    context.params.sequelize = {
      include: [
        // { model: users, attributes: ["email"] },
        { model: sample, as: "samples" },
      ],
      raw: false,
    };
    return context;
  };
};
```

This guide was helpful for using sequelize in Feathers JS
https://medium.com/@mohammedalrowad/feathersjs-association-hooks-with-sequelize-1825356b1843

### See also

These came up in my search for understanding how to include related objects in an api `find` or `get` 

https://github.com/feathersjs-ecosystem/feathers-sequelize/issues/20

The issue highlights an important point -- better documentation for getting up and running with Sequelize and Feathers. I got tripped up understanding that my service's hooks needed to inject the `include` parameters into the context. 


https://github.com/feathersjs/generator-feathers/issues/94#issuecomment-204165134

https://stackoverflow.com/questions/42841810/feathers-js-sequelize-service-with-relations-between-two-models/42846215#42846215

https://dreamdevourer.com/example-of-sequelize-associations-in-feathersjs/

## Many to Many

https://sequelize.org/master/manual/advanced-many-to-many.html

The trick was in initializing the through model

without creating a dedicated service for the through model
(although maybe that is desirable? ... can always add it in later)

I initialized manually in `api/src/sequelize.js` with:

``` 
const createThroughModel = require("./models/through.model.js");

module.exports = function (app) {

...

  app.setup = function (...args) {
    const result = oldSetup.apply(this, args);

    createThroughModel(app);
```

https://stackoverflow.com/questions/48602085/using-feathers-client-and-sequelize-many-to-many-relation

https://stackoverflow.com/questions/51827290/how-to-define-many-to-many-with-feathers-and-sequelize-with-additional-fields-in


## Error Handling (TODO)

I have been trying Sequelize out in the context of Feathers. So far my biggest complaint is not finding a clear way to see when configurations are having an error... it seems to fail silently

https://duckduckgo.com/?t=ffab&q=sequelize+error&ia=web
sequelize error at DuckDuckGo
https://stackoverflow.com/questions/35337738/where-to-handle-the-error-in-a-sequelize-orm-query-statement
javascript - Where to handle the error in a sequelize ORM query statement? - Stack Overflow
http://expressjs.com/en/guide/error-handling.html
Express error handling

## Migrations

Migrations is another important topic that comes up, especially when working with a data schema provided by someone else. 

If nothing else, just use a file to track the SQL commands that get applied

https://duckduckgo.com/?q=mysql+add+column&t=ffab&ia=web
mysql add column at DuckDuckGo
https://www.mysqltutorial.org/mysql-add-column/
How to Add Columns To A Table Using MySQL ADD COLUMN
https://phoenixnap.com/kb/how-to-rename-column-mysql
How to Rename a Column in MySQL {ALTER TABLE command}
https://www.mysqltutorial.org/mysql-drop-table
MySQL DROP TABLE


## Legacy Database Schema

Sometimes it's necessary to work with an existing schema. 

In this case, it can be useful to assign different names to columns

https://duckduckgo.com/?t=ffab&q=sequelize+specify+column+name&ia=web
sequelize specify column name at DuckDuckGo
https://stackoverflow.com/questions/55114922/change-column-name-sequilize
node.js - Change column name Sequilize - Stack Overflow
https://duckduckgo.com/?t=ffab&q=sequelize+specify+id&ia=web
sequelize specify id at DuckDuckGo
https://sequelize.org/master/manual/legacy.html
Manual | Sequelize
https://github.com/sequelize/sequelize/issues/741
Don't have a column call id column · Issue #741 · sequelize/sequelize

