# Sequelize

A Javascript based Object Relational Mapper (ORM) for tracking the relationship between a database schema and data objects used in software. (e.g. JSON representation)

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

## Model Files

For the file name of model files, I like using the pattern `model_name.model.js` 

### Index

For index fields, you can specify 

```
       id: {
            type: Sequelize.INTEGER,
            autoIncrement: true,
            primaryKey: true
        },
```

Remember, when sending API calls to create new records, be sure NOT to include an `id` parameter if the model's `id` is set to be automatically updated. The error is a bit cryptic:

```
SequelizeDatabaseError: Field 'id' doesn't have a default value
```


https://stackoverflow.com/questions/33775165/auto-increment-id-with-sequelize-in-mysql

> You must be sure you're not even sending the id key at all.



## Schema Generation

Sequelize Auto can help generate schemas from an existing database

https://github.com/sequelize/sequelize-auto  
sequelize/sequelize-auto: Automatically generate bare sequelize models from your database.  

TODO - document how to incorporate these in with an API  
fully automated vs grouping associations in with the model file  

TODO - Output should be closer to the Feather's model definition approach  
Include the associations in with the model  


## Migrations

https://sequelize.org/master/manual/migrations.html

Migrations keep your database schema in sync with the requirements of the current system architecture. 

The following assumes you have a `migrations` folder in the root of your app. (`api/migrations`)


### Initial Setup: one-time tasks

Configure dependencies in the API container. The database container may not have / need node installed. Install the [sequelize CLI](https://github.com/sequelize/cli):
 
```
docker-compose exec api bash

cd ../migrations
yarn add sequelize-cli --dev # npm install --save-dev sequelize-cli
```

Set up necessary files

```
npx sequelize-cli init
```

edit the `config/config.json` file to reflect your environment. 


### Migration definitions

Migrations make use of the Sequelize Query Interface to effect schema changes in the database:

https://sequelize.org/master/manual/query-interface.html

Create the migration file skeleton: 

```
docker-compose exec api bash
cd ../

npx sequelize-cli migration:generate --name user-columns
```

It may be necessary to update permissions at the host level if the container writes new files as root. (Or just create the files manually outside of the container -- not that much to them). 

```
sudo chown -R account: api/migrations/
```

Follow along with:

https://sequelize.org/master/manual/migrations.html#migration-skeleton

Note: the database table name needs to be used here (e.g. `users` below). The `queryInterface` hasn't applied model name mappings yet. 

example -- adding columns:

```
		return queryInterface.sequelize.transaction((t) => {
			return Promise.all([
				queryInterface.addColumn(
					"users",
					"username",
					{
						type: Sequelize.DataTypes.STRING,
					},
					{ transaction: t }
				),
			]);
		});
                
```

https://sequelize.org/v5/manual/data-types.html


and don't forget to do the reverse operation in `down`:

```
		return queryInterface.sequelize.transaction((t) => {
			return Promise.all([
				queryInterface.removeColumn("users", "username", { transaction: t }),
                ])
```


### Migration process

Apply the migrations with:

```
npx sequelize-cli db:migrate
```

Undo the migrations with:

```
npx sequelize-cli db:migrate:undo
```

### Seeds

```
npx sequelize-cli db:seed:all
```


### Migration Examples

#### Raw SQL commands

Sometimes there are commands that needs to be run that don't have a query builder equivalent:

```
"use strict";
module.exports = {
	up: async (queryInterface, Sequelize) => {
		await queryInterface.sequelize.query("SET FOREIGN_KEY_CHECKS = 0");
		await queryInterface.sequelize.query(
			"ALTER TABLE table MODIFY table_id INTEGER NOT NULL AUTO_INCREMENT"
		);
		await queryInterface.sequelize.query(
			"ALTER TABLE table AUTO_INCREMENT = 167"
		);
		await queryInterface.sequelize.query("SET FOREIGN_KEY_CHECKS = 1");
	},
	down: async (queryInterface, Sequelize) => {
		// put things back the way they were (minus data)
	},
};

```

### See Also

[relational databases](relational-db.md)

[Sequelize · feathers-docs-common](https://eddyystop.gitbooks.io/feathers-docs-common/content/databases/sequelize.html#migrations)

Having trouble locating the original repo, and it seems this is an earlier version of the feathers docs that got cut. 



## Queries

https://sequelize.org/master/manual/model-querying-basics.html

TODO - Seems very similar to mongo API. How does it compare? 

### Select

```
Model.findAll({
  attributes: ['foo', 'bar']
});
```

`findAll` can also be used in a search capacity

```
	db.item
		.findAll({
			where: {
				name: { [Op.like]: `%${req.params.query}%` },
			},
		})

```



### Insert

```
const jane = await User.create({ firstName: "Jane", lastName: "Doe" });
console.log("Jane's auto-generated ID:", jane.id);
```



## API Integration

Roughly follows actions that a feathers server provides by default

```
var express = require("express");
var router = express.Router();
var db = require("../db");

/* GET the associated records of a project */
router.get("/:id", function (req, res, next) {
	db.project
		.findOne({
			where: { _id: req.params.id },
			include: [
				{
					model: db.sample,
					as: "samples",
					include: [
						{ model: db.datafile, as: "files" },
						{ model: db.source, as: "source" },
					],
				},
				{ model: db.datafile, as: "files" },
			],
		})
		.then((match) => {
			res.json(match);
		});
});

router.patch("/:id", function (req, res, next) {
	db.project
		.findOne({
			where: { project_id: req.params.id },
		})
		.then((match) => {
			console.log("Found a match", match);
			match.update(req.body);
			console.log("match updated", match);
			res.json(match);
		})
		.catch((err) => {
			next(err);
		});

	// this approach is not working for me
	// maybe it requires all fields to be passed in for the update to work?
	// https://medium.com/@sarahdherr/sequelizes-update-method-example-included-39dfed6821d
	// db.project
	// 	.update(req.body, { returning: true, where: { project_id: req.params.id } })
	// 	.then(([rowsUpdated, [updatedItem]]) => {
	// 		res.json(updatedItem);
	// 	})
	// 	.catch((err) => {
	// 		next(err);
	// 	});
});

router.delete("/:id", function (req, res, next) {
	db.project
		.destroy({
			where: { _id: req.params.id },
		})
		.then((match) => {
			res.json({ result: "success" });
		})
		.catch((err) => {
			next(err);
		});
});

router.get("/", function (req, res, next) {
	db.project
		.findAll({
			include: [
				// { model: users, attributes: ["email"] },
				{
					model: db.sample,
					as: "samples",
					include: [
						{ model: db.datafile, as: "files" },
						{ model: db.source, as: "source" },
					],
				},
				{ model: db.datafile, as: "files" },

				{ model: db.investigator, as: "investigators" },
			],
		})
		.then((matches) => {
			res.json(matches);
		});
});

router.post("/", function (req, res, next) {
	// const newProject = JSON.parse(req.body);
	// console.log("CREATE NEW PROJECT", req.body);
	db.project
		.create(req.body)
		.then((created) => {
			res.json(created);
		})
		.catch((err) => {
			next(err);
		});
});

module.exports = router;

```


## Relations / Associations

Many of these can be defined automatically by generating the models from the database. (See Above)

With sequelize the behavior of populating a related object is called `include`.

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

### Many to Many

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



## Legacy Database Schema

Sometimes it's necessary to work with an existing schema. Ideally, apply database migrations as needed. 

https://sequelize.org/master/manual/legacy.html  
Manual | Sequelize  

It can be useful to assign different names to columns. 

```
class MyModel extends Model {}
MyModel.init({
  userId: {
    type: DataTypes.INTEGER,
    field: 'user_id'
  }
}, { sequelize });
```

When including related objects, it's possible to assign a different name. Use the 'as' parameter

```
			include: [
				{
					model: db.example,
					as: "examples",
                    ...
                    
```

https://duckduckgo.com/?t=ffab&q=sequelize+specify+column+name&ia=web  
sequelize specify column name at DuckDuckGo  
https://stackoverflow.com/questions/55114922/change-column-name-sequilize  
node.js - Change column name Sequilize - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=sequelize+specify+id&ia=web  
sequelize specify id at DuckDuckGo  
https://github.com/sequelize/sequelize/issues/741  
Don't have a column call id column · Issue #741 · sequelize/sequelize  


## CRUD Express Example

Replace `item` with your model

```
var express = require("express");
var router = express.Router();
var db = require("../db");
const { Op } = require("sequelize");

router.get("/find/:query", function (req, res, next) {
	db.item
		.findAll({
			where: {
				name: { [Op.like]: `%${req.params.query}%` },
			},
		})
		.then((match) => {
			console.log("Matched by find", match);
			res.json(
				match.map(({ name }) => {
					return { value: name };
				})
			);
		})
		.catch((err) => {
			next(err);
		});
});

/* GET the details of a item */
router.get("/:id", function (req, res, next) {
	db.item
		.findOne({
			where: { id: req.params.id },
			// without this, sequelize only returns one result?
			exclude: ["id"],
		})
		.then((match) => {
			res.json(match);
		})
		.catch((err) => {
			next(err);
		});
});

router.patch("/:id", function (req, res, next) {
	db.item
		.findOne({
			where: { id: req.params.id },
		})
		.then((match) => {
			console.log("Found a match", match);
			match.update(req.body);
			console.log("match updated", match);
			res.json(match);
		})
		.catch((err) => {
			next(err);
		});
});

router.delete("/:id", function (req, res, next) {
	db.item
		.destroy({
			where: { id: req.params.id },
		})
		.then((match) => {
			res.json({ result: "success" });
		})
		.catch((err) => {
			next(err);
		});
});

router.get("/", function (req, res, next) {
	db.item
		.findAll({})
		.then((matches) => {
			res.json(matches);
		})
		.catch((err) => {
			next(err);
		});
});

router.post("/", function (req, res, next) {
	// const newItem = JSON.parse(req.body);
	// console.log("CREATE NEW ITEM", req.body);
	db.item
		.create(req.body)
		.then((created) => {
			res.json(created);
		})
		.catch((err) => {
			next(err);
		});
});

module.exports = router;
```
