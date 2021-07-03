# MongoDB

Document Database

https://www.duckduckgo.com/search?q=mongodb
mongodb - Google Search
https://www.tutorialspoint.com/mongodb/mongodb_overview.htm
MongoDB - Overview - Tutorialspoint


## Connections

https://docs.mongodb.com/guides/server/drivers/
    
 
## Shell

If your container doesn't have it already, you can install the CLI with

    sudo apt-get install -y mongodb-clients

https://docs.mongodb.com/manual/mongo/

https://docs.mongodb.com/manual/reference/mongo-shell/

    mongo mongodb://$[hostlist]/$[database]?authSource=$[authSource] --username $[username]
    
or, for a local connection:

    mongo

See the current database:

    db

See all databases:

    show dbs
    
`use database_name`

To see all collections associated with a database:

    db.getCollectionNames()
    
https://stackoverflow.com/questions/8866041/how-can-i-list-all-collections-in-the-mongodb-shell

## Queries

Queries are the first step in most operations that specify the content to work with. 

https://docs.mongodb.com/manual/tutorial/query-documents/

Not sure what fields are in use in a given collection? 

    Object.keys(db.messages.findOne())
    
[via](https://stackoverflow.com/questions/5900792/how-to-view-document-fields-in-mongo-shell)

### Logical Operators

pass checks in as part of a list

	$and: [{ dataset: req.params.dataset }, { visible: false }],

https://docs.mongodb.com/manual/reference/operator/query-logical/

#### Existence

Sometime new fields may not exist in old records. Can check for a value or existence with:

	$or: [{ visible: true }, { visible: { $exists: false } }],

### Date range

return Episode.
      find({ airedAt: { $gte: '1987-10-19', $lte: '1987-10-26' } }).
      sort({ airedAt: 1 });
  })
  
https://mongoosejs.com/docs/tutorials/dates.html

### Null Fields

Match records that do not have a matching key:

    db.collection.find( { key_name : { $exists: false } } )

https://docs.mongodb.com/manual/tutorial/query-for-null-fields/

### Nested Fields

Place the full path to the field in quotes to use it in a query

    "paths.staged": "",

https://stackoverflow.com/questions/19889313/mongodb-query-in-on-a-hash-field

### Regular Expressions

https://docs.mongodb.com/manual/reference/operator/query/regex/#mongodb-query-op.-regex

Not working:
{createdAt: { $regex: /2021-06-30*/ } }

Seems like searching by a date range is a cumbersome process? 

https://stackoverflow.com/questions/43996930/regex-in-mongodb-for-iso-date-field


## Find
        
    db.collection.find()
    
Find accepts a json style dictionary/hash object of search parameters (queries)

https://docs.mongodb.com/manual/reference/method/db.collection.find/index.html

To iterate over the matched results immediately (vs returning all via an API):

```
for await (const doc of Person.find()) {
  console.log(doc)
  doc.set('field', 'value')
  await doc.save()
}
```
[via](https://stackoverflow.com/questions/64481616/mongodb-mongoose-iterate-over-find-results)

To exclude a specific field from the results, pass it in as the second parameter to find:

    db.datasets.find( { 'name': '210212_M70445_0071_000000000-JHB88' }, { checksums: 0 } )

[via](https://stackoverflow.com/questions/14559200/how-to-exclude-one-particular-field-from-a-collection-in-mongoose)


## Inserts / Updates

If using Mongoose, just call e.g. `Projects.save(project)`. It handles new and updates at the same time?

For a new record in MongoDB directly
 
    db.users.insertOne( {} )
    
https://docs.mongodb.com/guides/server/insert/

To update an existing record

    db.collection.updateOne() 

First parameter is same as find() (query parameter). Second one is a dictionary of update operation expressions:

    db.users.updateOne( { 'email': 'hello@example.com'}, { $set: {'uid': 'hello'} } )
    
updateMany works the same way:

    db.users.updateMany( { 'email': 'hello@example.com'}, { $set: {'uid': 'hello'} } )

https://docs.mongodb.com/manual/reference/method/db.collection.updateMany/

All update operators are documented here:

https://docs.mongodb.com/manual/reference/operator/update/#id1
            
## Sorting / Order

    db.collection.find().sort( { age: -1 } )

$orderby is deprecated
https://docs.mongodb.com/manual/reference/operator/meta/orderby/


## Rename 

To rename a collection:

    db.collection.renameCollection(target, dropTarget)

dropTarget 	boolean 	Optional. If true, mongod drops the target of renameCollection prior to renaming the collection. The default value is false.
    
https://docs.mongodb.com/manual/reference/method/db.collection.renameCollection/

To rename a field

    db.students.updateMany( {}, { $rename: { "nmae": "name" } } )

## Removing

Remove one item / document:

    db.collection.remove() 
    
https://docs.mongodb.com/manual/reference/method/db.collection.remove/





## Mongoose 

Elegant mongodb object modeling for node.js

Closely follows the base MongoDB API for queries. 

Mongoose ODM 
http://mongoosejs.com/


https://duckduckgo.com/?q=is+mongodb+the+whole+backend%3F&t=canonical&ia=web
is mongodb the whole backend? at DuckDuckGo
https://medium.com/javascript-in-plain-english/full-stack-mongodb-react-node-js-express-js-in-one-simple-app-6cc8ed6de274
Let’s build a full stack MongoDB, React, Node and Express (MERN) app
https://duckduckgo.com/?q=mongodb+as+api+server&t=canonical&ia=web
mongodb as api server at DuckDuckGo
https://docs.atlas.mongodb.com/api/
API — MongoDB Atlas
https://nordicapis.com/building-a-restful-api-using-node-js-and-mongodb/
Building a RESTful API Using Node.JS and MongoDB | Nordic APIs |
https://duckduckgo.com/?q=mongoose+vs+mongodb+native&t=canonical&ia=web
mongoose vs mongodb native at DuckDuckGo
http://voidcanvas.com/mongoose-vs-mongodb-native/
Mongoose vs mongodb native driver – what to prefer? | Void Canvas
https://stackoverflow.com/questions/28712248/difference-between-mongodb-and-mongoose
node.js - Difference between MongoDB and Mongoose - Stack Overflow
https://mongoosejs.com/
Mongoose ODM v5.9.4
https://mongoosejs.com/docs/guide.html
Mongoose v5.9.3: Schemas

### Populate

Mongoose can help populate arrays with references to other documents. 

    .populate("users")

It is even possible to run populate on nested objects to get what you need all in one go. 

```
	.populate({
			path: "groups",
			populate: {
				path: "members",
				select: { fullname: 1 },
			},
		})
		.populate("users")
```

To go the other direction, there is Reverse Populate

"mongoose-reverse-populate-v2": "^1.2.4",


### Specify collection

Mongoose by default produces a collection name by passing the model name to the utils.toCollectionName method. This method pluralizes the name. Set this option if you need a different name for your collection.

[via](https://mongoosejs.com/docs/guide.html#collection)

https://stackoverflow.com/questions/5794834/how-to-access-a-preexisting-collection-with-mongoose


## Import / Export Data

### Export / Backups / Dumps

cd /path/to/mongodump

```
mongo
show databases;
```

    mongodump --host localhost --db DBNAME

`mongodump` creates a folder called `dump`

    ls -lash dump/

### Import / Restore

Transfer and/or mount that directory into the destination container / machine.

If using containers: 
  - Ensure that the source of the dump file has been mounted in your db container via `docker-compose.yml`.
  - Connect to the container `docker-compose exec db bash`

On db host:

    # will depend on how your local db files are structured... 
    cd /srv/mongodump/mongodump/
    
    # the db destination will depend on how your API is written... 
    mongorestore --host localhost --db boilerplate

Change to the directory

    mongorestore 
    
or 

    cd /path/to/mongodump/
    mongorestore --host localhost
    
if there is existing data in the database, `restore` will add/append the data. If you only want the new data, it is necessary to use `--drop` first. 

    mongorestore --drop
    
[via](https://stackoverflow.com/questions/27666908/mongorestore-command-replace-existing-records)

### Exporting JSON

https://docs.mongodb.com/manual/reference/program/mongoexport/

If you want to extract the data stored in a collection to a different format, `mongoexport` may be a better tool for the job. 

    mongoexport --collection=<coll> [options]
    
example:

    mongoexport --collection=events --db=reporting --out=events.json

https://docs.mongodb.com/manual/reference/program/mongoexport/


From there you'll have a number of different json files... one for each type of document. I find it better to split out each object to a separate .json file. This is a good chance to give it a unique file name. 

TODO: abstract

mongo/convert_projects.js

If you have binary data stored in the database, if you want to extract it you'll need to convert the text blobs from base64 encoding to binary. Here's a script that does this:

mongo/convert_files.js


## Migrations

how to do migrations in Mongo?

collect the commands as you go in development

Might be possible to write a script too. 


## Scripts

Sometimes it is helpful to be able to interact with a database using on off scripts


```
var db = require("./db");
const mongoose = require("mongoose");
const Datasets = mongoose.model("dataset");

console.log("Starting check datasets");

console.log("Initializing database connection");

db.init(function (err) {
	if (err) console.log(err);
});

console.log("Finding datasets");

const query = {};
Datasets.find(query, (err, results) => {
	if (!err) {
		console.log("Matching datasets:", results);
		// res.status(200).send(results);
	} else {
		console.log(err);
		res.send({ results: results, error: err });
	}
});
```


where `db.js` is something like


```
//Require Mongoose
var mongoose = require('mongoose');

mongoose.set('useCreateIndex', true);

const dataset = require('./models/dataset');
var config = require('./config');

exports.init = function(cb) {
	console.log('initializing model');
	mongoose.connect(config.mongoose.url, {
		replicaSet: config.mongoose.replicaSet,
		useCreateIndex: true,
		useNewUrlParser: true,
		useUnifiedTopology: true,
		useFindAndModify: false
	}, function(err) {
		if(err) return cb(err);
		console.log("connected to mongo");

		exports.dataset = dataset;
		cb();
	});
}
```


and `config/index.js` has something like


```
exports.mongoose = {
	replicaSet: '',
	url: "mongodb://project-docker_db_1:27017/database",
	// url: "mongodb://127.0.0.1:27017/database",
};
```

## Docker

https://hub.docker.com/_/mongo


## Export Data

Glossary — MongoDB Manual 2.6.7
http://docs.mongodb.org/manual/reference/glossary/
mongoimport — MongoDB Manual 2.6.7
http://docs.mongodb.org/manual/reference/program/mongoimport/
mongoexport — MongoDB Manual 2.6.7
http://docs.mongodb.org/manual/reference/program/mongoexport/
mongo-sync-files
https://www.npmjs.com/package/mongo-sync-files
IonicaBizau/node-mongo-sync-files · GitHub
https://github.com/IonicaBizau/node-mongo-sync-files

https://github.com/IonicaBizau/node-mongof
GitHub - IonicaBizau/node-mongof: Sync MongoDB collections with JSON files.


## Importing Data

https://duckduckgo.com/?t=canonical&q=mongo+how+to+handle+foreign+keys&ia=web
mongo how to handle foreign keys at DuckDuckGo
https://stackoverflow.com/questions/6334048/foreign-keys-in-mongo
sql - Foreign keys in mongo? - Stack Overflow
https://duckduckgo.com/?t=canonical&q=mongoose+relationship&ia=web
mongoose relationship at DuckDuckGo
https://vegibit.com/mongoose-relationships-tutorial/
Mongoose Relationships Tutorial – Vegibit
https://duckduckgo.com/?q=mongoose+import+data&t=canonical&ia=web
mongoose import data at DuckDuckGo
https://stackoverflow.com/questions/30696946/how-to-import-json-into-mongodb-using-mongoose
node.js - How to import json into MongoDB using Mongoose - Stack Overflow
https://duckduckgo.com/?t=canonical&q=feathers+js+use+models+in+script&ia=web
feathers js use models in script at DuckDuckGo
https://duckduckgo.com/?t=canonical&q=mongo+script+to+import+collection+of+json+files&ia=web
mongo script to import collection of json files at DuckDuckGo
https://intercom.help/mongodb-compass/en/articles/1837498-import-documents-into-collection-from-a-json-or-csv-file
Import documents into collection from a JSON or CSV file. | MongoDB Compass Frequently Asked Questions


## Hosting

https://www.mongodb.com/cloud/atlas

https://www.google.com/search?q=mongo+atlas
mongo atlas - Google Search


## GUI

Download:

https://www.mongodb.com/download-center/compass

    cd ~/Downloads/
    # unmet dependencies (as of 20.04)
    sudo apt-get install gconf2-common libgconf-2-4
    sudo dpkg -i mongodb-compass_1.21.1_amd64.deb

https://www.mongodb.com/products/compass
Compass | MongoDB

https://www.google.com/search?q=mongo+compass
mongo compass - Google Search

If you expose the mongo db port from the container to the host:

``` docker-compose.yml
  db:
    image: mongo:4.0-xenial
    restart: always
    ports:
      # helpful for using a GUI client like compass for troubleshooting
      - 127.0.0.1:27017:27017
```

then in compass you can use the connection string:

    mongodb://127.0.0.1:27017/

More details about the connection string format:

https://docs.mongodb.com/manual/reference/connection-string/

https://docs.mongodb.com/compass/master/connect/

