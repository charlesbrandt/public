# MongoDB

Document Database

https://www.mongodb.com/

https://www.tutorialspoint.com/mongodb/mongodb_overview.htm  
MongoDB - Overview - Tutorialspoint  


## Connections

https://docs.mongodb.com/guides/server/drivers/

 
## Shell

If your container doesn't have it already, you can install the CLI with

```
sudo apt-get install -y mongodb-clients
```

https://docs.mongodb.com/manual/mongo/

https://docs.mongodb.com/manual/reference/mongo-shell/

```
mongo mongodb://$[hostlist]/$[database]?authSource=$[authSource] --username $[username]
```

or, for a local connection:

```
mongo
```

See the current database:

```
db
```

See all databases:

```
show dbs
```

`use database_name`

To see all collections associated with a database:

```
db.getCollectionNames()
```

https://stackoverflow.com/questions/8866041/how-can-i-list-all-collections-in-the-mongodb-shell


## Find
        
```
db.collection.find()
```

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

```
db.datasets.find( { 'name': '210212_M70445_0071_000000000-JHB88' }, { checksums: 0 } )
```

[via](https://stackoverflow.com/questions/14559200/how-to-exclude-one-particular-field-from-a-collection-in-mongoose)

### Sorting / Order

```
db.collection.find().sort( { age: -1 } )
```

$orderby is deprecated
https://docs.mongodb.com/manual/reference/operator/meta/orderby/


## Insert / Update

If using Mongoose, just call e.g. `Projects.save(project)`. It handles new and updates at the same time.

Alternatively, `create` is also available:

```
router.post("/", function (req, res, next) {
  Datapath.create(req.body)  
    .then((created) => {
      res.json(created);
    })
    .catch((err) => {
      next(err);
    });
});
```


For a new record in MongoDB directly
 
```
db.users.insertOne( {} )
```

https://docs.mongodb.com/guides/server/insert/

To update an existing record

```
db.collection.updateOne() 
```

First parameter is same as find() (query parameter). Second one is a dictionary of update operation expressions:

```
db.users.updateOne( { 'email': 'hello@example.com'}, { $set: {'uid': 'hello'} } )
```

updateMany works the same way:

```
db.users.updateMany( { 'email': 'hello@example.com'}, { $set: {'uid': 'hello'} } )
```

https://docs.mongodb.com/manual/reference/method/db.collection.updateMany/

All update operators are documented here:

https://docs.mongodb.com/manual/reference/operator/update/#id1

## Remove / Delete

Remove one item / document:

```
db.collection.remove() 
```

https://docs.mongodb.com/manual/reference/method/db.collection.remove/




## Queries

Queries are the first step in most operations that specify the content to work with. 

https://docs.mongodb.com/manual/tutorial/query-documents/

Not sure what fields are in use in a given collection? 

```
Object.keys(db.messages.findOne())
```

[via](https://stackoverflow.com/questions/5900792/how-to-view-document-fields-in-mongo-shell)

### Object ID

When you need to get a related document via Compass or CLI:

```
{ "_id" : ObjectId("4ecc05e55dd98a436ddcc47c") } 
```

### Logical Operators

pass checks in as part of a list

```
$and: [{ dataset: req.params.dataset }, { visible: false }],
```

https://docs.mongodb.com/manual/reference/operator/query-logical/

`not` works a little different

```
{ status: {$not: {$eq: "completed"}}  }  
```

#### Existence

Sometime new fields may not exist in old records. Can check for a value or existence with:

```
$or: [{ visible: true }, { visible: { $exists: false } }],
```

### Date range

```
return Episode.
      find({ airedAt: { $gte: '1987-10-19', $lte: '1987-10-26' } }).
      sort({ airedAt: 1 });
  })
```

https://mongoosejs.com/docs/tutorials/dates.html

### Null Fields

Match records that do not have a matching key:

```
db.collection.find( { key_name : { $exists: false } } )
```

https://docs.mongodb.com/manual/tutorial/query-for-null-fields/

### Nested Fields

Place the full path to the field in quotes to use it in a query

```
"paths.staged": "",
```

https://stackoverflow.com/questions/19889313/mongodb-query-in-on-a-hash-field

### Regular Expressions

https://docs.mongodb.com/manual/reference/operator/query/regex/#mongodb-query-op.-regex

Not working:
{createdAt: { $regex: /2021-06-30*/ } }

Seems like searching by a date range is a cumbersome process? 

https://stackoverflow.com/questions/43996930/regex-in-mongodb-for-iso-date-field


## Rename

To rename a collection:

```
db.collection.renameCollection(target, dropTarget)
```

dropTarget 	boolean 	Optional. If true, mongod drops the target of renameCollection prior to renaming the collection. The default value is false.
    
https://docs.mongodb.com/manual/reference/method/db.collection.renameCollection/

To rename a field

```
db.students.updateMany( {}, { $rename: { "nmae": "name" } } )
```





## Mongoose 

Elegant mongodb object modeling for node.js

Closely follows the base MongoDB API for queries. 

Mongoose ODM   
http://mongoosejs.com/  

https://mongoosejs.com/docs/guide.html  
Mongoose v5.9.3: Schemas  


### Install

If running in a container, add the settings in `docker-compose.yml`:

```
  mongo:
    # https://hub.docker.com/_/mongo
    image: mongo:5
    container_name: boilerplate_mongo
    restart: unless-stopped
    # ports:
    # helpful for using a GUI client like compass for troubleshooting
    # - 127.0.0.1:27017:27017
    environment:
      MONGO_INITDB_ROOT_USERNAME: root
      MONGO_INITDB_ROOT_PASSWORD: example
    volumes:
      - ./db_mongo:/data/db
      # for importing database files
      # - ./mongodump:/srv/mongodump
```

Connect to the container:


```
yarn add mongoose

or

npm install mongoose --save
```

Configure in the application, e.g. `config/index.js`:

```
exports.mongoose = {
  // https://stackoverflow.com/questions/60394290/mongo-db-docker-image-authentication-failed
  // url: "mongodb://root:example@boilerplate_mongo:27017/example",
  url: "mongodb://boilerplate_mongo:27017/boilerplate",
  // replicaSet: "",
};
```



### Populate

Mongoose can automatically populate references to other documents. This is a great feature to use mongoose for. 

```
.populate("users")
```

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

### findOneAndUpdate

Major *gotcha* here. The return value is the original value before update has been applied. Not what I expected!

https://mongoosejs.com/docs/tutorials/findoneandupdate.html

Reminder: the value that is passed on is the value that was 'found' before the update is applied. If you make updates to that original value and re-save, you may lose your updated values. 

### Specify collection

Mongoose by default produces a collection name by passing the model name to the utils.toCollectionName method. This method pluralizes the name. Set this option if you need a different name for your collection.

[via](https://mongoosejs.com/docs/guide.html#collection)

https://stackoverflow.com/questions/5794834/how-to-access-a-preexisting-collection-with-mongoose

### Saving Data

```
// if results is what is returned by mongoose findById (or equivalent)
body = { 'something': 'plain old object here' }
// this overrides the mongo attributes -- save() not available
results = body;
results.save(); <--- save not available!
// rather than try to update all changed parameters, just subsequently call findByIdAndUpdate later
```

### Pagination

https://github.com/edwardhotchkiss/mongoose-paginate

Looks like there is a maintained version available here (haven't tried yet):
https://www.npmjs.com/package/mongoose-paginate-v2



In the api, install mongoose-paginate

```
yarn add mongoose-paginate
```

The mongoose models will need to use the 'mongoose-paginate' plugin

```
var mongoosePaginate = require('mongoose-paginate');

const sampleSchema = new Schema(
	{ // field definitions
    },
	{ timestamps: true }
);
    
...

sampleSchema.plugin(mongoosePaginate);

module.exports = mongoose.model("sample", sampleSchema);
```

Then on the API side

TODO: compare this to feathers API
possible to standardize? 

```
const Sample = mongoose.model("sample");

router.get("/all/:page/:limit/:filter?", function (req, res, next) {
	let query = {};
	if (req.params.filter) {
		query = {
			name: { $regex: req.params.filter.toLowerCase(), $options: "i" },
		};
	}
	let page = !isNaN(req.params.page) ? parseInt(req.params.page) : 1;
	let limit = !isNaN(req.params.limit) ? parseInt(req.params.limit) : 20;
	let options = {
		page: page,
		limit: limit,
		populate: [{ path: "subSample", select: "-bigObjectToSkip" }, "sampleFriend", "user"],
	};
	console.log("Getting samples", options, query);
	Sample.paginate(query, options, function (err, result) {
		if (err) return next(err);

		// console.log("Result has the following keys", Object.keys(result));
        
		const options = {
			modelArray: result.docs,
			storeWhere: "subSample",
			arrayPop: true,
			mongooseModel: SubSample,
			idField: "sample",
		};

		reversePopulate(options, function (err, popCons) {
			popCons.forEach((pc) => {
				pc._doc["subSample"] = pc.subSample;
			});

			if (!result) return res.status(200).json([]);
			res.status(200).send(result);
		});
	});
});
```

Result has the following keys [ 'docs', 'total', 'limit', 'page', 'pages' ]






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

## Migrations

One idea with schemaless databases is that you don't need to do migrations. 

There may be a need to change values in an existing dataset. In that case, write a script.



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

In a `docker-compose.yml` file, you can use something like:

```
  mongo:
    # https://hub.docker.com/_/mongo
    image: mongo:5
    container_name: boilerplate_db
    restart: unless-stopped
    # ports:
    # helpful for using a GUI client like compass for troubleshooting
    # - 127.0.0.1:27017:27017
    #environment:
    # MONGO_INITDB_ROOT_USERNAME: root
    # MONGO_INITDB_ROOT_PASSWORD: example
    volumes:
      - ./db:/data/db
      # for importing database files
      # - ./mongodump:/srv/mongodump
```


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


## GUI

### Compass 

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

If you expose the mongo db port from the container to the host (in `docker-compose.yml1`):

``` yaml
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

### DBeaver

May have support for Mongo too!

## Hosting

https://www.mongodb.com/cloud/atlas

https://www.google.com/search?q=mongo+atlas
mongo atlas - Google Search


