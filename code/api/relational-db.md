# Relational Databases

## ORM

[Sequelize](sequelize.md) for an ORM for working with a database from an application. 


## Postgres


## MariaDB

need a client where ever access is required. (The database container is currently exposed via 127.0.0.1:3306 for GUI access)

```
sudo apt-get install mariadb-client -y
```

Connect with:
`-p` will prompt for a password

```
mysql -h 127.0.0.1 -u root -p database-name 
```

In a script, you can append the password immediately after `-p` (no space). 
Don't provide password via the CLI (would be available in the CLI history then)

```
mysql -h 127.0.0.1 -u root -pexample database-name 
```

### Describe a table


SQL statement that can be used to create a table:

```
show create table [db_name.]table_name;
```

Formatted output:

```
describe [db_name.]table_name;
```

https://stackoverflow.com/questions/1498777/how-do-i-show-the-schema-of-a-table-in-a-mysql-database

### Create Database

```
CREATE DATABASE [IF NOT EXISTS] database_name
```

### Export and Restore

https://mariadb.com/kb/en/backup-and-restore-overview/

These are run on the system shell, not the `mysql` cli. 

backup:

```
shell> mysqldump db_name > backup-file.sql
```

restore:

```
shell> mysql db_name < backup-file.sql
```

#### Via Node

```
var exec = require("child_process").exec;

function toJSONLocal(date) {
  var local = new Date(date);
  local.setMinutes(date.getMinutes() - date.getTimezoneOffset());
  return local.toJSON().slice(0, 10);
}

let date = new Date();
let name = toJSONLocal(date);
console.log(name);
var child = exec(
  // possible to run a command first if permissions need updating (eg GRANT ALL)
  `mysql -h 127.0.0.1 -u root -pexample database-name < ./grant-permissions.sql; mysqldump -h 127.0.0.1 -u root -pexample database-name > ../dbdump/${name}-database-name.sql`
);

```

#### Permission Errors

I received:

`The user specified as a definer does not exist" when using LOCK TABLES`

The fix was:

```
GRANT ALL ON *.* TO 'root'@'%' IDENTIFIED BY 'password' WITH GRANT OPTION;
```

if the error shows a different user, change it in the command above.

[via](https://stackoverflow.com/questions/10169960/mysql-error-1449-the-user-specified-as-a-definer-does-not-exist)

```
GRANT ALL PRIVILEGES ON database_name.* TO 'username'@'localhost';
```

https://chartio.com/resources/tutorials/how-to-grant-all-privileges-on-a-database-in-mysql/

### Schema Changes

```
SET FOREIGN_KEY_CHECKS = 0;

ALTER TABLE table_name MODIFY id INTEGER NOT NULL AUTO_INCREMENT; 
# if existing rows in the database, tell it where to start
ALTER TABLE table_name AUTO_INCREMENT = 10; 

SET FOREIGN_KEY_CHECKS = 1;
```

https://duckduckgo.com/?q=mysql+add+column&t=ffab&ia=web  
mysql add column at DuckDuckGo  
https://www.mysqltutorial.org/mysql-add-column/  
How to Add Columns To A Table Using MySQL ADD COLUMN  
https://phoenixnap.com/kb/how-to-rename-column-mysql  
How to Rename a Column in MySQL {ALTER TABLE command}  
https://www.mysqltutorial.org/mysql-drop-table  
MySQL DROP TABLE  

#### Foreign keys

https://stackoverflow.com/questions/43493889/cannot-delete-or-update-a-parent-row-a-foreign-key-constraint-fails-mysql

```
ALTER TABLE "appointments" ADD CONSTRAINT "appointments_user_id_foreign" FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE;
```

  - Add on delete cascade meaning that when user is deleted appointments relating to that user should also be deleted as suggested by Shadow (point number two)

  - Add `on delete set null` meaning that when user is deleted appointments user_id relating to that user should be set to null (though you will have to change user_id int(10) UNSIGNED NOT NULL to user_id int(10) UNSIGNED DEFAULT NULL



### Manual Migrations

If you haven't configured an actual migration system (like [Sequelize Migrations](sequelize.md#migrations)), track changes using a simple text file to note the SQL commands that get applied

Use SQL commands to `ALTER` tables


## Schema Diagrams

### Diagrams.net 

supports importing a sql file

https://stackoverflow.com/questions/4108816/entity-relationship-diagram-software
💤 database - Entity relationship diagram software - Stack Overflow

### mysqlworkbench

https://dev.mysql.com/downloads/workbench/

Just downloading directly from the site

```
sudo dpkg -i mysql-workbench-community_8.0.27-1ubuntu20.04_amd64.deb

sudo apt --fix-broken install
```

Using:

File -> Import -> Reverse Engineer MySQL Create Script

Will load the tables

Then use 

Model -> Create Diagram from Catalog Objects

From here it's possible to move the objects around for an optimal layout. 

It's also possible to access the database directly. If the database is available locally, there may not be an SSL proxy layer, which Workbench seems to want. 

https://stackoverflow.com/questions/69747663/how-to-configure-mysql-workbench-to-not-require-ssl-encryption

You can go to the Advanced tab and type the following in the Others field:

    useSSL=0





## Insert data

No quotes are required around the column names, but quotes are required around the string values. (number values don't need quotes)

```
INSERT INTO table1 (column1, column2,...)
VALUES
	(value1, value2,...);
```

## Update data

```
UPDATE [LOW_PRIORITY] [IGNORE] table_name 
SET 
    column_name1 = expr1,
    column_name2 = expr2,
    ...
[WHERE
    condition];
```

