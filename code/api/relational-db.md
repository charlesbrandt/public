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



### Insert data

No quotes are required around the column names, but quotes are required around the string values. (number values don't need quotes)

```
INSERT INTO table1 (column1, column2,...)
VALUES
	(value1, value2,...);
```

### Update data

```
UPDATE [LOW_PRIORITY] [IGNORE] table_name 
SET 
    column_name1 = expr1,
    column_name2 = expr2,
    ...
[WHERE
    condition];
```

## Manual Migrations

If you haven't configured an actual migration system (like [Sequelize Migrations](sequelize.md#migrations)), track changes using a simple text file to note the SQL commands that get applied

Use SQL commands to `ALTER` tables
