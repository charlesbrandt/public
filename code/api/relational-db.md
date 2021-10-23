# Relational Databases

## ORM

[Sequelize](sequelize.md) for an ORM for working with a database from an application. 


## Postgres


## MariaDB

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

I received:

`The user specified as a definer does not exist" when using LOCK TABLES`

The fix was:

```
GRANT ALL ON *.* TO 'root'@'%' IDENTIFIED BY 'password' WITH GRANT OPTION;
```

if the error shows a different user, change it in the command above.

[via](https://stackoverflow.com/questions/10169960/mysql-error-1449-the-user-specified-as-a-definer-does-not-exist)


### Alterations

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
