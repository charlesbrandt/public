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

The user specified as a definer does not exist" when using LOCK TABLES

GRANT ALL ON *.* TO 'root'@'%' IDENTIFIED BY 'password' WITH GRANT OPTION;

### Alterations

https://duckduckgo.com/?q=mysql+add+column&t=ffab&ia=web
mysql add column at DuckDuckGo
https://www.mysqltutorial.org/mysql-add-column/
How to Add Columns To A Table Using MySQL ADD COLUMN
https://phoenixnap.com/kb/how-to-rename-column-mysql
How to Rename a Column in MySQL {ALTER TABLE command}
https://www.mysqltutorial.org/mysql-drop-table
MySQL DROP TABLE

