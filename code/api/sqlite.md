# SQLite

Not every service needs its own container. SQLite does not include any network connections nor user accounts.

https://stackoverflow.com/questions/8357496/access-sqlite-from-a-remote-server

SQLite is easy to add in to an existing container. For an API, this is a good approach. 

## Installation

```
sudo apt-get install sqlite3
```

Load a local database file:

```
sqlite3 db.sqlite3
```


## Databases

Sqlite files contain one database. Multiple databases can be loaded ('attached') into a sqlite session.


## Tables

See available tables:

```
sqlite> .tables

select * from tbl1;
```

via:
https://www.sqlitetutorial.net/sqlite-tutorial/sqlite-show-tables/


To describe a specific table, use:

```
.schema table_name
```

or 

```
.header on
.mode column

pragma table_info('table_name');
```



## Export data to csv

Comma separated

```
sqlite3 /path/to/file.sqlite

.headers on
.mode csv
.output export.csv
SELECT * from data_table;
```

Tab separated

```
sqlite3 /path/to/file.sqlite

.headers on
.mode tabs
.output export.tsv
SELECT * from data_table;
```

## Search / Regular Expression

`%` matches one or more characters. `_` matches one character

```
SELECT p.FirstName, p.LastName, ph.PhoneNumber  
FROM Person.PersonPhone AS ph  
INNER JOIN Person.Person AS p  
ON ph.BusinessEntityID = p.BusinessEntityID  
WHERE ph.PhoneNumber LIKE '415%'  
ORDER by p.LastName;  
GO
```

https://www.sqlshack.com/sql-like-logical-operator-introduction-and-overview/


## GUI

DBeaver can open a sqlite database

File -> New -> Database Connection -> Sqlite -> fill in form for file location
