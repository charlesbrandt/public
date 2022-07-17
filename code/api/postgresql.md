# Postgres

Postgres clients are different from mysql clients. 

http://www.coderholic.com/postgresql-for-mysql-users/  
PostgreSQL for MySQL users  


Postgres does not accept double quoted values in SQL statements -- be sure to use single quotes.

```
/usr/pgsql-13/bin/psql 
```

To connect as a different user

```
psql -U username -d database -h 127.0.0.1 -W
```


## Show databases

Listing databases in PostgreSQL using psql command

```
\l
```

or with a select statement

```
SELECT datname FROM pg_database;
```

`show databases;` will not work

via  
https://www.geeksforgeeks.org/postgresql-show-tables/  

## Show tables

Supposedly `\dt` should work:

```
\dt
```

but I needed to use the following select statement

```
SELECT *
FROM pg_catalog.pg_tables
WHERE schemaname != 'pg_catalog' AND 
    schemaname != 'information_schema';
```

via  
https://www.geeksforgeeks.org/postgresql-show-tables/



## Manage Users

A good guide. 

https://www.digitalocean.com/community/tutorials/how-to-use-roles-and-manage-grant-permissions-in-postgresql-on-a-vps-2

Show all users (may require logging in as `postgres` user with `pgsql -U postgres`):

```
\du
```

https://stackoverflow.com/questions/760210/how-do-you-create-a-read-only-user-in-postgresql


```
create user sam;
```

Is `create user` simply an alias for `create role`? Seems like it may be. 


```
GRANT CONNECT ON DATABASE mydb TO xxx;
-- This assumes you're actually connected to mydb..
GRANT USAGE ON SCHEMA public TO xxx;
GRANT SELECT ON mytable TO xxx;
```

https://stackoverflow.com/questions/760210/how-do-you-create-a-read-only-user-in-postgresql

https://duckduckgo.com/?t=ffab&q=postgres+set+password+for+user&ia=web  
postgres set password for user at DuckDuckGo  
https://www.postgresqltutorial.com/postgresql-change-password/  
How To Change the Password of a PostgreSQL User  
https://duckduckgo.com/?q=postgres+password+for+system+user+no+password&t=ffab&ia=web  
postgres password for system user no password at DuckDuckGo  
https://www.liquidweb.com/kb/what-is-the-default-password-for-postgresql/  
What is the Default Password for PostgreSQL? - Liquid Web  
https://www.educba.com/postgresql-user-password/  
PostgreSQL User Password | Create, Change, Delete Password for Users  

## Import / Export 

https://www.a2hosting.com/kb/developer-corner/postgresql/import-and-export-a-postgresql-database  
How to import and export PostgreSQL databases  

```
pg_dump -U username dbname > dbexport.pgsql
```

```
psql -U username dbname < dbexport.pgsql
```



## Size of database

https://duckduckgo.com/?t=ffab&q=postgresql+show+size+of+tables&ia=web  
postgresql show size of tables at DuckDuckGo  
https://dbtut.com/index.php/2018/10/07/postgresql-list-of-table-sizes/  
How To Find the Size of Tables and Indexes in PostgreSQL - Database Tutorials  
https://www.geeksforgeeks.org/postgresql-size-of-a-table/  
PostgreSQL - Size of a Table - GeeksforGeeks  

https://duckduckgo.com/?t=ffab&q=dbeaver+show+size+of+tables&ia=web  
dbeaver show size of tables at DuckDuckGo  
https://github.com/dbeaver/dbeaver/issues/1750  
View total size of database on disk? · Issue #1750 · dbeaver/dbeaver · GitHub  
https://github.com/dbeaver/dbeaver/issues/10078  
display the size of table · Issue #10078 · dbeaver/dbeaver · GitHub  


```
SELECT
    relname AS "tables",
    pg_size_pretty (
        pg_total_relation_size (X .oid)
    ) AS "size"
FROM
    pg_class X
LEFT JOIN pg_namespace Y ON (Y.oid = X .relnamespace)
WHERE
    nspname NOT IN (
        'pg_catalog',
        'information_schema'
    )
AND X .relkind <> 'i'
AND nspname !~ '^pg_toast'
ORDER BY
    pg_total_relation_size (X .oid) ASC
LIMIT 10;
```


```
SELECT pg_table_size('size_test_table') AS data_size,
pg_relation_size('size_test_table_pkey') AS index_size,
pg_table_size('size_test_table') + pg_relation_size('size_test_table_pkey') AS total_size1,
pg_total_relation_size('size_test_table') AS total_size2;
```



```
WITH tbl_spc AS (
SELECT ts.spcname AS spcname
FROM pg_tablespace ts 
JOIN pg_database db ON db.dattablespace = ts.oid
WHERE db.datname = current_database()
)
(
SELECT
t.schemaname,
t.tablename,
pg_table_size('"' || t.schemaname || '"."' || t.tablename || '"') AS table_disc_size,
NULL as index,
0 as index_disc_size,
COALESCE(t.tablespace, ts.spcname) AS tablespace
FROM pg_tables t, tbl_spc ts

UNION ALL

SELECT
i.schemaname,
i.tablename,
0,
i.indexname,
pg_relation_size('"' || i.schemaname || '"."' || i.indexname || '"'),
COALESCE(i.tablespace, ts.spcname)
FROM pg_indexes i, tbl_spc ts
)
ORDER BY table_disc_size DESC,index_disc_size DESC;
```


## IP Based Access Control

Postgres can be configured to allow certain IP addresses.

$PG_ROOT/data/pg_hba.conf
usually /var/lib/pgsql or /var/lib/postgresql 

vi /var/lib/pgsql/13/data/pg_hba.conf
