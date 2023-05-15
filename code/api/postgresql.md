# Postgres

https://www.postgresql.org/  
PostgreSQL: The world's most advanced open source database  

Postgres does not accept double quoted values in SQL statements -- be sure to use single quotes.

The commands in Postgres clients are different from Mysql clients. 

http://www.coderholic.com/postgresql-for-mysql-users/  
PostgreSQL for MySQL users  


## CLI Client



```
/usr/pgsql-13/bin/psql 
```

To connect as a different user

```
psql -U username -d database -h 127.0.0.1 -W
```


### Show databases

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

### Show tables

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



### Import / Export 

https://www.a2hosting.com/kb/developer-corner/postgresql/import-and-export-a-postgresql-database  
How to import and export PostgreSQL databases  

```
pg_dump -U username dbname > dbexport.pgsql
```

```
psql -U username dbname < dbexport.pgsql
```


## Manage Users and Permissions

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

For full access

```
grant all on database mydb to xxx;
```

In some cases, if the schema is not listed as part of `\dt`, it may be necessary to grant permissions on the schema directly:

```
grant all on all tables in schema schemaname to username;
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


## Metabases

Ready to go APIs built on top of a database. Some rely on Postgres exclusively, others can use many types of different databases. 

https://github.com/topics/postgresql  
postgresql · GitHub Topics  
https://github.com/supabase/supabase  
supabase/supabase: The open source Firebase alternative. Follow to stay updated about our public Beta.  
https://github.com/metabase/metabase  
metabase/metabase: The simplest, fastest way to get business intelligence and analytics to everyone in your company  
https://github.com/cube-js/cube.js  
cube-js/cube.js: 📊 Cube — Headless Business Intelligence for Building Data Applications  
https://github.com/calcom/cal.com  
calcom/cal.com: Scheduling infrastructure for absolutely everyone.  


## Replication

https://duckduckgo.com/?t=ffab&q=is+it+possible+to+synchronize+two+postgres+databases%3F+&ia=web
is it possible to synchronize two postgres databases? at DuckDuckGo
https://dba.stackexchange.com/questions/214055/postgresql-database-synchronization
PostgreSQL database synchronization - Database Administrators Stack Exchange
https://stackoverflow.com/questions/1292107/synchronize-two-pg-databases
postgresql - synchronize two pg databases - Stack Overflow
https://stackoverflow.com/questions/73544294/synchronize-data-between-two-postgres-database
postgresql - Synchronize data between two postgres database - Stack Overflow
https://duckduckgo.com/?t=ffab&q=postgres+replication&ia=web
postgres replication at DuckDuckGo
https://www.postgresql.org/docs/current/different-replication-solutions.html
PostgreSQL: Documentation: 15: 27.1. Comparison of Different Solutions
https://duckduckgo.com/?t=ffab&q=postgres+Bucardo&ia=web
postgres Bucardo at DuckDuckGo
https://bucardo.org/Bucardo/
Bucardo
https://bucardo.org/Bucardo/Overview
Bucardo Overview

Quickly moves into clustering strategies. 

### Kubernetes

https://github.com/sorintlab/stolon/  
sorintlab/stolon: PostgreSQL cloud native High Availability and more.  
https://github.com/sorintlab/stolon/blob/master/examples/kubernetes/README.md  
stolon/README.md at master · sorintlab/stolon  

https://artifacthub.io/packages/search?ts_query_web=postgres&sort=stars&page=1  
Artifact Hub  
https://artifacthub.io/packages/helm/stolon/stolon  
stolon 1.7.2 · iomed/stolon  

https://duckduckgo.com/?t=ffab&q=postgres+kubernetes&ia=web  
postgres kubernetes at DuckDuckGo  
https://adamtheautomator.com/postgres-to-kubernetes/  
How to Deploy Postgres to Kubernetes  
https://artifacthub.io/packages/search?ts_query_web=postgres&sort=stars&page=1  
Artifact Hub  
https://artifacthub.io/packages/helm/bitnami/postgresql-ha  
postgresql-ha 9.4.1 · bitnami/bitnami  
https://artifacthub.io/packages/helm/bitnami/postgresql-ha  
postgresql-ha 9.4.6 · bitnami/bitnami  


