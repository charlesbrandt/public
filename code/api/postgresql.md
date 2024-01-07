# PostgreSQL

https://www.postgresql.org/  
PostgreSQL: The world's most advanced open source database  


Heads up: the commands in Postgres clients are different from Mysql clients   
Postgres does not accept double quoted values in SQL statements -- be sure to use single quotes.


http://www.coderholic.com/postgresql-for-mysql-users/  
PostgreSQL for MySQL users  

https://github.com/topics/postgresql  
postgresql · GitHub Topics  

https://github.com/dhamaniasad/awesome-postgres#gui  
dhamaniasad/awesome-postgres: A curated list of awesome PostgreSQL software, libraries, tools and resources, inspired by awesome-mysql  


## CLI Client

### pgcli

https://github.com/dbcli/pgcli  
dbcli/pgcli: Postgres CLI with autocompletion and syntax highlighting  


### psql

Standard postgres client

```
sudo apt install postgresql-client
```

https://ubuntu.com/server/docs/databases-postgresql  
Install and configure PostgreSQL | Ubuntu  

Should be available someplace like:

```
/usr/pgsql-13/bin/psql 
```

The default user is `postgres`

```
psql -U postgres -h 127.0.0.1 -W
```

To connect as a different user

```
psql -U username -d database -h 127.0.0.1 -W
```

Default user is `postgres`


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

Usually `\dt` should work:

```
\dt
```

Occasionally, I've needed to use the following select statement

```
SELECT *
FROM pg_catalog.pg_tables
WHERE schemaname != 'pg_catalog' AND 
    schemaname != 'information_schema';
```

via  
https://www.geeksforgeeks.org/postgresql-show-tables/

## Datatypes

Formats to use when defining a schema


### text


> Generally, there is no downside to using `text` in terms of performance/memory. On the contrary: `text` is the optimum. Other types have more or less relevant downsides. `text` is literally the "preferred" type among string types in the Postgres type system, which can affect function or operator type resolution.


## Backups 

If the system houses real data, set this up as soon as possible. 

It is possible to use a CLI / GUI tool for backups, but those are not commonly automated or scheduled. 

> pgAdmin uses `pg_dump` behind the scenes anyway

If it's data worth keeping, set up a backup routine. 

https://github.com/dhamaniasad/awesome-postgres#backups  
dhamaniasad/awesome-postgres: A curated list of awesome PostgreSQL software, libraries, tools and resources, inspired by awesome-mysql  


Export / Backup

Import / Restore

or 

```
psql -f globals.sql 
```

If you need to keep your users and roles, use `pg_dumpall`

```
pg_dumpall --globals-only to get users/roles/etc
pg_dump -Fc for each database
```

https://stackoverflow.com/questions/16618627/pg-dump-vs-pg-dumpall-which-one-to-use-to-database-backups

Then you would add this to your crontab to run nightly:

```
0 0 * * * /path/to/above/script.sh
```

#### Troubleshooting

```
pg_dumpall: error: server version: 15.1 (Debian 15.1-1.pgdg110+1); pg_dumpall version: 14.8 (Ubuntu 14.8-0ubuntu0.22.04.1)
```

```
sudo apt-get remove postgresql-client-14
sudo apt-get install postgresql-client-15
```

### Retention

A more complete example to manage a rotating batch of backups:

``` sh
#!/bin/bash

# Load environment variables
source /path/to/.env

# Export the password for pg_dumpall and pg_dump to use
export PGPASSWORD="${POSTGRES_PASSWORD}"

# Configuration
BACKUP_DIR="/path/to/backup/root" # Replace with your backup root path
POSTGRES_USER="postgres" # Replace with the PostgreSQL user

# Get date components
YEAR=$(date +'%Y')
MONTH=$(date +'%m')
DAY=$(date +'%d')
DAY_OF_WEEK=$(date +'%u')

# Function to create backup directories if they don't exist
create_directory() {
    if [ ! -d "$1" ]; then
        mkdir -p "$1"
    fi
}

# 1. Nightly Backup
DAILY_DIR="${BACKUP_DIR}/${YEAR}/${MONTH}/${DAY}"
create_directory "${DAILY_DIR}"

# Dump global metadata
pg_dumpall --globals-only -U "${POSTGRES_USER}" > "${DAILY_DIR}/globals.sql"

# Dump each database
for DB in $(psql -U "${POSTGRES_USER}" -t -c "SELECT datname FROM pg_database WHERE NOT datistemplate AND datname != 'postgres';"); do
    pg_dump -Fc -U "${POSTGRES_USER}" "${DB}" > "${DAILY_DIR}/${DB}.sql"
done

# 2. Backup Rotation

# Week's first day: keep it as a weekly snapshot
if [ "$DAY_OF_WEEK" -eq "1" ]; then
    WEEKLY_DIR="${BACKUP_DIR}/${YEAR}/${MONTH}/week_${DAY}"
    create_directory "${WEEKLY_DIR}"
    mv "${DAILY_DIR}"/* "${WEEKLY_DIR}/"
    rmdir "${DAILY_DIR}"  # Removing the now empty daily directory
fi

# Remove daily backups older than 7 days
find "${BACKUP_DIR}/${YEAR}/${MONTH}" -maxdepth 1 -type d -name '[0-9]*' -mtime +7 -exec rm -rf {} \;

# Month's first day: consider previous month for weekly cleanup
if [ "$DAY" -eq "01" ]; then
    LAST_MONTH=$(date --date="1 month ago" +'%m')
    # Keep the first weekly backup of the previous month, remove others
    FIRST_WEEK_DIR=$(find "${BACKUP_DIR}/${YEAR}/${LAST_MONTH}/week_*" -maxdepth 1 -type d | sort | head -n 1)
    for DIR in $(find "${BACKUP_DIR}/${YEAR}/${LAST_MONTH}/week_*" -maxdepth 1 -type d); do
        if [ "${DIR}" != "${FIRST_WEEK_DIR}" ]; then
            rm -rf "${DIR}"
        fi
    done
fi

# Make this script executable and add it to cron

```

run it out and make sure it works





### wal-g

For more realtime and finegrained backups. Useful when nightly is no longer sufficient. 

These may take up more space, are only restoreable to the same PostgreSQL version on the same platform, and back up all tables in all databases with no ability to exclude anything.

https://github.com/wal-g/wal-g  
wal-g/wal-g: Archival and Restoration for databases in the Cloud  
https://www.citusdata.com/blog/2017/08/18/introducing-wal-g-faster-restores-for-postgres/  
Introducing WAL-G by Citus: Faster Disaster Recovery for Postgres  

https://github.com/omniti-labs/omnipitr  
omniti-labs/omnipitr: Advanced WAL File Management Tools for PostgreSQL  
### Others

https://github.com/pgbackrest/pgbackrest  
pgbackrest/pgbackrest: Reliable PostgreSQL Backup & Restore  
https://pgbackrest.org/  
pgBackRest - Reliable PostgreSQL Backup & Restore  

https://github.com/aiven/pghoard  
aiven/pghoard: PostgreSQL® backup and restore service  
https://pgbarman.org/index.html  
Barman  
https://github.com/EnterpriseDB/barman  
EnterpriseDB/barman: Barman - Backup and Recovery Manager for PostgreSQL  
https://github.com/postgrespro/pg_probackup  
postgrespro/pg_probackup: Backup and recovery manager for PostgreSQL  
https://github.com/orgrim/pg_back/  
orgrim/pg_back: Simple backup tool for PostgreSQL  
https://dalibo.github.io/pitrery/  


https://duckduckgo.com/?t=ffab&q=postgresql+best+column+type+for+strings&atb=v343-1&ia=web  
postgresql best column type for strings at DuckDuckGo  
https://stackoverflow.com/questions/20326892/any-downsides-of-using-data-type-text-for-storing-strings  
sql - Any downsides of using data type "text" for storing strings? - Stack Overflow  

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



## Backups 

It is possible to use a CLI / GUI tool for backups, but those are not commonly automated or scheduled. 

If it's data worth keeping, set up a backup routine. 

### Import / Export 

https://www.a2hosting.com/kb/developer-corner/postgresql/import-and-export-a-postgresql-database  
How to import and export PostgreSQL databases  

```
pg_dump -U username dbname > dbexport.pgsql
```

```
psql -U username dbname < dbexport.pgsql
```

### wal-g

https://github.com/wal-g/wal-g  
wal-g/wal-g: Archival and Restoration for databases in the Cloud  
https://www.citusdata.com/blog/2017/08/18/introducing-wal-g-faster-restores-for-postgres/  
Introducing WAL-G by Citus: Faster Disaster Recovery for Postgres  

https://duckduckgo.com/?t=ffab&q=using+wal-g+guide&atb=v343-1&ia=web  
using wal-g guide at DuckDuckGo  
https://www.fusionbox.com/blog/detail/postgresql-wal-archiving-with-wal-g-and-s3-complete-walkthrough/644/  
Postgres WAL Archiving: A Complete Walkthrough  
https://duckduckgo.com/?t=ffab&q=+how+to+use+WAL-G+to+back+up+a+local+postgresql+database&atb=v343-1&ia=web  
how to use WAL-G to back up a local postgresql database at DuckDuckGo  
https://stackoverflow.com/questions/56117363/postgres-backup-with-wal  
postgresql - postgres backup with WAL - Stack Overflow  
https://wal-g.readthedocs.io/PostgreSQL/  
PostgreSQL - WAL-G  
https://supabase.com/blog/continuous-postgresql-backup-walg  
Continuous PostgreSQL Backups using WAL-G  
https://github.com/wal-g/wal-g  
wal-g/wal-g: Archival and Restoration for databases in the Cloud  
https://www.citusdata.com/blog/2017/08/18/introducing-wal-g-faster-restores-for-postgres/  
Introducing WAL-G by Citus: Faster Disaster Recovery for Postgres  


### 

https://github.com/pgbackrest/pgbackrest  
pgbackrest/pgbackrest: Reliable PostgreSQL Backup & Restore  
https://pgbackrest.org/  
pgBackRest - Reliable PostgreSQL Backup & Restore  


https://github.com/aiven/pghoard  
aiven/pghoard: PostgreSQL® backup and restore service  
https://pgbarman.org/index.html  
Barman  
https://github.com/EnterpriseDB/barman  
EnterpriseDB/barman: Barman - Backup and Recovery Manager for PostgreSQL  
https://github.com/omniti-labs/omnipitr  
omniti-labs/omnipitr: Advanced WAL File Management Tools for PostgreSQL  
https://github.com/postgrespro/pg_probackup  
postgrespro/pg_probackup: Backup and recovery manager for PostgreSQL  
https://github.com/orgrim/pg_back/  
orgrim/pg_back: Simple backup tool for PostgreSQL  
https://dalibo.github.io/pitrery/  


## Migrations

https://github.com/michaelsogos/pg-diff-api  
michaelsogos/pg-diff-api: PostgreSQL migration strategy for NodeJS  

https://github.com/dimitri/pgloader  
dimitri/pgloader: Migrate to PostgreSQL in a single command!  
https://pgloader.io/  
pgloader  

via:
https://www.postgresql.org/download/products/1-administrationdevelopment-tools/  
PostgreSQL: Software Catalogue - Administration/development tools  



## GUI

Postgres specific. For general tools, see [DB Guis](gui-db.md)

https://pgmodeler.io/  
pgModeler - PostgreSQL Database Modeler  
https://github.com/pgmodeler/pgmodeler  
pgmodeler/pgmodeler: Open-source data modeling tool designed for PostgreSQL. No more typing DDL commands. Let pgModeler do the work for you!  

### pgAdmin 

pgAdmin is a GUI specific for Postgresql databases.

Note: pgAdmin does not include an auto-generated ERD based on current database schema. 

https://www.pgadmin.org/  
pgAdmin - PostgreSQL Tools  
https://www.pgadmin.org/download/  
Download  
https://www.pgadmin.org/download/pgadmin-4-apt/  
Download via apt  

```
# Install the public key for the repository (if not done previously):
curl -fsS https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo gpg --dearmor -o /usr/share/keyrings/packages-pgadmin-org.gpg

# Create the repository configuration file:
sudo sh -c 'echo "deb [signed-by=/usr/share/keyrings/packages-pgadmin-org.gpg] https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list && apt update'

# Install for desktop mode only:
sudo apt install pgadmin4-desktop

```

https://www.enterprisedb.com/blog/google-cloud-sql-postgresql-deployment-pgadmin-4  
Google Cloud SQL - PostgreSQL Deployment with pgAdmin 4 | EDB  

## Utilities

https://github.com/dhamaniasad/awesome-postgres#backups  
dhamaniasad/awesome-postgres: A curated list of awesome PostgreSQL software, libraries, tools and resources, inspired by awesome-mysql  


## Metabases

Ready to go APIs built on top of a database. Some rely on Postgres exclusively, others can use many types of different databases. 

https://github.com/supabase/supabase  
supabase/supabase: The open source Firebase alternative. Follow to stay updated about our public Beta.  
https://github.com/metabase/metabase  
metabase/metabase: The simplest, fastest way to get business intelligence and analytics to everyone in your company  



## Replication and Architecture

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


