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


## GUI

For general tools, see [DB Guis](gui-db.md).  DBeaver is tough to beat.

These are Postgres specific. 

https://pgmodeler.io/  
pgModeler - PostgreSQL Database Modeler  
https://github.com/pgmodeler/pgmodeler  
pgmodeler/pgmodeler: Open-source data modeling tool designed for PostgreSQL. No more typing DDL commands. Let pgModeler do the work for you!  

### pgAdmin 

pgAdmin is a GUI specific for Postgresql databases.

Note: pgAdmin does not include an auto-generated ERD based on current database schema. 

> pgAdmin uses `pg_dump` behind the scenes for backups

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

Ensure any clients are running the same versions as the server to avoid errors like:

```
pg_dumpall: error: server version: 16.3 (Debian 16.3-1.pgdg120+1); pg_dumpall version: 14.12 (Ubuntu 14.12-0ubuntu0.22.04.1)
pg_dumpall: error: aborting because of server version mismatch
```

To install a specific version on Ubuntu

[Install and Configure PostgreSQL 16 on Ubuntu 22.04|20.04|18.04 | ComputingForGeeks](https://computingforgeeks.com/install-and-configure-postgresql-on-ubuntu/)

Remove any existing (non-compatible) version of the client

```
sudo apt remove postgresql-client-14
```

Update the APT package index

```
sudo apt update -y
```

Install the required packages

```
sudo apt install gnupg2 wget vim -y
```

Add the repository that provides the packages

```
sudo sh -c 'echo "deb https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
```

Now download the key signing for the repository:

```
curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc|sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/postgresql.gpg
```

Update your package list:

```
sudo apt update -y
```

Install PostgreSQL 16 on Ubuntu

```
sudo apt install postgresql-client-16
```

https://computingforgeeks.com/install-and-configure-postgresql-on-ubuntu/




Once installed, the client should be available someplace like:

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

## Create Database

```
CREATE DATABASE sampleDB;
```

Create a user with permissions to manage the database:

```
CREATE USER demo_user with encrypted password 'PassW0rd';
GRANT ALL PRIVILEGES ON DATABASE sampleDB to demo_user;
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

https://www.postgresql.org/docs/current/ddl-rowsecurity.html  
PostgreSQL: Documentation: 16: 5.8. Row Security Policies  

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

## IP Based Access Control

Postgres can be configured to allow certain IP addresses.

$PG_ROOT/data/pg_hba.conf
usually /var/lib/pgsql or /var/lib/postgresql 

vi /var/lib/pgsql/13/data/pg_hba.conf





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



## Installation

By default, a postgres server listens on port 5432.

When using docker, be sure you have important data being saved somewhere permanent. Do not use a temporary volume. 

How to install postgres in docker

Should have a build that works for all use cases
including backing up and restoring data

https://www.docker.com/blog/how-to-use-the-postgres-docker-official-image/  
How to Use the Postgres Docker Official Image | Docker  

I like postgres to run in a dedicated container and have other services all point to the same postgres instance. 

```
services:
  db:
    image: postgres:16-bookworm
    restart: unless-stopped
    healthcheck:
      test: ["CMD-SHELL", "pg_isready"]
      interval: 1s
      timeout: 5s
      retries: 10
    volumes:
      - ./postgres:/var/lib/postgresql/data
    environment:
      - POSTGRES_USER=${POSTGRES_USER} 
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD} 
    ports:
      - 127.0.0.1:5432:5432
      
networks:
  db-net:
    driver: bridge

```

For docker image details, see here:

https://github.com/docker-library/docs/blob/master/postgres/README.md  
docs/postgres/README.md at master · docker-library/docs · GitHub  

Go with the latest stable (typically even numbered major releases). May as well included the targeted linux distribution too? (bookworm = debian 12, bullseye = debian 11)
### .env

In the `.env`, the following key-values can be defined:

```
POSTGRES_USER=postgresql
POSTGRES_PASSWORD=example
```

Other options:

POSTGRES_USER – Specifies a user with superuser privileges and a database with the same name. Postgres uses the default user when this is empty.
POSTGRES_DB – Specifies a name for your database or defaults to the POSTGRES_USER value when left blank. 
POSTGRES_INITDB_ARGS – Sends arguments to postgres_initdb and adds functionality
POSTGRES_INITDB_WALDIR – Defines a specific directory for the Postgres transaction log. A transaction is an operation and usually describes a change to your database. 
POSTGRES_HOST_AUTH_METHOD – Controls the auth-method for host connections to all databases, users, and addresses
PGDATA – Defines another default location or subdirectory for database files

### Networking

To create a dedicate network, this could be added to `docker-compose.yaml`

```
networks:
  db-net:
    driver: bridge
    
```

If you want to configure another container to have access to the postgres database without using host networking, something like the following could work:

```
services:
  web:
    build: .
    ports:
      - "80:8000"
    depends_on:
      db:
        condition: service_healthy
    command: ["python", "app.py"]
 
networks:
  db-net      
```

Alternatively, `network: "host"` may be an easy way to grant network access to the host, but would miss out on some of the encapsulation features that docker provides. 

If you want to include a web based administration interface, include the following:

```
  adminer:
    image: adminer
    restart: unless-stopped
    ports:
      - 127.0.0.1:8080:8080
    depends_on:
      db:
        condition: service_healthy
```

https://duckduckgo.com/?t=ffab&q=running+postgres+in+docker&ia=web  
running postgres in docker at DuckDuckGo  
https://arctype.com/blog/postgres-kubernetes/  
Provisioning Postgres from Docker to Kubernetes  
https://arctype.com/?ref=arctype.com  
Arctype SQL Client for ClickHouse  
https://clickhouse.com/?ref=arctype  
Fast Open-Source OLAP DBMS - ClickHouse  

https://duckduckgo.com/?t=ffab&q=best+practices+for+initializing+a+new+postgres+database+docker&ia=web  
best practices for initializing a new postgres database docker at DuckDuckGo  
https://earthly.dev/blog/postgres-docker/  
Using Docker with Postgres: Tutorial and Best Practices - Earthly Blog  
https://stackoverflow.com/questions/38433835/the-best-practices-for-postgresql-docker-container-initialization-with-some-data  
The best practices for PostgreSQL docker container initialization with some data - Stack Overflow  

https://charlesbrandt.com/system/virtualization/docker-compose.html  
Docker Compose | Notes  
https://docs.docker.com/engine/security/rootless/  
Run the Docker daemon as a non-root user (Rootless mode) | Docker Docs  

### Replication and Architecture

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




## Backups 

If the system houses real data, set this up as soon as possible. 

It is possible to use a CLI / GUI tool for backups, but those are not commonly automated or scheduled. 

If it's data worth keeping, set up a backup routine. 

https://github.com/dhamaniasad/awesome-postgres#backups  
dhamaniasad/awesome-postgres: A curated list of awesome PostgreSQL software, libraries, tools and resources, inspired by awesome-mysql  


### Import / Export 

Export / Backup

```
pg_dump -U username dbname > dbexport.pgsql
```

Import / Restore

```
psql -U username dbname < dbexport.pgsql
```
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

https://www.a2hosting.com/kb/developer-corner/postgresql/import-and-export-a-postgresql-database  
How to import and export PostgreSQL databases  

### Docker

Backup

```
docker exec -t your-db-container pg_dumpall -c -U your-db-user | gzip > ./dump_$(date +"%Y-%m-%d_%H_%M_%S").gz
```

Restore

```
gunzip < your_dump.sql.gz | docker exec -i your-db-container psql -U your-db-user -d your-db-name
```

https://stackoverflow.com/questions/24718706/backup-restore-a-dockerized-postgresql-database  
Backup/Restore a dockerized PostgreSQL database - Stack Overflow  

https://stackoverflow.com/questions/39210274/how-do-i-backup-a-database-in-docker  
postgresql - how do i backup a database in docker - Stack Overflow  

https://duckduckgo.com/?t=ffab&q=backup+data+for+postgres+db+in+docker+container&ia=web  
backup data for postgres db in docker container at DuckDuckGo  

### Scheduling

Whatever script you use, add it to your crontab to run nightly:

```
0 0 * * * /path/to/backup/script.sh
```

### Retention

A more complete example to manage a rotating batch of backups:

``` sh
#!/bin/bash

# Load environment variables: POSTGRES_USER, POSTGRES_PASSWORD
# automatically export all variables
while IFS== read -r key value; do
  printf -v "$key" %s "$value" && export "$key"
done </path/to/.env
# If not set in .env, replace with the PostgreSQL user
# POSTGRES_USER="postgres" 

# Export the password for pg_dumpall and pg_dump to use
export PGPASSWORD="${POSTGRES_PASSWORD}"

# Configuration
BACKUP_DIR="/path/to/backup/root" # Replace with your backup root path

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
pg_dumpall --globals-only -U "${POSTGRES_USER}" -h 127.0.0.1 > "${DAILY_DIR}/globals.sql"

# Dump each database
for DB in $(psql -U "${POSTGRES_USER}" -h 127.0.0.1 -t -c "SELECT datname FROM pg_database WHERE NOT datistemplate AND datname != 'postgres';"); do
    pg_dump -Fc -U "${POSTGRES_USER}" -h 127.0.0.1 "${DB}" > "${DAILY_DIR}/${DB}.sql"
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


### Troubleshooting

```
pg_dumpall: error: server version: 15.1 (Debian 15.1-1.pgdg110+1); pg_dumpall version: 14.8 (Ubuntu 14.8-0ubuntu0.22.04.1)
```

```
sudo apt-get remove postgresql-client-14
sudo apt-get install postgresql-client-15
```

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



## Datatypes / Data Modeling

Formats to use when defining a schema


### text

> Generally, there is no downside to using `text` in terms of performance/memory. On the contrary: `text` is the optimum. Other types have more or less relevant downsides. `text` is literally the "preferred" type among string types in the Postgres type system, which can affect function or operator type resolution.




## Utilities, ORMs, Automatic APIs

https://github.com/dhamaniasad/awesome-postgres#backups  
dhamaniasad/awesome-postgres: A curated list of awesome PostgreSQL software, libraries, tools and resources, inspired by awesome-mysql  

### Prisma



### Metabases

Ready to go APIs built on top of a database. Some rely on Postgres exclusively, others can use many types of different databases. 

https://github.com/supabase/supabase  
supabase/supabase: The open source Firebase alternative. Follow to stay updated about our public Beta.  
https://github.com/metabase/metabase  
metabase/metabase: The simplest, fastest way to get business intelligence and analytics to everyone in your company  


## Materialized Views

https://duckduckgo.com/?t=ffab&q=materialized+view+postgresql&ia=web  
materialized view postgresql at DuckDuckGo  
https://www.postgresql.org/docs/current/rules-materializedviews.html  
PostgreSQL: Documentation: 15: 41.3. Materialized Views  
https://duckduckgo.com/?t=ffab&q=postgres+index+for+autocomplete+trie&ia=web  
postgres index for autocomplete trie at DuckDuckGo  
https://stackoverflow.com/questions/64353322/what-is-the-most-optimal-way-to-store-a-trie-for-typeahead-suggestions-in-distri  
database - What is the most optimal way to store a trie for typeahead suggestions in distributed systems? - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=prefix+tree+postgres&ia=web  
prefix tree postgres at DuckDuckGo  


