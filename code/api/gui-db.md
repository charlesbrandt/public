# Database GUI

## Beekeeper

Spiritual successor to SQLectron? With write capabilities?

https://www.beekeeperstudio.io/pricing/  
Beekeeper Studio Pricing | Beekeeper Studio  
https://github.com/beekeeper-studio/beekeeper-studio  
beekeeper-studio/beekeeper-studio: Modern and easy to use SQL client for MySQL, Postgres, SQLite, SQL Server, and more. Linux, MacOS, and Windows.  
https://www.beekeeperstudio.io/get  
Download Beekeeper Studio | Beekeeper Studio  

## DBGate

Might support Mongo too? 

https://dbgate.org/  
DbGate | Open Source SQL+noSQL Database Client  


## DBeaver

Universal Database Tool
Free multi-platform database tool for developers, SQL programmers, database administrators and analysts. Supports all popular databases: MySQL, PostgreSQL, MariaDB, SQLite, Oracle, DB2, SQL Server, Sybase, MS Access, Teradata, Firebird, Derby, etc.

https://dbeaver.io/

https://duckduckgo.com/?t=ffab&q=dbeaver&ia=web  
dbeaver at DuckDuckGo  
https://github.com/dbeaver/dbeaver  
dbeaver/dbeaver: Free universal database tool and SQL client  
https://dbeaver.io/download/  
Download | DBeaver Community  

### Apt package

Snap package cannot read `~/.ssh` by default (by design). Could grant access, etc, but probably easier to just use apt package.

Download via: 

https://dbeaver.io/download/

Then

```
cd Downloads
sudo dpkg -i dbeaver
```

When trying to establish a connection via an SSH tunnel using the DBeaver Snap version, I experienced an error like 

```
dbeaver Can't initialize tunnelCannot run program "ssh-keygen": error=13, Permission deniedCannot run program "ssh-keygen": error=13, Permission deniederror=13, Permission deniederror=13, Permission denied
```

Make sure to use the correct passphrase for your private key. You can test decoding it outside of dbeaver:

```
ssh-keygen -y -f ~/.ssh/id_rsa > ~/.ssh/id_rsa.pub
```

DBeaver seems to expect the db user to have a password. Some postgres servers can be set up with ident mode where it relies on the system user accounts. 

https://duckduckgo.com/?t=ffab&q=dbeaver+connect+to+postgres+system+user&ia=web  
dbeaver connect to postgres system user at DuckDuckGo  
https://github.com/dbeaver/dbeaver/issues/2298  
Ident connection on PostgreSQL with Dbeaver · Issue #2298 · dbeaver/dbeaver · GitHub  

https://duckduckgo.com/?t=ffab&q=dbeaver+schema+diagram&ia=web  
dbeaver schema diagram at DuckDuckGo  
https://www.inmotionhosting.com/support/server/databases/create-an-er-diagram-in-dbeaver/  
How to Create an ER Diagram in DBeaver – InMotion Hosting Support Center  

### Snap version

```
sudo snap install dbeaver-ce
```

Then run with:

```
/snap/bin/dbeaver-ce
```

## Mongo 

See [Mongo notes](mongo.md#compass) (Mongo Compass)

DBeaver only supports Mongo Databases in the Enterprise edition:

https://dbeaver.com/databases/mongo/


## SQLectron

Good for viewing, but does not allow modifying any of the data via the app. 

https://duckduckgo.com/?t=canonical&q=postgres+gui&ia=web
postgres gui at DuckDuckGo
https://scalegrid.io/blog/which-is-the-best-postgresql-gui-2019-comparison/
Which Is the Best PostgreSQL GUI? 2021 Comparison

Additional research for sql based database guis done elsewhere.  
(these are just web apps, anyway)


## MySQL Workbench

[MySQL Workbench](mysql.md#mysqlworkbench)


## See also

https://duckduckgo.com/?q=data+grip&t=ffab&ia=web  
data grip at DuckDuckGo  
https://www.jetbrains.com/datagrip/  
DataGrip: The Cross-Platform IDE for Databases & SQL by JetBrains  
https://duckduckgo.com/?t=ffab&q=datagrip+open+source&ia=web  
datagrip open source at DuckDuckGo  
https://www.slant.co/options/210/alternatives/~datagrip-alternatives  
20 best alternatives to DataGrip as of 2022 - Slant  
https://github.com/topics/postgresql  
postgresql · GitHub Topics  
https://github.com/topics/erd  
erd · GitHub Topics  
