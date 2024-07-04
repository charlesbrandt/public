# Disk Usage

## Counting Files

```
find . -type f | wc -l
```

https://duckduckgo.com/?t=ffab&q=linux+show+number+of+files+in+directory&ia=web


## ncdu

Allows exporting / importing metadata about the scan to a file 
Helps when doing analysis on large storage arrays. 

```
sudo apt-get install ncdu
```

For a wrapper script to standardize calls, see:

```
/home/account/repos/bumble-bee/worker/ncdu.py
```

https://duckduckgo.com/?t=ffab&q=ncdu+export+data&ia=software  
ncdu export data at DuckDuckGo  
https://dev.yorhel.nl/ncdu/jsonfmt  
Ncdu Export File Format  
https://dev.yorhel.nl/ncdu  
NCurses Disk Usage  

Snapshot:

```
ncdu -e -o ~/system/ncdu-nuc10-home-20240527.ncdu
```

```
       Since scanning a large directory may take a while, you can scan a
       directory and export the results for later viewing:

         ncdu -0exo- / | gzip >export.gz
         # ...some time later:
         zcat export.gz | ncdu -ef-

```

Some intereesting options (from `man ncdu`)

```
       -f FILE
           Load the given file, which has earlier been created with the "-o"
           option. If FILE is equivalent to "-", the file is read from
           standard input.

           For the sake of preventing a screw-up, the current version of ncdu
           will assume that the directory information in the imported file
           does not represent the filesystem on which the file is being
           imported. That is, the refresh, file deletion and shell spawning
           options in the browser will be disabled.

       dir Scan the given directory.

       -o FILE
           Export all necessary information to FILE instead of opening the
           browser interface. If FILE is "-", the data is written to standard
           output.  See the examples section below for some handy use cases.

           Be warned that the exported data may grow quite large when
           exporting a directory with many files. 10.000 files will get you an
           export in the order of 600 to 700 KiB uncompressed, or a little
           over 100 KiB when compressed with gzip. This scales linearly, so be
           prepared to handle a few tens of megabytes when dealing with
           millions of files.

       -e  Enable extended information mode. This will, in addition to the
           usual file information, also read the ownership, permissions and
           last modification time for each file. This will result in higher
           memory usage (by roughly ~30%) and in a larger output file when
           exporting.

           When using the file export/import function, this flag will need to
           be added both when exporting (to make sure the information is added
           to the export), and when importing (to read this extra information
           in memory). This flag has no effect when importing a file that has
           been exported without the extended information.

           This enables viewing and sorting by the latest child mtime, or
           modified time, using 'm' and 'M', respectively.

   Scan Options
       These options affect the scanning progress, and have no effect when
       importing directory information from a file.

       -x  Do not cross filesystem boundaries, i.e. only count files and
           directories on the same filesystem as the directory being scanned.

       --exclude PATTERN
           Exclude files that match PATTERN. The files will still be displayed
           by default, but are not counted towards the disk usage statistics.
           This argument can be added multiple times to add more patterns.


EXAMPLES           
       To export from a cron job, make sure to replace "-1" with "-0" to
       suppress any unnecessary output.

       You can also export a directory and browse it once scanning is done:

         ncdu -o- | tee export.file | ./ncdu -f-

       The same is possible with gzip compression, but is a bit kludgey:

         ncdu -o- | gzip | tee export.gz | gunzip | ./ncdu -f-

       To scan a system remotely, but browse through the files locally:

         ssh -C user@system ncdu -o- / | ./ncdu -f-

       The "-C" option to ssh enables compression, which will be very useful
       over slow links. Remote scanning and local viewing has two major
       advantages when compared to running ncdu directly on the remote system:
       You can browse through the scanned directory on the local system
       without any network latency, and ncdu does not keep the entire
       directory structure in memory when exporting, so you won't consume much
       memory on the remote system.

```


## QDirStat

Light-weight installation for visual du

```
sudo apt-get install qdirstat
```

https://github.com/shundhammer/qdirstat  
shundhammer/qdirstat: QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)  
https://github.com/shundhammer/qdirstat/wiki/disk-usage-tools-compared  
disk usage tools compared · shundhammer/qdirstat Wiki  
https://github.com/topics/disk-space-analyzer  
disk-space-analyzer · GitHub Topics  
https://github.com/topics/storage  
storage · GitHub Topics  
https://github.com/topics/disk-space  
disk-space · GitHub Topics  
https://github.com/topics/disk-cleaner  
disk-cleaner · GitHub Topics  
https://github.com/topics/disk-usage  
disk-usage · GitHub Topics  
https://github.com/topics/du  
du · GitHub Topics  
https://github.com/topics/filesystem  
filesystem · GitHub Topics  
https://github.com/muesli/duf  
muesli/duf: Disk Usage/Free Utility - a better 'df' alternative  
https://github.com/dundee/gdu  
dundee/gdu: Fast disk usage analyzer with console interface written in Go  
https://github.com/diskoverdata/diskover-community  
diskoverdata/diskover-community: Diskover Community Edition - Open source file indexer, file search engine and data management and analytics powered by Elasticsearch  
https://duckduckgo.com/?t=ffab&q=k4dirstat&atb=v343-5ja&ia=software  
k4dirstat at DuckDuckGo  
https://github.com/jeromerobert/k4dirstat  
jeromerobert/k4dirstat: K4DirStat (KDE Directory Statistics) is a small utility program that sums up disk usage for directory trees, very much like the Unix 'du' command. It displays the disk space used up by a directory tree, both numerically and graphically (copied from the Debian package description).  
https://duckduckgo.com/?t=ffab&q=ncdu&atb=v343-5ja&ia=web  
ncdu at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=graphical+ncdu&atb=v343-5ja&ia=web  
graphical ncdu at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=linux+disk+analyzer&atb=v343-5ja&ia=web  
linux disk analyzer at DuckDuckGo  
https://www.how2shout.com/tools/best-disk-usage-analyzer-tools-for-linux-system.html  
Top 10 disk space analyzer tools to use on Linux systems in 2021  
https://duckduckgo.com/?t=ffab&q=qdirstat&atb=v343-5ja&ia=web  
qdirstat at DuckDuckGo  


### Disk Usage Analyzer 

`Disk Usage Analyzer` is included by default on Ubuntu. 
I think that QDirStat's ability to scan files is more efficient than `Disk Usage Analyzer`

### K4DirStat

Older version of QDirStat. Requires a much larger installation

visual disk usage utility:
http://kdirstat.sourceforge.net/

```
sudo apt-get install k4dirstat
```

