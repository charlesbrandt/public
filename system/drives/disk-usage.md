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

https://duckduckgo.com/?t=ffab&q=ncdu+export+data&ia=software  
ncdu export data at DuckDuckGo  
https://dev.yorhel.nl/ncdu/jsonfmt  
Ncdu Export File Format  
https://dev.yorhel.nl/ncdu  
NCurses Disk Usage  


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

