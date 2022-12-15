# Crontab

Schedule tasks to run at regular intervals

This is a nice guide

https://linuxize.com/post/scheduling-cron-jobs-with-crontab/

Sometimes these settings are system specific and are useful to keep in a secrets vault file

```
ssh account@remote-system
```

```
crontab -e

*/2 * * * * command to run
```

crontab -l
(don't use sudo for crontab -- root does not have keys shared)

## Format

```
.---------------- minute (0 - 59)
| .-------------- hour (0 - 23)
| | .------------ day of month (1 - 31)
| | | .---------- month (1 - 12) OR jan,feb,mar ...
| | | | .-------- day of week (0 - 6) (Sunday=0 or 7) OR sun,mon,tue ...
| | | | |
* * * * * command to be executed
```

https://www.shellhacks.com/crontab-format-cron-job-examples-linux/

## Generators / Checkers

https://crontab.cronhub.io/  
Cron expression generator by Cronhub  
https://duckduckgo.com/?t=ffab&q=crontab+generator&ia=web  
crontab generator at DuckDuckGo  
https://crontab-generator.org/  
Crontab Generator - Generate crontab syntax  

## Logging

Helps to confirm that everything is running as expected

On a default installation the cron jobs get logged to

```
/var/log/syslog
```

You can see just cron jobs in that logfile by running

```
grep CRON /var/log/syslog
```

via:   
https://askubuntu.com/questions/56683/where-is-the-cron-crontab-log  
Where is the cron / crontab log? - Ask Ubuntu  


## Finding old files

```
find <Path_To_Old_Files> -type f -mtime +1 | xargs rm -f
```

https://ostechnix.com/how-to-find-and-delete-files-older-than-x-days-in-linux/

https://stackoverflow.com/questions/11119985/cronjob-to-remove-files-older-than-99-days  
linux - cronjob to remove files older than 99 days - Stack Overflow  

https://duckduckgo.com/?t=ffab&q=cron+schedule+rsync+and+delete+old+files&ia=web
cron schedule rsync and delete old files at DuckDuckGo
https://serverfault.com/questions/191201/how-to-use-rsync-to-delete-files-older-than-1-week
linux - How to use rsync to delete files older than 1 week - Server Fault


## Links

https://duckduckgo.com/?t=ffab&q=crontab+schedule+job&ia=web
crontab schedule job at DuckDuckGo
https://linuxize.com/post/scheduling-cron-jobs-with-crontab/
Scheduling Cron Jobs with Crontab | Linuxize


