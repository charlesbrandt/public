# Monitoring

Know what is happening. Set up alerts for important system events. 

## Netdata


Useful tool for monitoring everything :)

https://app.netdata.cloud/sign-in

Easy to install

Sign in -> Connect Nodes

Use the `wget` or `curl` commands provided there. That way they get connected to your panel. 


## Retention

By default, local clients don't retain data for long -- it could easily overwhelm edge devices with limited resources. To maintain history, send to a central time series database.

https://learn.netdata.cloud/docs/export/external-databases  
Export metrics to external time-series databases | Learn Netdata  


https://duckduckgo.com/?t=ffab&q=netdata+data+retention&ia=web  
netdata data retention at DuckDuckGo  
https://learn.netdata.cloud/guides/longer-metrics-storage/  
Netdata Longer Metrics Retention | Learn Netdata  
https://duckduckgo.com/?t=ffcm&q=Netdata+Agent+parents&ia=web  
Netdata Agent parents at DuckDuckGo  
https://learn.netdata.cloud/docs/agent/database/  
Database | Learn Netdata  
https://learn.netdata.cloud/docs/agent/streaming/  
Streaming and replication | Learn Netdata  
https://learn.netdata.cloud/docs/agent/exporting  
Exporting reference | Learn Netdata  

Main site  
https://www.netdata.cloud/  

## Using

Nice diagram of some of the recent improvements to monitoring:

https://user-images.githubusercontent.com/43294513/236503200-b1c60298-d6d9-49bb-8927-ede3c78e25e3.png

https://github.com/netdata/netdata/releases/tag/v1.39.0
