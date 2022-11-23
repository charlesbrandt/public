# Workers

It's important to have a system for scheduling jobs and knowing where they will be run. Makes it possible to allocate adequate resources and prevent core services from becoming bogged down. 

## Message Queues

The worker topic often ends up related to message queue systems. 

## Bull

https://github.com/Deadly0/bull-board-docker  
GitHub - Deadly0/bull-board-docker: Docker image for bull-board  

docker-compose configuration

docker volume create --name=redis_db_data

```
version: '3.5'

services:
  redis:
    container_name: redis
    image: redis:5.0-alpine
    restart: always
    ports:
      - 6379:6379
    volumes:
      - redis_db_data:/data

  bullboard:
    container_name: bullboard
    image: deadly0/bull-board
    restart: always
    ports:
      - 3000:3000
    environment:
      REDIS_HOST: redis
      REDIS_PORT: 6379
      REDIS_PASSWORD: example-password
      REDIS_USE_TLS: 'false'
      BULL_PREFIX: bull
    depends_on:
      - redis

volumes:
  redis_db_data:
    external: false

```

about:home  
New Tab  
https://github.com/taskforcesh/bullmq  
GitHub - taskforcesh/bullmq: BullMQ - Premium Message Queue for NodeJS based on Redis  
https://docs.bullmq.io/  
Quick Start - BullMQ  
https://docs.bullmq.io/what-is-bullmq  
What is BullMQ - BullMQ  
https://docs.bullmq.io/guide/introduction  
Introduction - BullMQ  
https://docs.bullmq.io/guide/connections  
Connections - BullMQ  
https://docs.bullmq.io/guide/flows  
Flows - BullMQ  
https://docs.bullmq.io/guide/queuescheduler  
QueueScheduler - BullMQ  
https://github.com/OptimalBits/bull  
GitHub - OptimalBits/bull: Premium Queue package for handling distributed jobs and messages in NodeJS.  
https://github.com/taskforcesh/bullmq/blob/master/docs/gitbook/api/bullmq.md  
bullmq/bullmq.md at master · taskforcesh/bullmq · GitHub  
https://github.com/taskforcesh  
Taskforce.sh Inc. · GitHub  
https://blog.taskforce.sh/  
Taskforce.sh Blog  

https://duckduckgo.com/?t=ffab&q=javascript+job+runner&ia=web  
javascript job runner at DuckDuckGo  
https://github.com/spilgames/job-runner/  
GitHub - spilgames/job-runner  
https://github.com/jaystack/jobkit  
GitHub - jaystack/jobkit: JavaScript job runner  
https://github.com/topics  
Topics on GitHub · GitHub  
https://duckduckgo.com/?t=ffab&q=javascript+bull&ia=web  
javascript bull at DuckDuckGo  
https://github.com/topics/queue  
queue · GitHub Topics · GitHub  
https://github.com/topics/job-queue  
job-queue · GitHub Topics · GitHub  
https://duckduckgo.com/?t=ffab&q=redis+maximum+number+of+connections&ia=web  
redis maximum number of connections at DuckDuckGo  
https://redis.io/topics/clients  
Redis Clients Handling – Redis  
https://redis.io/documentation  
Redis  
https://redis.io/  
Redis  
https://stackoverflow.com/questions/51517578/how-many-total-connection-or-max-connections-are-available-in-redis-server  
how many total connection or max connections are available in Redis Server? - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=redis+docker&ia=web  
redis docker at DuckDuckGo  
https://hub.docker.com/_/redis/  
Redis - Official Image | Docker Hub  
https://duckduckgo.com/?t=ffab&q=bull+vs+rabbitmq&ia=web  
bull vs rabbitmq at DuckDuckGo  
https://stackoverflow.com/questions/69680539/what-is-the-difference-between-bullmq-and-other-message-queue-implementations  
javascript - What is the difference between BullMQ and other message queue implementations? - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=bullmq+docker&ia=web  
bullmq docker at DuckDuckGo  
https://github.com/topics/bull  
bull · GitHub Topics · GitHub  
https://dockerquestions.com/2021/07/26/running-node-js-bullmq-on-containers-kubernetes-for-production-environments/  
Running node.js / bullmq on containers / kubernetes for production environments – Docker Questions  
https://github.com/felixmosh/bull-board  
GitHub - felixmosh/bull-board: 🎯 Queue background jobs inspector  
https://github.com/felixmosh/bull-board/blob/master/Dockerfile  
bull-board/Dockerfile at master · felixmosh/bull-board · GitHub  
