# Message Queues

Handy tool for distributing work across multiple systems. 

Very nice overview of the concept:

https://www.cloudamqp.com/blog/what-is-message-queuing.html
What is message queuing? - CloudAMQP

More technical version:

https://www.ibm.com/cloud/learn/message-queues
Message Queues: An Introduction | IBM


## Bull & BullMQ

Javascript original. Tried and true. Good if you need to support platforms that tail behind in the version of Node.

https://github.com/OptimalBits/bull


Latest version. Written in Typescript (but does not require using Typescript). 

https://github.com/taskforcesh/bullmq


Bull queues can be used by any javascript application easily. 

They only require access to the Redis store being used to coordinate all of the workers. 


Nice to have an easy to include system, especially if it works in javascript.

https://docs.bullmq.io/what-is-bullmq  
What is BullMQ - BullMQ  
https://docs.bullmq.io/  
Quick Start - BullMQ  


```
yarn add bullmq
```

### Connections

Be sure to configure where your redis server is when using the library (and make sure that the location running the code can access the redis server)

https://docs.bullmq.io/guide/connections


```
import { Queue, Worker } from 'bullmq'

// Create a new connection in every instance
const myQueue = new Queue('myqueue', { connection: {
  host: "myredis.taskforce.run",
  port: 32856
}});

const myWorker = new Worker('myworker', async (job)=>{}, { connection: {
  host: "myredis.taskforce.run",
  port: 32856
}});
```


### Docker

It's a good idea to run your redis store in a separate container, but the bull que and even the bull-board can be added in the API.


Possible to run all the necessary infrastructure under docker. In `docker-compose.yml`, add the following containers and volumes

https://github.com/Deadly0/bull-board-docker

```
  redis:
    container_name: redis
    image: redis:5.0-alpine
    restart: unless-stopped
    ports:
      - 6379:6379
    volumes:
      # - db_redis:/data
      - ./db_redis:/data
```


```
version: "3"
services:

  redis:
    container_name: redis
    image: redis:5.0-alpine
    restart: unless-stopped
    ports:
      - 6379:6379
    volumes:
      - db_redis:/data  
      
  bullboard:
    container_name: bullboard
    image: deadly0/bull-board
    restart: unless-stopped
    ports:
      - 9998:3000
    environment:
      REDIS_HOST: redis
      REDIS_PORT: 6379
      REDIS_PASSWORD: example-password
      REDIS_USE_TLS: 'false'
      BULL_PREFIX: bull
    depends_on:
      - redis


volumes:
  db_redis:
    external: false


```

## References

https://duckduckgo.com/?t=ffab&q=bull+mq&ia=web  
bull mq at DuckDuckGo  
https://docs.bullmq.io/what-is-bullmq  
What is BullMQ - BullMQ  
https://docs.bullmq.io/  
Quick Start - BullMQ  
https://docs.bullmq.io/what-is-bullmq  
What is BullMQ - BullMQ  
https://github.com/taskforcesh/bullmq  
taskforcesh/bullmq: BullMQ - Premium Message Queue for NodeJS based on Redis  
https://docs.bullmq.io/guide/workers  
Workers - BullMQ  
https://docs.bullmq.io/guide/connections  

Connections leverage ioredis for talking to the Redis database. Note that the version of Redis needs to be 5.0 or higher. 

Connections - BullMQ  
https://github.com/luin/ioredis/blob/master/API.md  
ioredis/API.md at master · luin/ioredis  


https://duckduckgo.com/?t=ffab&q=bullboard&ia=web  
bullboard at DuckDuckGo  
https://www.npmjs.com/package/bull-board  
bull-board - npm  
https://github.com/felixmosh/bull-board  
GitHub - felixmosh/bull-board: 🎯 Queue background jobs inspector  
https://github.com/OptimalBits/bull  
deadly0/bull-board at DuckDuckGo  
https://github.com/Deadly0/bull-board-docker/blob/master/Dockerfile  
bull-board-docker/Dockerfile at master · Deadly0/bull-board-docker · GitHub  
https://github.com/Deadly0/bull-board-docker/tree/master/src  
bull-board-docker/src at master · Deadly0/bull-board-docker · GitHub  
https://github.com/topics/message-queue  


OptimalBits/bull: Premium Queue package for handling distributed jobs and messages in NodeJS.  
https://github.com/OptimalBits/bull/blob/develop/REFERENCE.md  
bull/REFERENCE.md at develop · OptimalBits/bull  
https://duckduckgo.com/?t=ffab&q=deadly0%2Fbull-board&ia=web  

message-queue · GitHub Topics · GitHub  
https://github.com/OptimalBits/bull  
GitHub - OptimalBits/bull: Premium Queue package for handling distributed jobs and messages in NodeJS.  
https://github.com/taskforcesh/bullmq  
GitHub - taskforcesh/bullmq: BullMQ - Premium Message Queue for NodeJS based on Redis  

https://duckduckgo.com/?q=bullmq+run+python+job&t=ffab&ia=web  
bullmq run python job at DuckDuckGo  


## See Also

https://duckduckgo.com/?t=ffab&q=message+queuing&ia=web
message queuing at DuckDuckGo
