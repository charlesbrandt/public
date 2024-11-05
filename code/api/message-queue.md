# Message Queues

Handy tool for distributing work across multiple systems. 

Very nice overview of the concept:

https://www.cloudamqp.com/blog/what-is-message-queuing.html  
What is message queuing? - CloudAMQP  

More technical version:

https://www.ibm.com/cloud/learn/message-queues  
Message Queues: An Introduction | IBM  

Many solutions available

https://github.com/topics/job-queue  
job-queue · GitHub Topics  
https://github.com/topics/background-jobs  
background-jobs · GitHub Topics  
https://github.com/topics/task-queue  
task-queue · GitHub Topics · GitHub  
https://github.com/topics/worker-pool  
worker-pool · GitHub Topics · GitHub  



## Celery

https://github.com/topics/celery  
celery · GitHub Topics  
https://github.com/celery/celery  
celery/celery: Distributed Task Queue (development branch)  
https://docs.celeryq.dev/en/stable/getting-started/first-steps-with-celery.html#keeping-results  
First Steps with Celery — Celery 5.3.1 documentation  
https://docs.celeryq.dev/en/stable/userguide/configuration.html#std-setting-result_backend  
Configuration and defaults — Celery 5.3.1 documentation  
https://docs.celeryq.dev/en/stable/userguide/configuration.html#conf-rpc-result-backend  
Configuration and defaults — Celery 5.3.1 documentation  
https://duckduckgo.com/?t=ffab&q=celery+chained+events&ia=web  
celery chained events at DuckDuckGo  
https://stackoverflow.com/questions/55279820/running-chained-celery-tasks-at-specific-times  
python - Running Chained Celery tasks at specific times - Stack Overflow  

## pg-boss

https://duckduckgo.com/?q=MESSAGE_QUEUE_TYPE%3Dpg-boss&t=ffab&ia=web  
MESSAGE_QUEUE_TYPE=pg-boss at DuckDuckGo  
https://github.com/timgit/pg-boss  
GitHub - timgit/pg-boss: Queueing jobs in Node.js using PostgreSQL like a boss  
https://duckduckgo.com/?t=ffab&q=Error+response+from+daemon%3A+invalid+endpoint+settings%3A+network-scoped+alias+is+supported+only+for+containers+in+user+defined+networks+&ia=web  

This is used by Twenty CRM
Looks like a good solution if using postgres already

## RabbitMQ

Seems to be a favorite. Has the ability to be accessed from many different languages, which is a limitation of bullmq (js only). 

https://duckduckgo.com/?t=ffab&q=amqp&ia=web  
amqp at DuckDuckGo  
https://www.amqp.org/  
Home | AMQP  
https://duckduckgo.com/?t=ffab&q=celery+python&ia=web  
celery python at DuckDuckGo  
https://pypi.org/project/celery/  
celery · PyPI  
https://docs.celeryq.dev/en/latest/  
Celery - Distributed Task Queue — Celery 5.3.0b1 documentation  
https://docs.celeryq.dev/en/latest/getting-started/introduction.html  
Introduction to Celery — Celery 5.3.0b1 documentation  
https://rabbitmq.com/  
Messaging that just works — RabbitMQ  
https://rabbitmq.com/#community  
Messaging that just works — RabbitMQ  
https://github.com/rabbitmq?q=rabbitmq  
RabbitMQ  
https://github.com/rabbitmq/rabbitmq-server  
rabbitmq/rabbitmq-server: Open source RabbitMQ: core server and tier 1 (built-in) plugins  
https://github.com/topics/amqp  
amqp · GitHub Topics  
https://github.com/topics/mqtt  
mqtt · GitHub Topics  
https://github.com/emqx/emqx  
emqx/emqx: The most scalable open-source MQTT broker for IoT  
https://github.com/mqttjs/MQTT.js  
mqttjs/MQTT.js: The MQTT client for Node.js and the browser  
https://mqtt.org/  
MQTT - The Standard for IoT Messaging  
https://duckduckgo.com/?t=ffab&q=redis+message+queue&ia=web  
redis message queue at DuckDuckGo  
https://hevodata.com/learn/redis-message-queue/  
Redis Message Queue: 4 Easy Steps to Build a Message Broker - Learn | Hevo  
https://duckduckgo.com/?t=ffab&q=redis+vs+rabbitmq&ia=web  
redis vs rabbitmq at DuckDuckGo  
https://stackoverflow.com/questions/29539443/redis-vs-rabbitmq-as-a-data-broker-messaging-system-in-between-logstash-and-elas  
Redis Vs RabbitMQ as a data broker/messaging system in between Logstash and elasticsearch - Stack Overflow  
https://www.educba.com/rabbitmq-vs-redis/  
RabbitMQ vs Redis | Top 9 Differences You Should Know  
https://duckduckgo.com/?t=ffab&q=celery+vs+bull&ia=web  
celery vs bull at DuckDuckGo  
https://stackoverflow.com/questions/28470796/equivalent-of-celery-in-node-js  
node.js - Equivalent of Celery in Node JS - Stack Overflow  
https://thenewstack.io/how-kafka-and-redis-solve-stream-processing-challenges/  
How Kafka and Redis Solve Stream-Processing Challenges – The New Stack  


## RQ

Redis Queue. As the name implies, this will only work with Redis. If the Redis licensing is a problem, look elsewhere. Redis is still an amazingly performant and low resource shared state management system. 

https://github.com/rq/rq  
rq/rq: Simple job queues for Python  
https://duckduckgo.com/?t=ffab&q=using+redis+as+a+task+queue+python&ia=web  
using redis as a task queue python at DuckDuckGo  
https://realpython.com/flask-by-example-implementing-a-redis-task-queue/  
Flask by Example – Implementing a Redis Task Queue – Real Python  




## Bull & BullMQ

Javascript original. Tried and true. Good if you need to support platforms that lag behind from the latest version of Node.

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

### Jobs

One nice thing about using Bull is that it's easy to kick off jobs via your API, especially if you're already running javascript there (e.g. Express)

https://github.com/OptimalBits/bull/blob/develop/REFERENCE.md#queueadd

"Boilerplate Request" is the name of the job  
without providing, displays show "__default__" for every job  
however, the job queue needs to leverage that value too  
// .add("Boilerplate Request", req.body)  


```js
var express = require("express");
var router = express.Router();
var config = require("../config");

var Queue = require("bull");

const boilerplateQueue = new Queue("boilerplate", config.redis.url);

router.post("/new", (req, res, next) => {
  console.log("CREATING NEW BOILERPLATE JOB", req.body);
  boilerplateQueue
    // .add("Boilerplate Request", req.body)
    .add(req.body)
    .then((created) => {
      res.json(created);
    })
    .catch((err) => {
      next(err);
    });
});

module.exports = router;
```

#### Child Processes

Often useful to leverage a child process for calling jobs
[Child Processes](/code/javascript/child-process.md)



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

## Bullboard

https://github.com/felixmosh/bull-board  
GitHub - felixmosh/bull-board: 🎯 Queue background jobs inspector  

### Install

Seems best to integrate with an existing express stack  
but also possible to put it in its own container if that works better

```
yarn add @bull-board/express
```

```
const express = require('express')
const Queue = require('bull')
const { createBullBoard } = require('@bull-board/api')
const { BullAdapter } = require('@bull-board/api/bullAdapter')
const { ExpressAdapter } = require('@bull-board/express')

const someQueue = new Queue('someQueueName')
const someOtherQueue = new Queue('someOtherQueueName')

const serverAdapter = new ExpressAdapter();

const { addQueue, removeQueue, setQueues, replaceQueues } = createBullBoard({
  queues: [
    new BullAdapter(someQueue),
    new BullAdapter(someOtherQueue),
  ],
  serverAdapter:serverAdapter
})

const app = express()

serverAdapter.setBasePath('/admin/queues')
app.use('/admin/queues', serverAdapter.getRouter());

// other configurations of your server
```


### Docker

https://github.com/Deadly0/bull-board-docker

https://github.com/Deadly0/bull-board-docker/blob/master/Dockerfile  
bull-board-docker/Dockerfile at master · Deadly0/bull-board-docker · GitHub  
https://github.com/Deadly0/bull-board-docker/tree/master/src  
bull-board-docker/src at master · Deadly0/bull-board-docker · GitHub  
https://github.com/topics/message-queue  


## Agenda

https://github.com/agenda/agenda  
agenda/agenda: Lightweight job scheduling for Node.js  



## See Also

https://duckduckgo.com/?t=ffab&q=message+queuing&ia=web
message queuing at DuckDuckGo


[Redis](redis.md)
