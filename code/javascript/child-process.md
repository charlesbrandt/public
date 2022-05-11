# Child Processes

Kicking off a system process, a subprocess. 

```
const { exec } = require("child_process");

```

## `spawn` vs `exec`

Seems like the output from `exec` is kept in a buffer until the command finishes. May not be ideal for live updates. 


https://www.freecodecamp.org/news/node-js-child-processes-everything-you-need-to-know-e69498fe970a/  
Node.js Child Processes: Everything you need to know  

## Status updates via a bull message queue

WIP / TODO  
document a reproduceable example. Post a question. 

I am trying to spawn a child process and send the child process standard output (cli) as updates to a [bull message queue.](/code/api/message-queue.md#jobs)


Consider a basic python script 

```python
#!/usr/bin/env python3

from time import sleep

def usage():
    print(__doc__)

def delay_output(source):
    for i in range(10):
        print("on iteration", i)
        sleep(3)

if __name__ == '__main__':
    delay_output()
```

Then, a bull worker script

```js
const { spawn, exec } = require("child_process");
const config = require("../../config")
const Queue = require("bull");

const boilerplateQueue = new Queue("boilerplate", config.redis.url);
// console.log("Queue:", boilerplateQueue)

boilerplateQueue.process(function (job, done) {
  // See all job details
  console.log("Worker got a job!", job);
  // console.log("Worker got a job!");

  const dir = job.data.source;

  const command = `python boilerplate.py ${dir}`
  console.log("Initiating command:", command)
  // All output shows up at the end of the job
  // const child = spawn('python', ['boilerplate.py', dir], {
  //   // stdio: 'inherit',
  //   // shell: true
  // });

  // This is the option that I would most like to get working
  // All output shows up at the end of the job
  const child = spawn(command, {
    // stdio: 'inherit',
    shell: true
  });

  child.stdout.on('data', (data) => {
    console.log(`child stdout:\n${data}`);
    job.log(data);
  });

  // const child = spawn(`python boilerplate.py ${dir}`, {
  //   stdio: 'inherit',
  //   shell: true
  // });

  // process.stdout never gets triggered
  // process.stdout.on('data', (data) => {
  // stdout is undefined, even after importing
  // stdout.on('data', (data) => {
  //   job.log(data);
  //   console.log(`child stdout:\n${data}`);
  // });


  // child.on('exit', (code) => {
  child.on('close', (code) => {
    done(null, `finished with code ${code}`);
    // job.progress(data);
    console.log(`child exit: ${code}`);
  });

  // exec(`python boilerplate.py ${dir}'`, (error, stdout, stderr) => {
  //   if (error) {
  //     done(new Error(`error: ${error.message}`));
  //     // console.log(`error: ${error.message}`);
  //     return;
  //   }
  //   if (stderr) {
  //     done(new Error(`error: ${stderr}`));
  //     // console.log(`stderr: ${stderr}`);
  //     return;
  //   }

  //   // job.progress(42);

  //   // call done when finished
  //   done(null, `stdout: ${stdout}`);
  //   // console.log(`stdout: ${stdout}`);
  // });


  console.log("child_process call started");

});

console.log("Boilerplate worker listening for jobs");

```




https://duckduckgo.com/?t=ffab&q=node+exec+child+process+get+updates&ia=web  
node exec child process get updates at DuckDuckGo  
https://nodejs.org/api/child_process.html#optionsstdio  
Child process | Node.js v18.0.0 Documentation  
https://nodejs.org/api/child_process.html#subprocessstdout  
Child process | Node.js v18.0.0 Documentation  

https://duckduckgo.com/?t=ffab&q=node+child-process+stdout+trigger+on+any+event&ia=web  
node child-process stdout trigger on any event at DuckDuckGo  
https://stackoverflow.com/questions/34899519/node-child-process-event-listen  
javascript - Node child process event listen - Stack Overflow  
https://duckduckgo.com/?t=ffab&q=javascript+child_process+data+event+only+triggered+on+exit&ia=web  
javascript child_process data event only triggered on exit at DuckDuckGo  
https://www.brainbell.com/javascript/child-process.html  
Cross platform child process: spawn, fork, exec ad execFile in Node.js  


https://duckduckgo.com/?t=ffab&q=bull+message+queue+spawn+child+process&ia=web  
bull message queue spawn child process at DuckDuckGo  
https://github.com/OptimalBits/bull/issues/923  
A lot of processes · Issue #923 · OptimalBits/bull  
https://github.com/OptimalBits/bull/issues/488  
[Feature] Support for running jobs in child processes · Issue #488 · OptimalBits/bull  
https://github.com/jtassin/bull/commit/98296d957744f459fa8dbece9b6644a88273196b  
docs: add a hyperlink to #488 in README · jtassin/bull@98296d9  
