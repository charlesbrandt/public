# File I/O

Files are a core aspect of computation. Either you're writing to a file or you're writing to a stream. Maybe both at the same time. 

The fact that you have to treat files differently, depending on if you're on a server vs client, makes for a point of confusion. In most languages, you're local. You're running at an elevated status. You have access to the system. 

Not in the browser. Think of that like a virtual machine. A container.

You can access what is available in the container. You can access remote resources.

## Node JS (System)

https://nodejs.org/api/fs.html

## List files in a directory

```js
const fs = require('fs');

// directory path
const dir = './node_modules/';

// list all files in the directory
fs.readdir(dir, (err, files) => {
    if (err) {
        throw err;
    }

    // files object contains all files names
    // log them on console
    files.forEach(file => {
        console.log(file);
    });
});
```

https://attacomsian.com/blog/nodejs-list-directory-files

## Load JSON from file

Async:

```js
var fs = require('fs');
var obj;
fs.readFile('file', 'utf8', function (err, data) {
  if (err) throw err;
  obj = JSON.parse(data);
});
```

Sync:

```js
var fs = require('fs');
var obj = JSON.parse(fs.readFileSync('file', 'utf8'));
```

https://stackoverflow.com/questions/10011011/using-node-js-how-do-i-read-a-json-file-into-server-memory  
javascript - Using Node.JS, how do I read a JSON file into (server) memory? - Stack Overflow  


https://duckduckgo.com/?q=node+js+load+content+from+file&t=canonical&ia=qa
node js load content from file at DuckDuckGo

This can quickly move beyond the scope of just being Javascript specific to a system level Distributed File System.

## Walk a file system

```js
const fs = require("fs");
const path = require("path");

let walk = function (dir) {
  // get the contents of dir
  console.log("Starting walk for", dir);
  fs.readdir(dir, (e, items) => {
    // console.log("Found contents", items);
    items.forEach((item) => {
      // construct the item path
      let itemPath = path.join(dir, item);

      fs.stat(itemPath, (e, stats) => {
        console.log(itemPath, stats);

        // use stats to find out if the current item is a dir
        if (stats && stats.isDirectory()) {
          walk(itemPath);
        }
      });
    });
  });
};

walk(process.cwd());


```

Inspired by:  
https://dustinpfister.github.io/2018/07/20/nodejs-ways-to-walk-a-file-system/  
Some Ways to walk a file system in node.js | Dustin John Pfister at github pages  

https://duckduckgo.com/?t=ffab&q=javascript+node+walk+filesystem+&ia=web  
javascript node walk filesystem at DuckDuckGo  
https://github.com/jprichardson/node-klaw  
jprichardson/node-klaw: A Node.js file system walker with a Readable stream interface. Extracted from fs-extra.  
https://www.npmjs.com/package/klaw  
klaw - npm  
https://www.npmjs.com/package/node-dir  
node-dir - npm  
https://dustinpfister.github.io/2017/05/14/nodejs-rimraf/  

https://stackabuse.com/node-list-files-in-a-directory/  
Node: List Files in a Directory  
https://duckduckgo.com/?q=node+directory-tree&t=canonical&ia=web  
node directory-tree at DuckDuckGo  
https://stackoverflow.com/questions/7041638/walking-a-directory-with-node-js  
https://stackoverflow.com/questions/7041638/walking-a-directory-with-node-js  
https://github.com/nspragg/filehound  
GitHub - nspragg/filehound: Flexible and fluent interface for searching the file system  


## Read & Parse a File in a Browser

Loading files in the browser requires either local storage or input from the user. 

```js
      const fileReader = new FileReader();
      fileReader.addEventListener("load", () => {
        console.log("Parsing file content");
        
        // console.log('File content:', fileReader.result)
        
        const lines = fileReader.result.split(/\r?\n/g);
        
        lines.forEach((line, i) => {
            console.log('Current line: ', line.match(error1))
          }
        });
      });
      fileReader.readAsText(this.files[0]);
```


> One of the most common things you'll want to do with just about any programming language is open and read a file. With most languages, this is pretty simple, but for JavaScript veterans it might seem a bit weird. For so many years JavaScript was only available in the browser, so front-end developers may only be familiar with the FileReader API or similar.

via: https://stackabuse.com/read-files-with-node-js/  
Read Files with Node.js  


### How much space can javascript use in browser? 

2MB to 10MB

[According to](https://www.sitepoint.com/html5-local-storage-revisited/)


> Unlike cookies, data stored using local storage isn't sent back to the server. All data stays on the client, and you can currently store from 2MB to 10MB. This limit is tied to the specific browser, protocol (HTTP or HTTPS), port, and top level domain in use. Mar 12, 2015

Not a ton, but plenty for a robust database. 


## File Input

https://duckduckgo.com/?t=canonical&q=javascript+access+uploaded+file&ia=web  
javascript access uploaded file at DuckDuckGo  
https://stackoverflow.com/questions/16505333/get-the-data-of-uploaded-file-in-javascript  
html - get the data of uploaded file in javascript - Stack Overflow  
https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications  
Using files from web applications - Web APIs | MDN  
https://web.dev/read-files/#toc-reading-files  
Read files in JavaScript  
https://duckduckgo.com/?q=vue+html5+input+file&t=canonical&ia=web  
vue html5 input file at DuckDuckGo  
https://stackoverflow.com/questions/45179061/file-input-on-change-in-vue-js  
javascript - File input on change in vue.js - Stack Overflow  
https://www.digitalocean.com/community/tutorials/vuejs-file-reader-component  
Creating a Vue.js File Reader Component Using the FileReader API | DigitalOcean  


Good example of splitting files up in the browser to work within max-upload-size limits on the server

https://deliciousbrains.com/using-javascript-file-api-to-avoid-file-upload-limits/

Example created/tested in music_server/ui/components/FileReader.vue and pages/Editor.vue


## File Sizes

https://github.com/sindresorhus/pretty-bytes  
sindresorhus/pretty-bytes: Convert bytes to a human readable string: 1337 → 1.34 kB  
https://web.archive.org/web/20150324153922/https://pacoup.com/2009/05/26/kb-kb-kib-whats-up-with-that/  
kb, kB, KiB… What’s Up With That? | Pacoup.com  

## CSV

[CSV Parsing](csv.md)

