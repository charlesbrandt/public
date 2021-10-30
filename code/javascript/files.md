# File I/O

Files are a core aspect of computation. Either you're writing to a file or you're writing to a stream. Maybe both at the same time. 

The fact that you have to treat files differently, depending on if you're on a server vs client, makes for a point of confusion. In most languages, you're local. You're running at an elevated status. You have access to the system. 

Not in the browser. Think of that like a virtual machine. A container.

You can access what is available in the container. Nothing else. 

Some debate on what you can access from the web. Only other resources from the same address? The same source? (same-origin URLs)

So that also gives rise to auth. Authentication (who are you?) and Authorization (what can you do?). 

## WebDAV

if you think you want to do a lot of interaction with filesystems, consider switching to WebDAV. It's an existing standard for working with files on a remote system. Python is another option for lots of custom system side automation. Might be possible in node, but if you have existing experience with Python, this is a good time to switch. 

## Node JS (System)

https://nodejs.org/api/fs.html


https://stackoverflow.com/questions/10011011/using-node-js-how-do-i-read-a-json-file-into-server-memory
javascript - Using Node.JS, how do I read a JSON file into (server) memory? - Stack Overflow

Sync:

```
var fs = require('fs');
var obj = JSON.parse(fs.readFileSync('file', 'utf8'));
```

Async:

```
var fs = require('fs');
var obj;
fs.readFile('file', 'utf8', function (err, data) {
  if (err) throw err;
  obj = JSON.parse(data);
});
```

https://duckduckgo.com/?q=node+js+load+content+from+file&t=canonical&ia=qa
node js load content from file at DuckDuckGo

This can quickly move beyond the scope of just being Javascript specific to a system level [Distributed File System](~/public/storage/storage.md)


## List files in a directory

```
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


## How much space can javascript use in browser? 

2MB to 10MB

[According to](https://www.sitepoint.com/html5-local-storage-revisited/)


"""
Unlike cookies, data stored using local storage isn't sent back to the server. All data stays on the client, and you can currently store from 2MB to 10MB. This limit is tied to the specific browser, protocol (HTTP or HTTPS), port, and top level domain in use. Mar 12, 2015
"""

Not a ton, but plenty for a robust database. 





Loading files in the browser requires either local storage or input from the user. 

> One of the most common things you'll want to do with just about any programming language is open and read a file. With most languages, this is pretty simple, but for JavaScript veterans it might seem a bit weird. For so many years JavaScript was only available in the browser, so front-end developers may only be familiar with the FileReader API or similar.

via: https://stackabuse.com/read-files-with-node-js/
Read Files with Node.js


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


## Node file system interaction

If you want to work programmaticly with a filesystem from node, not many libraries available. Maybe because it's not necessary to work at that level? See above 

See also [WebDAV](webdav.md)

https://duckduckgo.com/?q=node+list+files+in+folder&t=canonical&ia=web
node list files in folder at DuckDuckGo
https://nodejs.org/api/fs.html
File system | Node.js v15.1.0 Documentation
https://stackabuse.com/node-list-files-in-a-directory/
Node: List Files in a Directory
https://duckduckgo.com/?q=node+directory-tree&t=canonical&ia=web
node directory-tree at DuckDuckGo
https://stackoverflow.com/questions/7041638/walking-a-directory-with-node-js
https://stackoverflow.com/questions/7041638/walking-a-directory-with-node-js
https://github.com/nspragg/filehound
GitHub - nspragg/filehound: Flexible and fluent interface for searching the file system
https://docs.npmjs.com/cli/v6/configuring-npm/folders
folders | npm Docs
https://github.com/mihneadb/node-directory-tree
GitHub - mihneadb/node-directory-tree: Convert a directory tree to a JS object.
https://github.com/MrRaindrop/tree-cli
GitHub - MrRaindrop/tree-cli: 🌴List contents of directories in tree-like format.

## File Sizes

https://github.com/sindresorhus/pretty-bytes
sindresorhus/pretty-bytes: Convert bytes to a human readable string: 1337 → 1.34 kB
https://web.archive.org/web/20150324153922/https://pacoup.com/2009/05/26/kb-kb-kib-whats-up-with-that/
kb, kB, KiB… What’s Up With That? | Pacoup.com

