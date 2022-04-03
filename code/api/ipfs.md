# InterPlanetary File System (IPFS)

Think of IPFS as a place to store your data. The binaries. The things that don't change often. Track the CIDs that are important to you and your purpose. 

Use an IPFS client (js-client if browser based) to retrieve the information your application needs


https://ipfs.io/  
IPFS Powers the Distributed Web  
  
https://ipfs.io/#how  
IPFS Powers the Distributed Web  
  
https://js.ipfs.io/  
JS IPFS  
  
https://en.wikipedia.org/wiki/InterPlanetary_File_System  
InterPlanetary File System - Wikipedia  

## Data sets

https://awesome.ipfs.io/datasets/  
Datasets  

https://ipfs.io/ipfs/QmXyNMhV8bQFp6wzoVpkz3NqDi7Fj72Deg7KphAuew3RYU/  
Wistful Books  

## Installation

```
npm install ipfs-core
```

Generate CID for file:

```
import * as IPFS from 'ipfs-core'

const ipfs = await IPFS.create()
const { cid } = await ipfs.add('Hello world', {onlyHash: true})
console.info(cid)
```

The `add` api documentation is available:  
https://github.com/ipfs/js-ipfs/blob/master/docs/core-api/FILES.md#options  

## Topics of interest

https://duckduckgo.com/?q=ipfs+video+streaming&t=canonical&ia=web  
ipfs video streaming at DuckDuckGo  
https://blog.fission.codes/experimenting-with-hls-video-streaming-and-ipfs/  
Experimenting with HLS Video Streaming and IPFS  

https://duckduckgo.com/?t=canonical&q=ipfs+pinning&ia=web  
ipfs pinning at DuckDuckGo  
https://docs.ipfs.io/how-to/pin-files/  
Pin files | IPFS Docs  




## InterPlanetary Linked Data (IPLD)

Data objects for the distributed web. Basically JSON objects that allow linking to other nodes in the IPFS network. Linking data with this method is a flexible model.

https://docs.ipld.io/#the-data-model  
InterPlanetary Linked Data | IPLD Documentation  
https://docs.ipld.io/tutorial.html#addressing  
Thinking in data structures | IPLD Documentation  
https://docs.ipld.io/getting-started/js.html#storing-ipld-data-in-ipfs  
Getting Started with IPLD in JavaScript | IPLD Documentation  
https://docs.ipld.io/docs/gtd/#getting-things-done-with-ipld  
IPLD Documentation  
https://specs.ipld.io/data-model-layer/data-model.html#kinds-reference  
Specification: IPLD Data Model | IPLD Specifications  
https://specs.ipld.io/schemas/  
IPLD Specifications  
https://specs.ipld.io/schemas/feature-summary.html#constructs-for-describing-common-data-forms  
IPLD Schemas Feature Summary | IPLD Specifications  
https://duckduckgo.com/?q=IPLD+date+object&t=canonical&ia=images  
IPLD date object at DuckDuckGo  
https://duckduckgo.com/?t=canonical&q=json+represent+date&ia=web  
json represent date at DuckDuckGo  
https://stackoverflow.com/questions/91413/how-to-represent-date-and-or-time-information-in-json  
data formats - How to represent date and/or time information in JSON? - Stack Overflow  
https://en.wikipedia.org/wiki/ISO_8601  
ISO 8601 - Wikipedia  

### Video Overview

https://docs.ipld.io/docs/media/#foundations-for-decentralization-data-with-ipld-gpn19  
IPLD Documentation  

### Name System

https://duckduckgo.com/?t=canonical&q=ipfs+name+system&ia=web  
ipfs name system at DuckDuckGo  
https://docs.ipfs.io/concepts/ipns/#example-ipns-setup-with-cli  
IPNS | IPFS Docs  
https://docs.ipfs.io/concepts/dnslink/#resolve-dnslink-name  
DNSLink | IPFS Docs  
https://dnslink.io/  
DNSLink - link content with dns  
  
https://duckduckgo.com/?t=canonical&q=use+ipns+in+ipld&ia=web  
use ipns in ipld at DuckDuckGo  

### File System

https://docs.ipfs.io/concepts/file-systems/#unix-file-system-unixfs  
File systems | IPFS Docs  
https://github.com/ipfs/camp/tree/master/CORE_AND_ELECTIVE_COURSES/CORE_COURSE_A  
camp/CORE_AND_ELECTIVE_COURSES/CORE_COURSE_A at master · ipfs/camp · GitHub  

https://medium.com/towardsblockchain/understanding-ipfs-in-depth-2-6-what-is-interplanetary-linked-data-ipld-c8c01551517b  
Understanding IPFS in Depth(2/6): What is InterPlanetary Linked Data(IPLD)? | by vasa | towardsblockchain | Medium  
https://duckduckgo.com/?t=canonical&q=IPFS+MFS+mount+in+linux&ia=web  
IPFS MFS mount in linux at DuckDuckGo  
https://github.com/tableflip/ipfs-fuse  
GitHub - tableflip/ipfs-fuse: Mount IPFS MFS as a FUSE volume using Node.js  
https://duckduckgo.com/?t=canonical&q=ipfs+access+control&ia=web  
ipfs access control at DuckDuckGo  
https://github.com/ipfs/notes/issues/376  
why don't we need access control ? · Issue #376 · ipfs/notes · GitHub  
https://blog.textile.io/the-5-steps-to-end-to-end-encrypted-photo-storage-and-sharing/  
The 5 steps to end-to-end encrypted photo storage and sharing  
  
### Database 

https://duckduckgo.com/?t=canonical&q=ipfs+database&ia=web  
ipfs database at DuckDuckGo  

https://rossbulat.medium.com/orbitdb-deploying-the-distributed-ipfs-database-with-react-79afa1a7fabb  
OrbitDB: Deploying the Distributed IPFS Database in the Browser | by Ross Bulat | Medium  





## Installation: Clients / Servers

https://github.com/ipfs/js-ipfs/tree/master/examples/browser-vue  
js-ipfs/examples/browser-vue at master · ipfs/js-ipfs · GitHub  

https://ipfs.io/#install  
IPFS Powers the Distributed Web  

https://ipfs.io/#install  
IPFS Powers the Distributed Web  
https://js.ipfs.io/  
JS IPFS  

https://github.com/ipfs/js-ipfs/tree/master/examples#js-ipfs-examples-and-tutorials  
js-ipfs/examples at master · ipfs/js-ipfs · GitHub  
https://github.com/ipfs/js-ipfs/tree/master/examples/browser-webpack  
js-ipfs/examples/browser-webpack at master · ipfs/js-ipfs · GitHub  


### Docker

https://hub.docker.com/u/ipfs/  
ipfs's Profile  
https://blog.ipfs.io/1-run-ipfs-on-docker/  
Run IPFS in a Docker container  
https://duckduckgo.com/?t=canonical&q=ipfs+docker&ia=web  
ipfs docker at DuckDuckGo  


### Desktop GUI Client

https://github.com/ipfs-shipyard/ipfs-desktop  
GitHub - ipfs-shipyard/ipfs-desktop: An unobtrusive and user-friendly desktop application for IPFS on Windows, Mac and Linux.  

#### AppImage

https://appimage.org/

Download an application, make it executable, and run! No need to install. No system libraries or system preferences are altered. Can also run in a sandbox like Firejail

```
cd ~/Downloads
chmod +x ipfs-desktop-0.14.0-linux-x86_64.AppImage
./ipfs-desktop-0.14.0-linux-x86_64.AppImage
```

#### Snap

https://docs.ipfs.io/install/command-line/#package-managers

```
snap install ipfs
```

## Private / Local Only

For a private network, all nodes must use the same key.

https://duckduckgo.com/?t=canonical&q=ipfs+local+only&ia=web  
ipfs local only at DuckDuckGo  
https://medium.com/@s_van_laar/deploy-a-private-ipfs-network-on-ubuntu-in-5-steps-5aad95f7261b  
Deploy a private IPFS network in 5 steps | by Sander van Laar | Medium  


### Deployment

https://specs.ipld.io/data-structures/hashmap.html#introduction  
Specification: HashMap | IPLD Specifications  
https://specs.ipld.io/block-layer/graphsync/graphsync.html  
Graphsync | IPLD Specifications  




