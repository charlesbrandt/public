# Image Gallery

Serving images and keeping track of image metadata is an important aspect to most application APIs.

Binary data is not a good fit for a version control system. Binary assets typically don't change as often as an application's code. When changes do happen, the differences between files can be greater.

So a different type of system can help.

A lot of overlap here with databases and persistence / storage stacks. May even be able to use distributed web technologies like IPFS.

Seems like this should be a solved problem. 

However, I think there is work being done
to simplify the process
of delivering the correct image for the most popular scenarios

this also provides some avenue for analytics (how are users viewing your content?)


## Image sizes

Ideally, we only want to send clients the size of an image that will optimally fill the space on their display. Different devices have different resolutions, so that needs to be accounted for.

On the server, we don't want to keep lots of copies of different image resolutions. (e.g. `sizes/` directories)

We certainly want to keep the original. From there it may make sense to keep one more that would cover most large display scenarios. Usually these are much smaller than the original, and make a good starting point for generating even smaller versions dynamically.

https://duckduckgo.com/?t=canonical&q=ideal+image+size+for+image+hosting&ia=web
ideal image size for image hosting at DuckDuckGo
https://flothemes.com/flothemes-image-sizes/
Best Image Sizes and How to Save Images For the Web (2021)


## On demand image servers

https://github.com/imgproxy/imgproxy
💤 GitHub - imgproxy/imgproxy: Fast and secure standalone server for resizing and converting remote images

https://www.sitepoint.com/improving-performance-perception-on-demand-image-resizing/
Improving Performance Perception: On-demand Image Resizing - SitePoint
https://www.sitepoint.com/how-to-build-responsive-images-with-srcset/
How to Build Responsive Images with srcset - SitePoint

In need of a system to help us initiate the sizing based on the requests. 

https://github.com/pinterest/PINRemoteImage
💤 GitHub - pinterest/PINRemoteImage: A thread safe, performant, feature rich image fetcher

https://github.com/h2non/imaginary
💤 GitHub - h2non/imaginary: Fast, simple, scalable, Docker-ready HTTP microservice for high-level image processing

This has a nice API example of what I would like, but it is written in Go. Maybe clone it using Node (feathers?) and Sharp

https://github.com/abdollahpour/micro-image-manager
GitHub - abdollahpour/micro-image-manager: Distributed, manage and optimise image for your microservices


https://duckduckgo.com/?t=canonical&q=node+js+image+server&ia=software
node js image server at DuckDuckGo
https://github.com/LoganHeinzelman/image-server
GitHub - LoganHeinzelman/image-server: NodeJS image server
https://stackoverflow.com/questions/5823722/how-to-serve-an-image-using-nodejs
node.js - How to serve an image using nodejs - Stack Overflow
https://duckduckgo.com/?t=canonical&q=node+image+server+resize&ia=web
node image server resize at DuckDuckGo

this one is even more of a work in progress (abandoned?)

https://github.com/weareoffsider/on-demand-resizer
GitHub - weareoffsider/on-demand-resizer: On Demand Image Resizing for Node Templating and Views


## Smart Cropping

A few different approaches that can be taken with cropping intelligently

Can take into account where the subject of interest is in the image
then work to crop towards that


https://github.com/jwagner/smartcrop.js
GitHub - jwagner/smartcrop.js: Content aware image cropping


There is also this crazy tool that manages to edit the image itself to compress or expand features in the picture -- automatically!

https://github.com/esimov/caire
💤 GitHub - esimov/caire: Content aware image resize library


## Display Images

Client side -- decide what needs to be displayed, at what size, and when
then push the handling off to the server

https://github.com/strues/retinajs
💤 GitHub - strues/retinajs: JavaScript, SCSS, Sass, Less, and Stylus helpers for rendering high-resolution image variants


### Lazy loading

An equally important part is on the client, only loading those images that are actually visible. A page may have a lot of content on it. No need to load it unless it's visible

https://github.com/callmecavs/layzr.js
GitHub - callmecavs/layzr.js: A modern lazy loading library for images.


Not sure if any of these vue specific ones are necessary? 

https://duckduckgo.com/?t=ffab&q=vite+lazy+load+images&ia=web
vite lazy load images at DuckDuckGo

vue3
https://github.com/jambonn/vue-lazyload
GitHub - jambonn/vue-lazyload: Vue module for lazy-loading images in your vue 3 applications.

vue2
https://github.com/hilongjw/vue-lazyload
GitHub - hilongjw/vue-lazyload: A Vue.js plugin for lazyload your Image or Component in your application.


## Image managers and Galleries

Once you have a mechanism to handle resizing on demand

This brings to mind the idea of a gallery or meta data database for images

much that can be done in this space in terms of automating content classification and identification

https://github.com/opencv/opencv
GitHub - opencv/opencv: Open Source Computer Vision Library

https://github.com/JaidedAI/EasyOCR
GitHub - JaidedAI/EasyOCR: Ready-to-use OCR with 80+ supported languages and all popular writing scripts including Latin, Chinese, Arabic, Devanagari, Cyrillic and etc.


Sometimes it is useful to rasterize a vector graphic for display
could be the case if a thumbnail is smaller than a complex svg

https://github.com/axe312ger/sqip
GitHub - axe312ger/sqip: "SQIP" (pronounced \skwɪb\ like the non-magical folk of magical descent) is a SVG-based LQIP technique.


https://github.com/fengyuanchen/viewerjs
GitHub - fengyuanchen/viewerjs: JavaScript image viewer.

https://github.com/pqina/filepond
💤 GitHub - pqina/filepond: 🌊 A flexible and fun JavaScript file upload library
https://github.com/fengyuanchen/cropperjs
💤 GitHub - fengyuanchen/cropperjs: JavaScript image cropper.
https://github.com/alexjc/neural-doodle
💤 GitHub - alexjc/neural-doodle: Turn your two-bit doodles into fine artworks with deep neural networks, generate seamless textures from photos, transfer style from one image to another, perform example-based upscaling, but wait... there's more! (An implementation of Semantic Style Transfer.)

https://github.com/nhn/tui.image-editor
💤 GitHub - nhn/tui.image-editor: 🍞🎨 Full-featured photo image editor using canvas. It is really easy, and it comes with great filters.

## Decentralized Photo Sharing

https://github.com/pixelfed/pixelfed
GitHub - pixelfed/pixelfed: Photo Sharing. For Everyone.
https://pixelfed.org/
Pixelfed - Federated Image Sharing
https://docs.pixelfed.org/running-pixelfed/
Run your own Pixelfed website | Pixelfed Documentation
https://fedidb.org/software/pixelfed
FediDB - Developer Tools for ActivityPub
https://fedidb.org/network/instance?domain=pixelfed.anartist.org
FediDB - Developer Tools for ActivityPub
https://fediverse.party/en/fediverse/
About Fediverse - Fediverse.Party - explore federated networks
https://medium.com/we-distribute/a-quick-guide-to-the-free-network-c069309f334
A quick guide to The Free Network | by Sean Tilley | We Distribute | Medium
https://socialhome.network/
Socialhome HQ - Socialhome
https://git.feneas.org/socialhome/socialhome/-/issues
Issues · Socialhome / socialhome · GitLab
https://git.feneas.org/socialhome/socialhome
Socialhome / socialhome · GitLab
https://github.com/jaywink/socialhome
GitHub - jaywink/socialhome: A federated social home
https://github.com/topics/federation
federation · GitHub Topics · GitHub
https://github.com/topics/fediverse
fediverse · GitHub Topics · GitHub
https://github.com/topics/diaspora
diaspora · GitHub Topics · GitHub
https://github.com/topics/federated-social
federated-social · GitHub Topics · GitHub
https://github.com/LemmyNet/lemmy
GitHub - LemmyNet/lemmy: 🐀 Building a federated alternative to reddit in rust
https://pixelfed.anartist.org/
Anartist Pixelfed


## Node based image galleries

(See also solutions noted previously)

https://duckduckgo.com/?t=canonical&q=node+image+gallery&ia=web
node image gallery at DuckDuckGo
https://node.qodeinteractive.com/image-gallery/
Image Gallery – Node
https://github.com/cianclarke/node-gallery
GitHub - cianclarke/node-gallery: NodeJS Photo Gallery

https://duckduckgo.com/?q=node+image+server&t=canonical&ia=images
node image server at DuckDuckGo
https://duckduckgo.com/?t=canonical&q=node+image+manager&ia=web
node image manager at DuckDuckGo
https://froala.com/wysiwyg-editor/docs/sdks/nodejs/image-manager/
Node.JS Image Manager - Froala
https://duckduckgo.com/?t=canonical&q=node+image+uploader&ia=images
node image uploader at DuckDuckGo
https://sabre.io/dav/
sabre/dav - sabre/dav
https://github.com/OpenMarshal/npm-WebDAV-Server
GitHub - OpenMarshal/npm-WebDAV-Server: WebDAV Server for npm


## Research


https://github.com/topics/image-processing
image-processing · GitHub Topics · GitHub

https://github.com/topics/image
💤 image · GitHub Topics · GitHub


https://github.com/python-pillow/Pillow
💤 GitHub - python-pillow/Pillow: The friendly PIL fork (Python Imaging Library)
https://github.com/topics/image-manager
💤 image-manager · GitHub Topics · GitHub
https://github.com/jonathanong/ims
💤 GitHub - jonathanong/ims: A basic Image Management System for managing your assets

https://duckduckgo.com/?t=ffab&q=on+demand+image+resizing+&ia=web
on demand image resizing at DuckDuckGo
http://sizr.io/
Sizr.io


## Image libraries

To handle generating different sizes, a library for your programming language will help with the task.

Wrapping `libvips` seems like a good approach, and in node there is `sharp` for this.

https://github.com/lovell/sharp
GitHub - lovell/sharp: High performance Node.js image processing, the fastest module to resize JPEG, PNG, WebP, AVIF and TIFF images. Uses the libvips library.

### Sharp

Sharp is the one to use in Node

https://github.com/lovell/sharp
GitHub - lovell/sharp: High performance Node.js image processing, the fastest module to resize JPEG, PNG, WebP, AVIF and TIFF images. Uses the libvips library.

https://malcoded.com/posts/nodejs-image-resize-express-sharp/
Resizing Images in Node.js using Express & Sharp | malcoded

Many projects utilize it!!

https://github.com/lovell/sharp/network/dependents?dependent_type=PACKAGE&dependents_after=MTEwODM5NTMyODA
Network Dependents · lovell/sharp · GitHub
https://github.com/ppvg/svelte-picture-source
GitHub - ppvg/svelte-picture-source: Svelte picture <source> image optimization preprocessor
https://github.com/gigasource/file-explorer-backend
GitHub - gigasource/file-explorer-backend
https://github.com/mariowix/imageCropper
GitHub - mariowix/imageCropper: Automated tool for reduce image to the most small image possible
https://github.com/paroi-tech/media-engine
GitHub - paroi-tech/media-engine: A backend library to upload and store files in SQLite, then to serve them.
https://github.com/MarcusCemes/image-processing-pipeline
GitHub - MarcusCemes/image-processing-pipeline: An image build orchestrator for the modern web
https://github.com/juliomrqz/nuxt-optimized-images
GitHub - juliomrqz/nuxt-optimized-images: 🌅🚀 Automatically optimizes images used in Nuxt.js projects (JPEG, PNG, SVG, WebP and GIF).


https://github.com/lovell/sharp/network/dependents
💤 Network Dependents · lovell/sharp · GitHub
https://github.com/lovell/sharp/network/dependents?dependent_type=PACKAGE&dependents_after=MTEwOTIwNTY0MDk
Network Dependents · lovell/sharp · GitHub


https://ahmadawais.com/resize-optimize-images-javascript-node/
Resize & Optimize Images With JavaScript in Node.js
https://duckduckgo.com/?t=canonical&q=node+image+library&ia=web
node image library at DuckDuckGo
https://stackoverflow.com/questions/10692075/which-library-should-i-use-for-server-side-image-manipulation-on-node-js
javascript - Which library should I use for server-side image manipulation on Node.JS? - Stack Overflow




## Caching

smart caching should happen at this layer
do not store different image sizes on disk like before
(unless you keep one extra besides full size, for thumbs / default ?)

see discussion here:
https://github.com/imgproxy/imgproxy

caching may not be required here... 
may want it at a different layer of the web application when traffic scales to needing it






