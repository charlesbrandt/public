# Image Gallery

Serving images and keeping track of image metadata is an important aspect to most application APIs.

Binary data is not a good fit for a version control system. Binary assets typically don't change as often as an application's code. When changes do happen, the differences between files can be greater.

So a different type of system can help.

A lot of overlap here with databases and persistence / storage stacks. May even be able to use distributed web technologies like IPFS.

https://duckduckgo.com/?t=canonical&q=node+image+gallery&ia=web
node image gallery at DuckDuckGo
https://node.qodeinteractive.com/image-gallery/
Image Gallery – Node
https://github.com/cianclarke/node-gallery
GitHub - cianclarke/node-gallery: NodeJS Photo Gallery

## Image sizes

Ideally, we only want to send clients the size of an image that will optimally fill the space on their display. Different devices have different resolutions, so that needs to be accounted for.

On the server, we don't want to keep lots of copies of different image resolutions. (e.g. `sizes/` directories)

We certainly want to keep the original. From there it may make sense to keep one more that would cover most large display scenarios. Usually these are much smaller than the original, and make a good starting point for generating even smaller versions dynamically.

https://duckduckgo.com/?t=canonical&q=ideal+image+size+for+image+hosting&ia=web
ideal image size for image hosting at DuckDuckGo
https://flothemes.com/flothemes-image-sizes/
Best Image Sizes and How to Save Images For the Web (2021)

## Image libraries

To handle generating different sizes, a library for your programming language will help with the task.

Wrapping `libvips` seems like a good approach, and in node there is `sharp` for this.

https://github.com/lovell/sharp
GitHub - lovell/sharp: High performance Node.js image processing, the fastest module to resize JPEG, PNG, WebP, AVIF and TIFF images. Uses the libvips library.

https://github.com/abdollahpour/micro-image-manager
GitHub - abdollahpour/micro-image-manager: Distributed, manage and optimise image for your microservices

https://github.com/weareoffsider/on-demand-resizer
GitHub - weareoffsider/on-demand-resizer: On Demand Image Resizing for Node Templating and Views
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
https://github.com/strapi/strapi
GitHub - strapi/strapi: 🚀 Open source Node.js Headless CMS to easily build customisable APIs
https://strapi.io/documentation/developer-docs/latest/content-api/api-endpoints.html#endpoints
API Endpoints | Strapi Developer Documentation
https://github.com/chakra-ui/chakra-ui
GitHub - chakra-ui/chakra-ui: ⚡️ Simple, Modular & Accessible UI Components for your React Applications
https://chakra-ui.com/
Chakra UI - A simple, modular and accessible component library that gives you the building blocks you need to build your React applications. - Chakra UI
https://duckduckgo.com/?t=canonical&q=node+js+image+server&ia=software
node js image server at DuckDuckGo
https://github.com/LoganHeinzelman/image-server
GitHub - LoganHeinzelman/image-server: NodeJS image server
https://stackoverflow.com/questions/5823722/how-to-serve-an-image-using-nodejs
node.js - How to serve an image using nodejs - Stack Overflow
https://duckduckgo.com/?t=canonical&q=node+image+server+resize&ia=web
node image server resize at DuckDuckGo
https://malcoded.com/posts/nodejs-image-resize-express-sharp/
Resizing Images in Node.js using Express & Sharp | malcoded
https://ahmadawais.com/resize-optimize-images-javascript-node/
Resize & Optimize Images With JavaScript in Node.js
https://duckduckgo.com/?t=canonical&q=node+image+library&ia=web
node image library at DuckDuckGo
https://stackoverflow.com/questions/10692075/which-library-should-i-use-for-server-side-image-manipulation-on-node-js
javascript - Which library should I use for server-side image manipulation on Node.JS? - Stack Overflow
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
