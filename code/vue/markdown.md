# Markdown in Vue

Using markdown is always a good way to write documentation and content. 

When it comes time to display the content in a system, it helps to have a markdown component. 

## markdown-it vue

https://github.com/ravenq/markdown-it-vue

A solid foundation for markdown parsing in Vue based applications. 

https://github.com/ravenq/markdown-it-vue  
ravenq/markdown-it-vue: The vue lib for markdown-it.  

https://github.com/JanGuillermo/vue3-markdown-it  
GitHub - JanGuillermo/vue3-markdown-it: An awesome Vue 3 markdown-it wrapper plugin that can even support external plugins!  


Based on 

https://github.com/markdown-it/markdown-it  
markdown-it/markdown-it: Markdown parser, done right. 100% CommonMark support, extensions, syntax plugins & high speed  


### Styling

Where to apply styling to content?   
At the component level?   
In this case, CSS can help a lot (don't need to use something class based like tailwind, unless it's via an @apply specification)  


### Tables / Extensions / Plugins

It looks like tables are enabled in the basic version

A more complex version? (TODO: verify)

|             |          Grouping           ||
First Header  | Second Header | Third Header |
 ------------ | :-----------: | -----------: |
Content       |          *Long Cell*        ||
Content       |   **Cell**    |         Cell |

New section   |     More      |         Data |
And more      | With an escaped '\|'         ||
[Prototype table]


https://fletcher.github.io/MultiMarkdown-6/syntax/tables.html

https://www.npmjs.com/package/markdown-it-multimd-table  
markdown-it-multimd-table - npm  
https://spec.commonmark.org/  
CommonMark Spec  
https://spec.commonmark.org/0.30/  
CommonMark Spec  

https://www.npmjs.com/search?q=keywords:markdown-it-plugin  
keywords:markdown-it-plugin - npm search  


https://duckduckgo.com/?t=ffab&q=mark+down+it+vue&ia=web  
mark down it vue at DuckDuckGo  


