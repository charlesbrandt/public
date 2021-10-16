# Code Conventions

It's a good idea to give some thought to what you name your objects and services on the API. Variables, methods and classes also have their own conventions. These conventions vary from ecosystem to ecosystem and team to team. 

If you join a team or project and conventions are already in place, use them. 


## Singular vs Plural

Be consistent with the route for accessing a service.

Everything is easier when singular/plural forms are consistent. 

Should that apply at the UI route level too? I'd say yes. 

This is a great discussion on the topic. 
https://stackoverflow.com/questions/6845772/rest-uri-convention-singular-or-plural-name-of-resource-while-creating-it

I *prefer* singular for is non-ambiguity. Going with that.

In my experience plural only complicates naming, but I get the appeal. 


## Choose a unique name

Ideally the name should be a unique term in the code base. It will appear enough as it is once it gets started. 

Before settling on a name, do a search in the code base for the potential term. Do any other matches show up? Are those matches related to the domain of the term you want to use? If so, that's good. If not, it may be a good sign to use a different term. 

Ideally a service name will not share a common prefix with other services

Ideally a single word will work. In some cases it is necessary to use two words. The two word case opens the door for deciding what case strategy will be used. (eg. kebab, camel, snake). I feel that it may be best to just smoosh all the words together to form the new term. No spaces, no dashes, no lowlines. 

### Names to avoid

 "product" -- term comes up often in cases like "production", shares a common prefix with "project"
 "data", "file" -- these are both too generic. I do think it's okay to combine them though to "datafile"
 "template", "script", "style" -- these are used in the front-end framework Vue. It will be very difficult to use search to find related files. 


## Case

camelCase is used heavily in Javascript

snake_case is another useful alternative, especially in databases and filesystems where case can be problematic. Works fine in HTML templates without flipping to kebab case

kebab-case is good in urls, filenames and as ids (not good for variable names in code. `-` gets interpreted as a minus)

Some good reading on the topic:

https://blog.lmorchard.com/2013/01/23/naming-conventions/
Naming Things: CamelCase vs snake_case - blog.lmorchard.com
http://xahlee.info/comp/camelCase_vs_snake_case.html
camelCase vs snake_case


Seems like camelCase is preferred over names_with_underscores. Variable names-with-hyphens are not allowed in python or javascript. 

https://www.w3schools.com/js/js_conventions.asp



And for a laugh:

https://www.youtube.com/watch?v=LH5dOVqqCc8
Coding Style: CamelCase vs. snake_case - YouTube

## Falsehoods

Looks like some good reading here

https://github.com/kdeldycke/awesome-falsehood
kdeldycke/awesome-falsehood: 😱 Falsehoods Programmers Believe in

## Linting

Conventions get codified in [linting](linting.md) config files. 

## Style Guides

Style guides often cover more than just naming conventions, but they're a related topic. 

Australia has a great developer's style guide on naming conventions:

https://api.gov.au/standards/national_api_standards/naming-conventions.html

## See Also

[Vue Style Guide](./vue/index.md#style-guide-naming-conventions)

## Best Practices and Conventions

https://www.thinkful.com/learn/javascript-best-practices-1/

https://blog.risingstack.com/javascript-clean-coding-best-practices-node-js-at-scale/

