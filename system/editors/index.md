# Text Editor

A good text editor is important when writing code and editing system files. This is not the same as a word processor.

Different text editors have various strengths and weaknesses including usability, features, availability, and resource usage. It helps to try out different editors.

Being able to work with more than one editor is important. Depending on the resources that are available, you may not be able to run something as heavy as visual studio code.

Standardize on common keymaps for all editors (as much as possible). Standardizing makes it easier and more seamless to transition from one editor to another.

## micro

[Micro Editor](micro/index.md)

Not an easy name to search for, but the editor is quickly becoming a favorite. Fast to start, easy to use. 

## [VS Code](vs-code/vs-code.md)

VS Code is a great environment for editing code. 

My only complaint is it's very memory intensive. If you like to keep a lot of editors open at the same time, some of the lighter weight editors may be better options.

## [Emacs](emacs/emacs.md)

```
cd
rm .emacs
rm .emacs.d
ln -s ~/public/system/editors/emacs/.emacs .emacs
ln -s ~/public/system/editors/emacs/.emacs.d .emacs.d
```

## [vi](vi/) / nano

It's important to know how to edit something via ssh. `vi` and `nano` are both good for that.


## Performance Considerations

This is a good overview of how different editors compare, performance wise.

https://github.com/jhallen/joes-sandbox/tree/master/editor-perf  
joes-sandbox/editor-perf at master · jhallen/joes-sandbox  

Memory utilization adds up quickly if you keep a lot of buffers open. 

https://www.duckduckgo.com/search?q=open+source+text+editor+low+memory+usage  
open source text editor low memory usage - Google Search  
https://news.ycombinator.com/item?id=13933128  
Text Editor Performance Comparison | Hacker News  
https://news.ycombinator.com/from?site=github.com  
Submissions from github.com | Hacker News  
https://github.com/jhallen/joes-sandbox  
jhallen/joes-sandbox  
https://joe-editor.sourceforge.io/  
Home - Joe's Own Editor  



