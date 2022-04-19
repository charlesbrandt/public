# Typescript

Allows type checking in Javascript so your editor can give you a heads up if something isn't being passed as expected. 

Don't need to go overboard with this.

If a `const` or a `let` is enough to convey the variable, just use that. 

## Configuration

The `tsconfig.json` determines how typescript will behave. What checks are enforced and what are skipped. When just getting started, you may want to allow javascript files to co-exist and not flag an error. 

Under the compilerOptions object-property, add:

```
"allowJs": true
```

https://stackoverflow.com/questions/70344209/virtual-script-not-found-may-missing-script-lang-ts-allowjs-true-jsc
typescript - Virtual script not found, may missing `<script lang="ts">` / "allowJs": true / jsconfig.json.volar - Stack Overflow


https://duckduckgo.com/?t=ffab&q=vitesse+disable+typescript+requirement&ia=web
vitesse disable typescript requirement at DuckDuckGo

This is the actual message that shows up in VSCode
https://duckduckgo.com/?t=ffab&q=Virtual+script+not+found%2C+may+missing+%3Cscript+lang%3D%22ts%22%3E+%2F+%22allowJs%22%3A+true+%2F+jsconfig.json&ia=web
Virtual script not found, may missing `<script lang="ts">` / "allowJs": true / jsconfig.json at DuckDuckGo
