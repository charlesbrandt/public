# Shell

AKA prompt command-line command-line-interface cli bash terminal console linux-shell zsh 

## Bash

https://www.gnu.org/software/bash/manual/bash.html
Bash Reference Manual

### Variables

How to assign a variable? 

```
export item="something"

echo $item
```

### Dates

How to print a formatted date in a command?

now="$(date +'%Y%m%d')"

$(date +%F)

### List contents of a directory sorted by modified timestamp

    ls -lt

### Sequential Commands

aka a list of commands. 
Separate each command by `&&`

https://www.gnu.org/software/bash/manual/bash.html#Lists

### Current Directory

search terms used: "bash name of current directory"

    printf '%s\n' "${PWD##*/}"

[via](https://stackoverflow.com/questions/1371261/get-current-directory-name-without-full-path-in-a-bash-script)





