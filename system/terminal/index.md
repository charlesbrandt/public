# Terminal

```
cd
rm .bashrc
ln -s ~/public/system/linux/.bashrc .bashrc
```


## Alias vs Functions

interesting discussion came up here:  
~/public/system/virtualization/docker.md

Which is better, an alias or a function? 

#### Function

```
dew() {
  docker-compose exec web $@
}
```

The $@ part handles passing along all of the function’s arguments. 

https://duckduckgo.com/?q=docker-compose+shortcut&t=ffab&ia=web
docker-compose shortcut at DuckDuckGo
https://stackoverflow.com/questions/40429508/shortcut-command-for-docker-compose-build-down-up
Shortcut command for docker-compose build + down + up - Stack Overflow
https://nickjanetakis.com/blog/docker-tip-26-alias-and-function-shortcuts-for-common-commands
Docker Tip #26: Alias and Function Shortcuts for Common Commands — Nick Janetakis

## See also

~/public/system/.bashrc
