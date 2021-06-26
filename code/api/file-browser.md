# File Browser

A good solution to quickly and easily make files available locally available via the network that the local server has access to. 

Includes local user accounts. 

## Installation

```
curl -fsSL https://raw.githubusercontent.com/filebrowser/get/master/get.sh | bash
filebrowser -r /path/to/your/files
```

https://filebrowser.org/installation

## Running

https://filebrowser.org/cli/filebrowser

address
-a
address to listen on

port
-p
port to listen on

filebrowser -r /path/to/your/files -a 192.168.0.100 -p 9999

## Credentials

Tracking credentials in keepass -> Filebrowser entry

By default the service starts with the following.

   - Username: admin
   - Password: admin

Change this!!

May be possible to script the setup as needed:

https://filebrowser.org/cli/filebrowser-users

`filebrowser users`

https://filebrowser.org/configuration/authentication-method

It is also possible to disable authentication with

    filebrowser config set --auth.method=noauth


## TODO 

set to start up at boot? 

## Customizing / Development

How do I integrate content in File Browser with my own site / application? 

The current UI client should be considered as documentation for using the server's API in your own application. 

If you're using vue, you may be able to re-use large chunks of the existing UI. No need to set up a special server if you've already got your own UI dev environment going. 

Don't have one of those? Try web-ui-api-db

Probably only need the UI to get going! 

https://github.com/filebrowser/filebrowser/tree/master/frontend

### Dev Setup

Making changes requires both a go server and node environment to be configured. I had difficulty getting the frontend code to talk to another running instance of filebrowser. It may be possible, but it seems easier to run both pieces together. 

This is the main guide on setting up a development instance

https://filebrowser.org/contributing
Contributing - File Browser

It's a bit out of date
TODO: contribute an update / pull request. 

I had difficulty getting the go server to build with golang 1.14, which is what is available via ubuntu apt-get (20.10). When running `go build`, I received the following

```
go build
frontend/assets.go:5:8: package embed is not in GOROOT (/usr/lib/go-1.14/src/embed)
```

Downloading and installing directly from the golang site fixed that issue:

https://golang.org/dl/
Downloads - The Go Programming Language

```
wget https://golang.org/dl/go1.16.5.linux-arm64.tar.gz
sudo tar -C /usr/local -xzf go1.16.5.linux-arm64.tar.gz
```

Then add the path to the go binary to your shell path

```
vi ~/.bashrc



source ~/.bashrc
```

Be sure to generate the frontend bundle with either `yarn run build` or `yarn run watch` (for development).

```
cd frontend
yarn run build
```


Go rice is no longer required:

https://github.com/filebrowser/filebrowser/issues/1394
Rice.Go error when trying to setup local environment for development · Issue #1394 · filebrowser/filebrowser · GitHub

Run filebrowser in development mode

```
go run -tags dev main.go
```

You can pass all of the same command line interface parameters as you would with the prebuilt version. 




