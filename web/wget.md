# wget

`wget` is a good tool for simple scraping scenarios.

`wget` requires no extra options to download a remote URL to a local file. (curl requires -o or -O)

super simple version:

```
wget -rk -l 1 [sitename]
```

via:
https://unix.stackexchange.com/questions/96208/download-one-html-page-with-all-needed-graphics-and-linked-pdf-zip-files


It is similar to `curl`, but `curl` is better suited for testing web connections.

## wget vs curl

The main differences are:

   - wget's major strong side compared to curl is its ability to download recursively.
   - wget is command line only. There's no lib or anything, but curl's features are powered by libcurl.
   - curl supports FTP, FTPS, HTTP, HTTPS, SCP, SFTP, TFTP, TELNET, DICT, LDAP, LDAPS, FILE, POP3, IMAP, SMTP, RTMP and RTSP. wget supports HTTP, HTTPS and FTP.
   - curl builds and runs on more platforms than wget.
   - wget is released under a free software copyleft license (the GNU GPL). curl is released under a free software permissive license (a MIT derivate).
   - curl offers upload and sending capabilities. wget only offers plain HTTP POST support.

[via](https://unix.stackexchange.com/questions/47434/what-is-the-difference-between-curl-and-wget)

https://daniel.haxx.se/docs/curl-vs-wget.html

## Install (from source)

If you OS doesn't come with `wget`, or have a package manager that provides `wget` (e.g. `apt-get install wget`), installing from source is also easy to do. 

Grab the wget source code from http://ftp.gnu.org/pub/gnu/wget/wget-1.9.1.tar.gz (or get the latest here) . Open a terminal window and follow along:

```
tar xzf wget-1.9.1.tar.gz
cd wget-1.9.1
./configure
make
sudo make install
```

The wget binary should now be in your `/usr/local/bin` folder. Let’s also put `/usr/local/bin` in the shell path so we can type `wget` from the command line rather than the tedious `/usr/local/bin/wget` when we want to run the program. This is accomplished by editing `~/.bash_profile` to add the following line:

```
PATH=$PATH:/usr/local/bin; export PATH
```

To get the shell to re-read this file again, type `source ~/.bash_profile`. Now whenever you start the terminal, `/usr/local/bin` will be in the path.

from:
http://74.125.95.132/search?q=cache:4Q78XCZm29oJ:www.asitis.org/installing-wget-for-mac-os-x+wget+leopard&hl=en&ct=clnk&cd=1&gl=us

originally:
http://www.asitis.org/installing-wget-for-mac-os-x

## See also

[Troubleshooting](troubleshooting.md)


