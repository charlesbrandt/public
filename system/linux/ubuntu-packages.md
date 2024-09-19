# Ubuntu Packages

Based on Debian, so leverages the `apt-get` or just `apt` package manager:

Sometimes while updating the list of avialable packages, the upstream sources become unavailable. For example:

```
sudo apt-get update
```

Receive a message like

```
N: Skipping acquire of configured file 'main/binary-i386/Packages' as repository 'https://apt.postgresql.org/pub/repos/apt jammy-pgdg InRelease' doesn't support architecture 'i386'
```

Edit the corresponding list file to update the source. Something like:

```
sudo -H micro /etc/apt/sources.list.d/pgdg.list
```

Then this should work

```
sudo apt-get update
```

via:
https://stackoverflow.com/questions/61523447/skipping-acquire-of-configured-file-main-binary-i386-packages
