# Python OS Modules

## Remove a file

https://stackoverflow.com/questions/42636018/python-difference-between-os-remove-and-os-unlink-and-which-one-to-use

With `pathlib` it's been consolidated to `.unlink()`, so sticking with that as the preferred nomenclature. 

https://docs.python.org/3/library/pathlib.html

```
from pathlib import Path

p = Path('.')
```
