# CSV

CSV is good for read and writing tabular data. 

Decide if you want to parse each row into:

  - a dictionary with keys based on the headers
  - an array 
  
Both have their use cases, but if you need to access the data via keys, let the parser handle that for you. 

## Dictionary

The one to use with most data

```py
import csv

with open(source, mode='r') as csv_file:
    csv_reader = csv.DictReader(csv_file)
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f'Header: {", ".join(row)}')
        else:
            print(f'\t{row["name"]}, {row["description"]}')
        line_count += 1
    print(f'Processed {line_count} lines.')

```


## Array

```py
import csv

with open('source') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f'Header: {", ".join(row)}')
        else:
            print(f'\t{row[0]}, {row[1]}')
        line_count += 1
    print(f'Processed {line_count} lines.')
```


Via  
https://realpython.com/python-csv/


## Writing

```py
with open('output_file.csv', mode='w') as csv_output:
    csv_writer = csv.writer(csv_output, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

    for row in results:
        csv_writer.writerow(row)

```


[Javascript CSV](/code/javascript/csv.md)


## More than 1024 columns

I often take an excel file supplied by someone that needs additional processing, open it with Libre Calc, save it as a CSV, then process from there. 

However, just ran into a case where the number of columns in the source excel spreadsheet exceeds 1024.  There is a hard limit in Libre Calc for the number of columns to 1024.  It doesn't look like this will be changed any time soon:  
https://bugs.freedesktop.org/show_bug.cgi?id=50916  

Keep the data in the native Excel format and use a dedicated library to read it. Back in 2014, this one worked well.

https://pypi.python.org/pypi/openpyxl  
http://pythonhosted.org//openpyxl/  
https://bitbucket.org/ericgazoni/openpyxl/src  


### Other attempts

http://www.python-excel.org/ 

Another library to keep data in the excel format and read and write directly.   

http://www.simplistix.co.uk/presentations/python-excel.pdf  
https://github.com/python-excel/xlwt  
https://secure.simplistix.co.uk/svn/xlrd/trunk/xlrd/doc/xlrd.html?p=4966  
https://github.com/python-excel/xlrd  
https://secure.simplistix.co.uk/svn/xlwt/trunk/xlwt/doc/xlwt.html?p=4966  

Except when I tried to write back to an excel file, xlwt has an even lower limit of 256 columns:  
https://www.google.com/search?q=ValueError%3A+column+index+(256)+not+an+int+in+range(256)&oq=ValueError%3A+column+index+(256)+not+an+int+in+range(256)&aqs=chrome..69i57j69i58.940j0j1&sourceid=chrome&espv=210&es_sm=91&ie=UTF-8  
http://stackoverflow.com/questions/7658513/python-xlwt-more-than-256-columns  

https://www.google.com/search?q=more+than+1024+columns+libre+office+calc&oq=more+than+1024+columns+libre+office+calc&aqs=chrome..69i57.10296j0j1&sourceid=chrome&espv=210&es_sm=91&ie=UTF-8

