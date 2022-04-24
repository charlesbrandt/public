# CSV

## Papa Parse

Nice documentation (fun to read! :) ) 

https://www.papaparse.com/  
Papa Parse - Powerful CSV Parser for JavaScript  

https://www.papaparse.com/docs  
Documentation - Papa Parse  
  
https://github.com/mholt/PapaParse  
mholt/PapaParse: Fast and powerful CSV (delimited text) parser that gracefully handles large files and malformed input  

### Setup

```
yarn add papaparse
```

Then use in a component with:

```
  // parse the csv file into options
  Papa.parse(event.target.files[0], {
    dynamicTyping: true,
    // worker: true,
    header: true,
    transformHeader: undefined,
    complete: (parsed) => {
      console.log("Parsing complete!", parsed)
      options.value = parsed.data
      parsed.data.forEach((row, index) => {
        console.log("Current row:", index, row);
      })
    },
    error: (result) => {
      console.log("CSV Parsing encountered an error:", result)
    }
  });
```

See below for a complete example of a vue component.

With `header: true` you get a list of json objects with keys that use the corresponding columns header value. Great! Good to go!


## Configuration Options

https://www.papaparse.com/faq#workers

`worker` 	Whether or not to use a worker thread. Using a worker will keep your page reactive, but may be slightly slower. 

`dynamicTyping` 	If true, numeric and boolean data will be converted to their type instead of remaining strings. 

Useful settings different than the defaults:

```js
{
	dynamicTyping: true,
	header: true,
	transformHeader: undefined,
	worker: true,
	complete: undefined,
	error: undefined,
}
```

This looks like another good option to explore if worker doesn't pan out:

	step: undefined,

Allow comments in source files -- cool!

	comments: false,

Don't load the whole CSV, only get the first few rows:

	preview: 0,

https://www.papaparse.com/docs#config

Default Config With All Options

```js
{
	delimiter: "",	// auto-detect
	newline: "",	// auto-detect
	quoteChar: '"',
	escapeChar: '"',
	header: false,
	transformHeader: undefined,
	dynamicTyping: false,
	preview: 0,
	encoding: "",
	worker: false,
	comments: false,
	step: undefined,
	complete: undefined,
	error: undefined,
	download: false,
	downloadRequestHeaders: undefined,
	downloadRequestBody: undefined,
	skipEmptyLines: false,
	chunk: undefined,
	chunkSize: undefined,
	fastMode: undefined,
	beforeFirstChunk: undefined,
	withCredentials: undefined,
	transform: undefined,
	delimitersToGuess: [',', '\t', '|', ';', Papa.RECORD_SEP, Papa.UNIT_SEP]
}
```



## Parsing tips / Anti-patterns

### Client side error checking

With larger CSV files, per row API checks can bog the UI down. A worker may help, but shouldn't need to do this. 

Don't make calls out to the API to check every row of the file on the client side. 

Don't worry about checking for duplicates on the client side. 

Build the Objects the way you want to use them, then pass that back to the API for validation.


### Manually mapping

```js
      // parse the csv file into this.results
      Papa.parse(this.files[0], {
        complete: (parsed) => {
          parsed.data.forEach((row, i) => {
            const local_value = row[2]
            const different_value = row[3]
            // ... etc

            console.log("Current row:", i, data_filename, row);
```

This requires counting columns, is time consuming, and error prone. 

Modern CSV parsing libraries can parse headers. Use those to map as needed. 


## CSV Input Component

```js
<template>
  <div>
    <div>
      <p>Upload a csv file with metadata</p>
      <form class="pt-4" @submit="onSubmit" @reset="onReset">
        <input type="file" @change="parseFiles" ref="files" id="file" multiple />
      </form>
    </div>
    <div v-if="options">
      <h3>Matched results</h3>
      <div v-for="(row, index) in options" :key="row.label" class="pb-4">
        {{ row }}
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref } from "vue";
import Papa from "papaparse";

let files = ref(null)
let options = ref(null)

const parseFiles = (event) => {
  // these point to the same object
  console.log("Event files", event.target.files[0])
  console.log("File files", files.value.files);

  // parse the csv file into options
  Papa.parse(event.target.files[0], {
    dynamicTyping: true,
    // worker: true,
    header: true,
    transformHeader: undefined,
    complete: (parsed) => {
      console.log("Parsing complete!", parsed)
      options.value = parsed.data
      parsed.data.forEach((row, index) => {
        console.log("Current row:", index, row);
      })
    },
    error: (result) => {
      console.log("CSV Parsing encountered an error:", result)
    }
  });
}

const onSubmit = (event) => {
  event.preventDefault();
  console.log("onSubmit function called");
}

const onReset = () => {
  console.log('Resetting')
}
</script>


```


### Cypress Testing Template

[Upload a file via Cypress](/code/test/cypress.html#file-uploads)

```js
Cypress.Commands.add(
  "uploadFile",
  (fileNamePath, fileName, fileType = " ", selector) => {
    cy.get(selector).then((subject) => {
      cy.fixture(fileNamePath, "base64")
        .then(Cypress.Blob.base64StringToBlob)
        .then((blob) => {
          const el = subject[0];
          const testFile = new File([blob], fileName, {
            type: fileType,
          });
          const dataTransfer = new DataTransfer();
          dataTransfer.items.add(testFile);
          el.files = dataTransfer.files;
        });
    });
  }
);

describe("Import data", () => {
  beforeEach(() => {
    cy.viewport(1000, 800);
    cy.request(Cypress.env("API_URL") + "/verify-test?user=boilerplate-user").then(
      (response) => {
        console.log("The API verify-test response: ", response);
        localStorage.setItem("uid", response.body.uid);
        localStorage.setItem("roles", response.body.roles);
        localStorage.setItem("role", response.body.role);
        localStorage.setItem("jwt", response.body.jwt);
        localStorage.setItem("jwt_exp", response.body.jwt_exp);
        return response;
      }
    );
  });

  it("has a form to choose a source file", () => {
    cy.visit("/import-raw");
    cy.uploadFile(
      // stored in `ui/test/fixtures/`
      "boilerplate-test-file.csv",
      "boilerplate-test-file.csv",
      "text/csv",
      "#file"
    );
    // seems like the uploadFile should trigger this...
    cy.get("#file").trigger("change"); 
  });

});

```
## See also

https://www.papaparse.com/docs#config  
Documentation - Papa Parse  
https://www.papaparse.com/docs#results  
Documentation - Papa Parse  
https://www.papaparse.com/faq#workers  
FAQ - Papa Parse  
https://www.papaparse.com/docs#configurable  
Documentation - Papa Parse  


## Other Options

Many other interesting libraries available, but I didn't need to look any further. 

https://duckduckgo.com/?t=ffab&q=javascript+csv+parse&ia=web  
javascript csv parse at DuckDuckGo  
https://csv.js.org/parse/  
CSV Parse - Usage  
https://github.com/adaltas/node-csv-parse  
adaltas/node-csv-parse: CSV parsing implementing the Node.js `stream.Transform` API  
https://github.com/adaltas/node-csv  
adaltas/node-csv: Full featured CSV parser with simple api and tested against large datasets.  
https://github.com/topics/csv  
csv · GitHub Topics  
https://github.com/SheetJS/sheetjs  
SheetJS/sheetjs: SheetJS Community Edition -- Spreadsheet Data Toolkit  

Don't want to parse csv's manually -- too many edge conditions to consider. 

https://stackoverflow.com/questions/1293147/example-javascript-code-to-parse-csv-data  
Example JavaScript code to parse CSV data - Stack Overflow  


