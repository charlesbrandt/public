# Configuration

For projects, it helps to abstract out variables to designate where resources can be found based on the environment. The most common example is how to access the API.

in src/config.js

``` js
const exports = {
  mode: "development",
  api: "https://localhost:3333/api",
  defaultRedirect: "/signin",
  analyticsId: "G-FOO",

  // mode: "test",
  // api: "http://phi_mdd.local/api/",
  // defaultRedirect: "/signin",
  // analyticsId: "G-FOO",
};

export default exports;
```
