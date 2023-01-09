# Selenium

A tried and true tool for creating end-to-end (e2e) tests. 

Useful for automated testing and data retrieval. 

## Installation

Get the WebDriver installed for the browser(s) you want to test with. 

IMPORTANT: If using the snap based version of chromium on linux, the chromedriver is included there. It's important to use that version, otherwise the chromedriver will have trouble accessing the isolated instance of the browser. 

In snap, the chrome driver is available `/snap/bin/chromium.chromedriver`

https://stackoverflow.com/a/61980562

Some of the steps I found necessary to get this working

```
vi ~/.profile 

PATH="/snap/bin:$PATH"

source ~/.profile 
```

### Chrome Driver

If not using the snap version of Chrome, you may need to download the chrome driver separately

https://chromedriver.chromium.org/getting-started

Check your version of Chrome (Menu -> About Chromium). Download the right version of the driver. 

https://chromedriver.chromium.org/downloads

Extract the driver

Help WebDriver find the downloaded ChromeDriver executable
Any of these steps should do the trick:

        include the ChromeDriver location in your PATH environment variable

```
env
mv ~/Downloads/chromedriver /usr/local/bin/
sudo chown account: /usr/local/bin/chromedriver
```


## Language Bindings

The selenium-webdriver has bindings for many languages. 

### Python

```
pip install selenium
```

via

https://selenium-python.readthedocs.io/installation.html#installing-python-bindings-for-selenium  
1. Installation — Selenium Python Bindings 2 documentation  

https://selenium-python.readthedocs.io/  
Selenium with Python — Selenium Python Bindings 2 documentation  


Try out the getting started script:

``` python
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By

driver = webdriver.Firefox()
driver.get("http://www.python.org")
assert "Python" in driver.title
elem = driver.find_element(By.NAME, "q")
elem.clear()
elem.send_keys("pycon")
elem.send_keys(Keys.RETURN)
assert "No results found." not in driver.page_source
driver.close()
```

https://selenium-python.readthedocs.io/getting-started.html  
2. Getting Started — Selenium Python Bindings 2 documentation  


Then run it

```
python get_data.py
```

If all goes well, a browser should pop up, go to the `python.org` site, and then close. 

If you want to interact with the browser before it closes, comment out `driver.close()` in the script and then run python interactively

```
python -i get_data.py
```

This is a good point to update the script to go to any login page so you can do that step manually. Automating logins is often difficult and opens up the risk of exposing credentials. 

From here create a `helpers.py` script that you can import and subsequently reload your module without needing to restart your active browser context (and consequently, re-login). 

> Note: this approach




### Node Javascript

https://www.npmjs.com/package/selenium-webdriver

```
cd ~/projects
mkdir selenium-shared
cd selenium-shared

yarn init
yarn add --dev selenium-webdriver
```

Now the tests can live locally or in your project directory. 



Create a test script (e.g. `init.js`) and add the following to try it out

```
const {Builder, By, Key, until} = require('selenium-webdriver');

(async function example() {
  let driver = await new Builder().forBrowser('chrome').build();
  try {
    await driver.get('http://www.google.com/ncr');
    await driver.findElement(By.name('q')).sendKeys('webdriver', Key.RETURN);
    await driver.wait(until.titleIs('webdriver - Google Search'), 1000);
  } finally {
    await driver.quit();
  }
})();
```

then run with 

```
node init.js
```


## Local Testing

Some settings may be necessary to handle "Connection Is Not Private"

http://blogs.stevelongchen.com/2020/05/14/selenium-how-to-disable-or-bypass-connection-is-not-private-for-chrome-and-firefox/


## UI Testing

One benefit over Cypress is that Selenium works when testing remote services. One downside when compared with Cypress is that Selenium does not offer tight integration with the node application. 

It may be possible to get some of the nice testing integration by using Selenium with Mocha and Chai. 

https://nehalist.io/selenium-tests-with-mocha-and-chai-in-javascript/
