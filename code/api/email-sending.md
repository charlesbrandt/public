# Email

Most projects need to send email.

## Client / UI side

Starts off with a web form to submit the message to the the API. 

### Gotchas

When setting up a system for sending email, it can be tricky to know why the message didn't make it through. 

If your form won't trigger the javascript actions to submit on firefox (but works on chrome) be sure to use the right button type:

If you have `<button type="submit">` that triggers that axios request, change the button to `<button type="button">`

https://stackoverflow.com/questions/61522494/react-axios-error-request-aborted-for-delete-request-in-firefox-but-not-in-chro/63711849?noredirect=1#comment123709638_63711849


### Captcha

Depending on your framework, there may be a widget that can help with this process. 

```
yarn add @hcaptcha/vue-hcaptcha
```

https://github.com/hCaptcha/vue-hcaptcha

( !!! `yarn add vue-hcaptcha` is very old !!! )


!!! LOCAL DEV WON'T WORK UNLESS YOU DO THIS

To make this work with local development, you need to configure your host to be something other than localhost

https://docs.hcaptcha.com/#local-development

TODO:
How to register and configure sitekeys



## Nodemailer

Configure Nodemailer on the API side to handle sending emails.

In a project with nodemailer already configured, searching for `nodemailer` will show other places in the project that send email.


```js
var nodemailer = require("nodemailer");
var express = require("express");
var winston = require("winston");

var config = require("../config");

var router = express.Router();
var logger = new winston.createLogger(config.logger.winston);

router.post("/", function (req, res) {
  logger.info("contact received post");

  let mailer = nodemailer.createTransport(config.transport);

  logger.info("mailer transport created");
  let debug = JSON.stringify(req.body);
  logger.info(`received request body ${debug}`);

  let text = `\n\n${req.body.fullname} has sent a message`;
  text += `\n\n${req.body.link}\n\n`;
  text += config.contact.footer;

  console.log("contact -> message text:", text);

  mailer.sendMail(
    {
      from: `"boilerplate-system-user" ${config.contact.email}`, // Sender address
      // to: req.body.contacts.join(","), // List of receivers
      to: req.body.email, // Receiver
      subject: `${req.body.fullname} has sent a message`, // Subject line
      text: text, // Plain text body
    },

    function (err, info) {
      if (err) {
        logger.error(err);
        console.log(err);
        res.json({ status: "error", err: err });
      } else {
        res.json({ status: "ok", info: info });
      }
    }
  );
});

module.exports = router;

```

### Sendmail

You can use a local sendmail package. Add it to your node's `Dockerfile`

```
FROM node:12

RUN apt-get update
RUN apt-get update && \
    apt-get install -y \
    sendmail

# And clean up the image
RUN rm -rf /var/lib/apt/lists/*
```

### Testing (Mailtrap)

It helps to use tools that you know to be reliable. 

test nodemailer with mailtrap to avoid spamming

https://mailtrap.io/

Update api configuration with something like

```
// mailtrap is a useful service for testing / development
exports.transport = {
  host: "smtp.mailtrap.io",
  port: 2525,
  auth: {
    user: "",
    pass: "",
  },
};
```

Get the credentials for the mailbox from mailtrap.io site. 

> These are *not* the same as the credentials to log in to the site!!!

Make sure config file (`index.js`) is set to be ignored by repo in `.gitignore`


### Testing (Cypress)

Even with mailtrap in the mix, it helps to have a test runner. This one only tackles the API. UI testing requires either disabling the captcha, or manually testing. 

```js
/// <reference types="cypress" />

context("Network Requests", () => {
  beforeEach(() => {
    console.log("API URL:", Cypress.env("API_URL"));
    cy.request(Cypress.env("API_URL") + "/health")
      .then((response) => {
        console.log("The API health status is: ", response);
        return response;
      })
      .its("body")
      .should("be.an", "object")
      .should("contain", {
        status: "ok",
      });
  });

  it("POSTS a message to the contact form", () => {
    cy.request({
      method: "POST",
      url: Cypress.env("API_URL") + "/contact",
      body: {
        fullname: "Test User",
        email: "boilerplate-test-user@example.com",
        comment: "Testing API directly",
        link: "https://gitlab.com/fern-seed/web-ui-api-db",
        currentpage: "/internal/testing/path",
      },
    })
      .then((response) => {
        console.log("The POST response was: ", response);
        return response;
      })
      .its("body")
      .should("contain", {
        status: "ok",
      });
  });
});

```
