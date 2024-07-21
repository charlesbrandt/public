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

## Hosted solutions

https://sendgrid.com/en-us/pricing  
Pricing and Plans | SendGrid  
https://github.com/MoonMail/MoonMail  
MoonMail/MoonMail: Email marketing platform for bulk emailing via Amazon SES (Google Cloud Platform and Azure coming soon)  
https://moonmail.io/pricing  
Email marketing software Pricing | MoonMail  
https://github.com/MoonMail/MoonMail/forks  
Forks · MoonMail/MoonMail  

https://duckduckgo.com/?t=ffab&q=Amazon+SES&ia=web  
Amazon SES at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=Amazon+SES+google+equivalent&ia=web  
Amazon SES google equivalent at DuckDuckGo  
https://www.reddit.com/r/googlecloud/comments/gfzov7/ses_equivalent/  
SES Equivalent? : r/googlecloud  
https://cloud.google.com/compute/docs/tutorials/sending-mail#using_standard_email_ports  
Sending email from an instance  |  Compute Engine Documentation  |  Google Cloud  
https://mailtrap.io/blog/amazon-ses-alternatives/  
Top 6 Amazon SES Alternatives [2024]: Best Options Compared  
https://stackoverflow.com/questions/60088537/what-is-the-google-smtp-service-similar-to-amazon-ses-is-it-gmail-smtp  
What is the google smtp service similar to amazon ses, is it gmail smtp? - Stack Overflow  
https://stackshare.io/stackups/amazon-ses-vs-google-cloud-messaging  
Amazon SES vs Google Cloud Messaging | What are the differences?  

https://github.com/knadh/listmonk  
knadh/listmonk: High performance, self-hosted, newsletter and mailing list manager with a modern dashboard. Single binary app.  
https://listmonk.app/  
listmonk - Free and open source self-hosted newsletter, mailing list manager, and transactional mails  
https://listmonk.app/docs/installation/  
Installation - listmonk / Documentation  
https://gist.github.com/MaximilianKohler/e5158fcfe6de80a9069926a67afcae11  
Complete Listmonk setup guide. Step-by-step tutorial for installation and all basic functions. Amazon EC2 & SES  

https://github.com/mjmlio/mjml  
mjmlio/mjml: MJML: the only framework that makes responsive-email easy  
https://www.mailjet.com/  
Email Delivery Service for Marketing & Developer Teams | Mailjet  

https://github.com/topics/email-campaigns  
email-campaigns · GitHub Topics  
https://github.com/freeCodeCamp/mail-for-good  
GitHub - freeCodeCamp/mail-for-good: An open source email campaign management tool for nonprofits  
https://github.com/TedGoas/Cerberus  
GitHub - TedGoas/Cerberus: A few simple, but solid patterns for responsive HTML email templates and newsletters. Even in Outlook and Gmail.  
https://github.com/zudochkin/awesome-newsletters  
GitHub - zudochkin/awesome-newsletters: A list of amazing Newsletters  

https://duckduckgo.com/?t=ffab&q=email+list+manager&ia=web  
email list manager at DuckDuckGo  
https://clean.email/best-email-list-management-software  
5 Best Email List Management Software To Use in 2024  
https://github.com/topics/email-list  
email-list · GitHub Topics · GitHub  
https://github.com/topics/email  
email · GitHub Topics · GitHub  
https://github.com/topics/newsletter  
newsletter · GitHub Topics · GitHub  
https://github.com/topics/subscriptions  
subscriptions · GitHub Topics · GitHub  
https://github.com/killbill/killbill  
GitHub - killbill/killbill: Open-Source Subscription Billing & Payments Platform  
https://github.com/dunglas/mercure  
GitHub - dunglas/mercure: 🪽 An open, easy, fast, reliable and battery-efficient solution for real-time communications  
https://github.com/topics/subscriptions-list  
subscriptions-list · GitHub Topics · GitHub  

https://duckduckgo.com/?t=ffab&q=listmonk+vs+mautic&ia=web  
listmonk vs mautic at DuckDuckGo  
https://www.reddit.com/r/selfhosted/comments/zp089g/comparison_of_ui_options_for_bulk_email_sending/  
Comparison of UI options for bulk email sending with Amazon SES and other SMTP providers. Review of Sendy, Mailwizz, and Listmonk. : r/selfhosted  
https://www.libhunt.com/compare-mautic-vs-listmonk  
Mautic vs Listmonk - compare differences and reviews? | LibHunt  
https://github.com/mautic/mautic  
GitHub - mautic/mautic: Mautic: Open Source Marketing Automation Software.  
https://github.com/topics/marketing-automation  
marketing-automation · GitHub Topics · GitHub  
https://github.com/Mailtrain-org/mailtrain  
GitHub - Mailtrain-org/mailtrain: Self hosted newsletter app  

https://github.com/topics/email-campaigns  
email-campaigns · GitHub Topics · GitHub  
https://github.com/topics/email-marketing  
email-marketing · GitHub Topics · GitHub  
https://github.com/topics/newsletter  
newsletter · GitHub Topics  
https://github.com/knadh/listmonk  
knadh/listmonk: High performance, self-hosted, newsletter and mailing list manager with a modern dashboard. Single binary app.  
https://github.com/topics/campaign-management  
campaign-management · GitHub Topics  
https://listmonk.app/  
listmonk - Free and open source self-hosted newsletter, mailing list manager, and transactional mails  
https://railway.app/pricing  
Pricing | Railway  
https://www.pikapods.com/login?redirect=/pods  
PikaPods - Login  
https://dash.elest.io/signup  
Managed Service Dashboard  
https://dash.elest.io/20308/default-project/services/creation  
Managed Service Dashboard  
https://github.com/topics/email-newsletter  
email-newsletter · GitHub Topics · GitHub  
https://github.com/rykov/paperboy  
GitHub - rykov/paperboy: 💌💨 Email Campaign Delivery built with GoLang inspired by GoHugo  
https://www.paperboy.email/  
Paperboy  

