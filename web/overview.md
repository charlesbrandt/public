# General / Introductory

http://en.wikipedia.org/wiki/Website
Website - Wikipedia, the free encyclopedia

## 2020.03.03 11:56:05

So you want a website, huh?

Exciting! These days I recommend squarespace.com. They make the process pretty intuitive. I'm happy to walk through it with you.

Your homework (!?!?) before we meet is to start writing content and collecting any pictures that you want to use. In my experience, this is the hardest part. You can start with something simple like a document in a word processor (even an email) and just write some of the things you want to include. It doesn't have to be a final draft... just rough notes are fine. Think about questions like "What service do I offer?", "How much does it cost?", or frequently asked questions that clients may have.

It also helps to search for other examples of sites that you like. They could be sites that offer a similar service to yours, or just sites that you like the way they look.

## Creating a Website

A high level over view.

An undefined problem has an infinite number of solutions.
-- Robert A. Humphrey

Many people want to have a website of their own. The first thing to do is to decide on the reason why you want a website. Set a goal for the site, and look for the shortest path to meet that goal.

The next step is to make the site. Remember: keep it simple.

## Free Solutions

The easiest option is to use a free hosted solution like `google sites <http://sites.google.com/?hl=en&tab=w3&pli=1>`\_, blogger, wordpress, facebook, myspace, livejournal, twitter, etc. These solutions are _very_ user friendly and easy to get started. If you don't have much experience creating web sites, I encourage you to start here. These solutions will help you get familiar with some of the concepts of creating a website.

Even if you want your site to do more eventually, this is a great place to start. Having something in place lets gives you a frame of reference to help you define what else you need. At the very least, you'll start organizing the content you'll need for other solutions. At the very best, this option might do everything you wanted it to. Great!

## Your Own Domain Name

The next step up on the road of website development is to register your own domain name. If you have your own domain name, you can point it to one of the free hosted solutions already mentioned. With your own domain name, you can always change where it goes. This gives you complete control over what is displayed and how it is displayed. If you change your mind on the system you want to use for the site, you can change it without having to tell people the new address. (Some links to specific content may be broken in this scenario.)

If you need a recommendation for a good affordable host, try Dreamhost:

http://www.dreamhost.com/r.cgi?266169

If you wouldn't mind saying that I referred you, just use that link, or when asked for an email, use admin [at] charlesbrandt [dot] com (formatted as a real email address).

### Registration

To find an available domain name, go to:
http://www.dreamhost.com/domains.html

Under "Check domain availability" enter the domain name of choice. Press the "Check Availability" button.
If the name is available, you will see a page that says:
Congratulations! <domain> is available for registration!

Once you've decided on a name, press the button that says:
"Register <domain>..."

On the next page "Start Your Free Trial!" enter email and password to create an account. You'll use this to make changes in the future, so remember it.

On the next page "Pick a domain..." The default of "Register a new .com/net/org/info domain for me." is correct. Re-enter the domain in the "Domain:" box.

On the next page "Pick a plan..." under "Just register <domain> with DreamHost for:"
choose how many years to register the domain.

Make sure that you do _not_ select an option under "Full web hosting:", unless you plan to host your website at Dreamhost. They are a great host, but this will cost ~$9/month. If you're not certain you'll actually use your website, this can be more expensive.

The rest of the steps should be pretty self explanatory.

Once you're registered, you'll need to set up your name to the location you have your free site hosted.

This varies from service to service. Generally you'll want to add an entry to the DNS records at Dreamhost that forward traffic from your new domain name to the address currently hosting your website. You may also need to configure the service hosting your website so they know to accept traffic for your domain name.

## Static Sites

The next easiest thing to do for more control over the content is create your website with static content. You will need a way to create the [static content] and a way to host it on a publicly available web server (web host).

Remember: Don't get lost in the infinite ways of presenting your content! Start simple.

It's easy to get lost with the details of creating a website.

From here it might make sense to start with notes on making a `static website <static-websites.html>`\_

## Dynamic Sites

Middle ground... template based static site generation. (some programming, still static results). python/templates.txt

If you find that you are changing your static content frequently, but have difficulty keeping the navigational elements of your site up to date with the changes, it may be time to investigate a dynamically generated site. These require software or programming to automatically generate the dynamic parts of your site. These solutions may also require a different hosting solution. General solutions fall in the realm of content management systems (CMSes) and custom solutions involve using web frameworks to create web applications, or just starting from scratch with custom code to interact with a database or other storage mechanism. Get ready to do a lot of research, or call in your local programmer at this point!

## Web Applications / Dynamic Sites

Dynamic sites can use a number of techniques to generate pages:

### Server side

Web servers can process, store, and generate pages based on parameters of what is requested (user, location, etc). To process requests and generate responses, a programming or scripting language is used for custom behavior. I prefer Python, but most languages provide web related frameworks and libraries.

There are also many considerations to make when choosing what type of server and other services to use for the infrastructure of the site. These considerations can get into system administration topics.

### Client side

Much can be accomplished directly in a web browser these days using Javascript, CSS and HTML5.

## Open Designs

Originally I used a design found via: `oswd.org <http://www.oswd.org/>`\_

Thanks `Payal Dhar <http://writeside.net/>`\_ for the very nice site design and open permissions for use.

## Further reading

Web development touches many spaces.

[Code](../code)
[Systems](../system/administration)
