============================
Creating a Static Website
============================
By: Charles Brandt
http://www.charlesbrandt.com
site [at] charlesbrandt [dot] com
On: 2009.09.19 18:59:34
Updated: 2013.12.20

Static websites are great option for sites that don't change often, or are changed by only one person and a custom look and navigation is needed.

# Tools

If you're not familiar with HTML, start with a WYSIWYG (What You See Is What You Get) Editor for web pages.

http://en.wikipedia.org/wiki/WYSIWYG
WYSIWYG - Wikipedia, the free encyclopedia

These are often changing, based on what is being actively developed. I try to keep an up to date list in:
wysiwyg_editors.txt

Some of the more promising ones include:
http://www.bluegriffon.org/

Bluegriffon is the most up to date version of kompozer I've found recently

http://en.wikipedia.org/wiki/BlueGriffon

and a browser based editor:
http://maqetta.org/
http://en.wikipedia.org/wiki/Maqetta

http://en.wikipedia.org/wiki/Comparison_of_HTML_editors

It’s a good idea to familiarize yourself with some of the basic concepts of HTML. It's really not too scary. It's a _markup_ language, not a programming language. Instead of selecting text in a word processor and pressing a "Bold" button, you surround the text with bold tags <b>like this</b>.

There are many great tutorials available on the web. This looks like a good one:
http://www.webreference.com/html/tutorial1/

Once you know HTML, it is possible to only use a basic text editor to create web pages. This is equivalent to using the Source view in KompoZer.

# Templates

From there start with a free template:

http://www.google.com/search?hl=en&client=firefox-a&rls=org.mozilla%3Aen-US%3Aofficial&hs=wA8&q=open+web+templates&aq=f&oq=&aqi=
open web templates - Google Search
http://www.oswd.org/
Open Source Web Design - Download free web design templates.
http://www.opendesigns.org/
Open Design Community - Download Free Web Design Templates - OpenDesigns.org
http://www.openwebdesign.org/
Open Web Design - Download Free Web Design Templates
http://www.opensourcetemplates.org/
Open Source Templates | Free CSS and XHTML Website Templates

Or turn any existing design you have into a template.

- extract any style elements into a style sheet.
- only include header, navigation and footer elements in the template

Or generate a template using a templating language. (More on this later)

# Content

If you haven't done this already, now is the time to organize your content. It's always a good idea to consider how your content is organized and evaluate if there are better structures for it. The more organized you are here, the more organized your website will be.

One way to think about the organization is to think of the main navigation bar for your site. Typically, there are at most 10 items in a navigation bar, and the fewer you can manage, the better. At the same time, organizing your content into an overly deep structure is also something to avoid. People will have to click on many links to get the information they're looking for. So try to pick the top 5 categories of your site. For most sites, this may be all that's needed.

Don't worry if you don't get it right the first time. This is the great thing about creating your own site. You can try things out and improve them later, as time permits.

Start with a folder called "content". Move your files into it. The content folder should contain the following items:

content/
css/
images/
files/
other_content_pages...
other_content_directories...
template.html

If a topic will only have one page, make the content page a file. If a topic has multiple pages, make a directory and put each page file in that directory.

Remember:
Make sure that you give plenty of thought to the navigation. It should roughly mirror the structure you have created in content.

After you have your content organized, make a directory that is the name of your site. I recommend using the base url for the site here. (don't include the www prefix).

Within the site directory, make two folders, content and static_site. Your folder should now look something like:
::

example.com/
content/
static_site/

Move the previously organized items in your content directory here into the new folder called content.

# Building the Site

At this point you have all of the pieces to create your site. Now it is time to put them together.

That process is: take the files with content, insert them each into the template, save the result in a static file for use on the site.

Once you have a template, you can start inserting your content for each page that you specified in the navigation. To simplify things, make sure to call the main file of your site "index.html". This is the default home page. Most servers know that if they see that file to use it by default if no other page is requested explicitly.

# Publishing Your Site

This guide assumes you have already _registered your domain name_ and _configured hosting for your site_. (TODO: link to those guides)

At this point you should have all of the static content prepared for your site. You should be able to browse the pages locally, using your browser's "Open File..." option.

The next step is to publish your site to your web server or hosting provider. Most often this is accomplished using "FTP". FTP stands for File Transfer Protocol. This is a common way to send files from one computer to another. Kompozer has limited FTP functionality built in, or you can use a stand alone application like:

Filezilla
http://filezilla-project.org/

Cyberduck
http://cyberduck.ch/

Configure your client to log in to your web host, establish the connection to your web host (connect), and then transfer your static content (only!).

At this point, if you've done everything correctly you should be able to open a web browser, enter your domain name, and see your new pages.

Congratulations! You know how to make a web page!

If you had any trouble, please refer to Troubleshooting (TODO)

# Generating Pages Automatically

The processes described so far are manual. After you've cut and paste all of your content into a template a few times, you might feel like there should be a better way.

Don't worry. There is a better way! It's possible to use software to automatically put the pieces of your content into your template for you. This is a tedious process to do manually. Although there are many, MANY approaches to doing this, they essentially automate the process you have already learned. (Remember this if you run into any trouble with the automatic process.)

We'll start by generating those pages on our local computer and transferring them to the web server in the same way as before. If you are already familiar with a specific programming language, you may want to look for a solution that does this process in your language. Otherwise, I'll talk about a script I wrote using Python and a templating language called Mako. It's called content_station and is available here: TODO.

Make sure you have a python interpreter (python.org/download) for your computer.

Extract the contents of the content_station package into the top directory for your site.

, and then install Mako. (or just run setup.py)

Then download content_station, the python static site generator.

Using a web framework.

Other references on Static Websites:

static web site - Google Search
http://en.wikipedia.org/wiki/Static_web_page
Static web page - Wikipedia, the free encyclopedia
http://philip.greenspun.com/panda/static
Static Site Development
