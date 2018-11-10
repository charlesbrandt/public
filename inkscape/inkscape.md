# Inkscape

Add Inskape to favorites

Launch inkscape

<img src="screenshots/0001-default-start.png" alt="When Inkscape first starts...">

Exit fullscreen and resize as needed:

<img src="screenshots/0010-exit_fullscreen.png" alt="">

Configure best default settings

  - In this order:
    - Ctrl - Shift - O
    - Ctrl - Shift - M
    - Ctrl - Shift - A
    - Ctrl - Shift - F


  - View -> Show/Hide -> Uncheck following:  
    - Commands Bar  
    - Snap Controls Bar  
    - Palette  

<img src="screenshots/0050-inkscape-view-show_hide.png" alt="">

- Consider showing arrange settings.  
  Object -> Arrange...

Exit inkscape so defaults take effect.

## Filesystem browser

Set inkscape to be the default opener for svg files
Filemanager -> browse to svg -> right click -> open with -> choose another application -> inkscape -> make default

## Default template

decide on a default template when it opens...

    cd /c/public/templates/svg/pixels
    cp 1920x1080-desktop-017.svg ~/.config/inkscape/templates/default.svg


 Just start with a blank document, change the canvas size to whatever you want, and then save the document as templates/default.svg in your Inkscape config directory (~/.config/inkscape on Linux). Then restart Inkscape, and it should open with whatever document you just saved as the default template.

via:
https://graphicdesign.stackexchange.com/questions/5830/inkscape-changing-default-canvas-size

https://duckduckgo.com/?q=inkscape+default+template&t=canonical&ia=qa

## Default fonts

Set better default font size

    Text -> Text and Font...  
    (ctrl-alt-t)  


<img src="screenshots/.png" alt="">


Also choose where you want to start with template

## TODO


add fonts to the system (see also... ???)  
configure a default font as desired

research default dpi conversion:
https://inkscape.org/en/learn/faq/#dpi_change

<img src="screenshots/" alt="">

How to create arrows? [2018.09.15 05:50:22]

## Links

*2016.02.01 18:06:47 raster vector potrace  
https://duckduckgo.com/?q=convert+raster+to+svg&t=canonical  
convert raster to svg at DuckDuckGo  
https://en.wikipedia.org/wiki/Image_tracing  
Image tracing - Wikipedia, the free encyclopedia  
https://en.wikipedia.org/wiki/Potrace  
Potrace - Wikipedia, the free encyclopedia  


*2014.05.07 18:49:09  
was curious what the tweak tool does...  
interesting...  
like a smudge tool for vector  

http://tavmjong.free.fr/INKSCAPE/MANUAL/html/Tweak.html  
