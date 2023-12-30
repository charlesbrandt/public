# DPI change in Inkscape 0.92

via:
https://inkscape.org/en/learn/faq/#dpi_change

## Why did document DPI change in Inkscape 0.92?

To prepare objects whose size is given in physical units like inches (in), millimeters (mm) or point (pt) for rendering on your computer screen, Inkscape needs to know the conversion factor from these "real world" units to the purely digital size of a pixel (px).

Earlier versions of SVG and CSS specifications did not define a fixed conversion between those units. It was basically up to the application to determine something useful and - short of requiring users to measure the actual physical size of one pixel on their monitor - a common approach was to define a "pixel density" of 90 dots per inch (DPI), i.e. a pixel is fixed to be 1/90 inch in size. Inkscape 0.91 and earlier followed this approach and used a fixed value of 90 DPI for the conversion.

Recent versions of the CSS specification changed this detail as it turned out to be impractical: conversion factors varied widely across different systems and applications and they often used "arbitrary" fixed conversion factors of their own choice. To unify behavior, a pixel density of 96 DPI is now the universal standard which is followed by all web browsers and is also part of the upcoming SVG2 standard.

In order to match the rendering of browsers and become compatible to SVG2, Inkscape has adjusted the pixel density used internally from 90 DPI to 96 DPI. While this ensures that your artwork will always look the same, regardless how or where it's shown, it also means that old documents have to be scaled if their size should stay identical:

  - a document that is 90 pixels wide had a physical width of exactly 1 inch in Inkscape 0.91 (90 px / 90 DPI = 1 in)
  - the same document will be 6% smaller in Inkscape 0.92 (90 px / 96 DPI = 0.9375 in)

## What method should I select to update my files?

Functionality to automatically scale your documents to account for the change in document DPI has been added to Inkscape 0.92. As not all scaling methods work equally well for all applications, Inkscape gives you three choices, so you can select the method that suits your requirements best:

  - Files containing artwork for screen display:  
    Files that are intended to be displayed on computer monitors, mobile phones, digital TVs and similar devices usually do not need to be rescaled at all. A pixel in Inkscape 0.91 is still exactly one pixel in size in Inkscape 0.92. If you created a desktop background image with a size of 1920 x 1080 pixels in Inkscape 0.91, it will still have this size in Inkscape 0.92. This option is the safest, as it does not do any scaling at all. You can also use it for other types of artwork for which the exact physical size is not important.

  - Files intended for physical output:  

    This includes all files that were designed to have a specific physical size that needs to be maintained, e.g. documents intended to be printed on a specific paper size like US letter or A4, technical drawings that were drawn to scale, documents that are intended to be processed by a 3D printer / plotter / cutter,  and any other file that needs a fixed physical size for some reason.

    All artwork of this type needs to be scaled to keep physical dimensions identical, and there are two approaches for this scaling that have different strengths:

    - Method 2a preserves the visual appearance of elements, especially for clipped or masked objects, filters, and clones.  
      For this option, Inkscape scales the whole drawing as if it were one big group (by transforming it using the SVG document's viewBox attribute). This means that the size of each individual element in the SVG source will stay as-is (i.e. too small), but that after scaling the whole image, the size of elements will be as expected (you can think of it as using a magnifying lens to look at the image - while size of the original image stays identical the resulting image appears larger). This is the best option if you want to preserve the visual appearance of your document and it's the correct choice in almost all cases where scaling is required.

    - Method 2b preserves the physical size of each individual element.  
      For this option, Inkscape scales each element separately. This means that a single line that was 1 in long in Inkscape 0.91 (for example think of a 1:1 drawing of the scale on a ruler) will still be 1 in long after scaling in Inkscape 0.92, which requires changing its size in the SVG source from 90 px to 96 px. This is especially important if the path data itself has to be used (for example to provide coordinates for a 3D printer, plotter or cutter). The downside: Scaling each element individually is error-prone and might result in different visual appearance (especially for the elements listed above) and can even fail completely in some cases. Only use this option if you depend on unscaled elements with proper physical size in the resulting SVG file.

Further information

  - CSS specification for the definition of "Absolute lengths" in the CSS Values and Units Module (specifically see "Note: This definition of the pixel unit [...]")
  - Specification of "Units" in SVG 1.1 and "Units" in SVG 2
  - Some known bugs about Method 2b (scale individual elements) can be found by searching the Launchpad bugtracker
