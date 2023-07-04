# Tailwind

Heavy overlap with CSS documentation.

See there for the concepts behind these utility classes. 


## Layout References

### Responsive Grid

On wider screens, don't want to keep the card options artificially width constrained. Use the space that is available. 

https://tailwindcss.com/docs/grid-template-columns  
Grid Template Columns - Tailwind CSS  

```
      <div
        class="
          grid grid-cols-2
          md:grid-cols-3
          lg:grid-cols-4
          xl:grid-cols-5
          2xl:grid-cols-6
        "
      >
        <div v-for="index in 20">
          {{ index }}
        </div>
      </div>
```

In tailwind, the default is for the smallest screen

https://tailwindcss.com/docs/responsive-design  
Responsive Design - Tailwind CSS  

https://tailwindcss.com/docs/grid-template-rows  
Grid Template Rows - Tailwind CSS  
https://tailwindcss.com/docs/grid-row  
Grid Row Start / End - Tailwind CSS  
https://tailwindcss.com/docs/transition-property  
Transition Property - Tailwind CSS  


https://medium.com/@harrycresswell/building-a-responsive-card-system-d98f93794e1a

### Responsive Design

https://duckduckgo.com/?t=ffab&q=tailwind+breakpoints&ia=web  
tailwind breakpoints at DuckDuckGo  
https://blog.logrocket.com/tailwind-css-dynamic-breakpoints-and-container-queries/  
Tailwind CSS: Using dynamic breakpoints and container queries - LogRocket Blog  
https://duckduckgo.com/?t=ffab&q=vue+call+method+on+window+change+event&ia=web  
vue call method on window change event at DuckDuckGo  
https://masteringjs.io/tutorials/vue/resize-event  
How to Handle Window Resize Events in Vue - Mastering JS  
https://stackoverflow.com/questions/72190270/prisma-throwing-error-ambiguous-relation-detected  



### Flex

Good for rendering rows of columns, like a data table. 
e.g. want the rows to stay in the same row. 

https://tailwindcss.com/docs/container  
Container - Tailwind CSS  
https://tailwindcss.com/docs/flex-direction  
Flex Direction - Tailwind CSS  

To achieve a 'row' like flex container, on the parent row element include:

```
class="flex flex-wrap relative"
```

`box-sizing: border-box` is the default (`box-border`) in Tailwind
https://tailwindcss.com/docs/box-sizing


Then, for the contents in a row, use different combinations of flex options to specify how the contents grow and shrink

`flex-auto` works well for items you want to grow to fill space
`flex-none` works well for items that should stay their initial size

https://tailwindcss.com/docs/flex


Some column based layouts (e.g. 24 columns) will make use of `max-width` and e.g. `flex: 0 0 87.5%;` to ensure elements stay in line with the corresponding column concepts. I don't think this transfers to a purely tailwind based layout. (Use a grid in this case?)

I still like using `<div>` and `<span>` to approximate rows and columns. 

https://tailwindcss.com/docs/flex-wrap  
Flex Wrap - Tailwind CSS  
https://tailwindcss.com/docs/position  
Position - Tailwind CSS  
https://tailwindcss.com/docs/flex-direction  
Flex Direction - Tailwind CSS  
https://tailwindcss.com/docs/flex  
Flex - Tailwind CSS  
https://tailwindcss.com/docs/box-sizing  
Box Sizing - Tailwind CSS  
https://tailwindcss.com/docs/flex-shrink  
Flex Shrink - Tailwind CSS  


### Cards

A good source of inspiration

https://tailwindcomponents.com/components/cards



### Navbar

Combine the above to define the interaction you want

see web-ui-api-db/ui/content/navbar.md


## Library References

### Positioning

https://developer.mozilla.org/en-US/docs/Web/CSS/display  
display - CSS: Cascading Style Sheets | MDN  

> The display CSS property sets whether an element is treated as a block or inline element and the layout used for its children, such as flow layout, grid or flex.

> Formally, the display property sets an element's inner and outer display types. The outer type sets an element's participation in flow layout; the inner type sets the layout of children. Some values of display are fully defined in their own individual specifications; for example the detail of what happens when display: flex is declared is defined in the CSS Flexible Box Model specification. See the table at the end of this document for all of the individual specifications.

https://tailwindcss.com/docs/display  
Display - Tailwind CSS  

https://tailwindcss.com/docs/overflow  
Overflow - Tailwind CSS  
https://tailwindcss.com/docs/container  
Container - Tailwind CSS  
https://tailwindcss.com/docs/position  
Position - Tailwind CSS  
https://duckduckgo.com/?t=ffab&q=css+max-height+overflow+scroll&ia=web  
css max-height overflow scroll at DuckDuckGo  
https://stackoverflow.com/questions/24227053/max-height-overflowscroll-and-position  
css - max-height, overflow:scroll and position - Stack Overflow  
https://duckduckgo.com/?q=tailwind+max+height+overflow+scroll&t=ffab&ia=web  
tailwind max height overflow scroll at DuckDuckGo  

https://tailwindcss.com/docs/top-right-bottom-left  
Top / Right / Bottom / Left - Tailwind CSS  

https://tailwindcss.com/docs/z-index  
Z-Index - Tailwind CSS  


https://tailwindcss.com/docs/grid-template-columns  
Grid Template Columns - Tailwind CSS  


### Sizing

https://tailwindcss.com/docs/width  
Width - Tailwind CSS  
https://tailwindcss.com/docs/height  
Height - Tailwind CSS  
https://tailwindcss.com/docs/align-items  
Align Items - Tailwind CSS  
https://tailwindcss.com/docs/justify-content  
Justify Content - Tailwind CSS  
https://tailwindcss.com/docs/margin  
Margin - Tailwind CSS  
https://tailwindcss.com/docs/font-size  
Font Size - Tailwind CSS  
https://tailwindcss.com/docs/font-weight  
Font Weight - Tailwind CSS  
https://tailwindcss.com/docs/ring-width  
Ring Width - Tailwind CSS  
https://tailwindcss.com/docs/border-radius  
Border Radius - Tailwind CSS  
https://tailwindcss.com/docs/border-width  
Border Width - Tailwind CSS  

https://tailwindcss.com/docs/max-height  
Max-Height - Tailwind CSS  
https://tailwindcss.com/docs/min-height  
Min-Height - Tailwind CSS  

https://tailwindcss.com/docs/resize  
Resize - Tailwind CSS  


### Color

https://tailwindcss.com/docs/border-color  
Border Color - Tailwind CSS  
https://tailwindcss.com/docs/background-color  
Background Color - Tailwind CSS  

```
bg-gray-200
```

https://tailwindcss.com/docs/background-image  
Background Image - Tailwind CSS  


### Visibility

## Forms

https://github.com/tailwindlabs/tailwindcss-forms  
tailwindlabs/tailwindcss-forms  

## UI Dev Tools

Upcoming tool stack that looks very promising. Currently [2021.10.05] sponsor ware only. 

https://ui-devtools.com/  
UI Devtools for Tailwind CSS  
https://twitter.com/uidevtools  
UI devtools (@uidevtools) / Twitter  

https://tailwindcss.nuxtjs.org/tailwind-viewer/  
Nuxt TailwindCSS  

## Utilities

https://tailwindcss.com/docs/theme  
Theme Configuration - Tailwind CSS  
https://tailwindcss.com/docs/adding-base-styles  
Adding Base Styles - Tailwind CSS  
https://tailwindcss.com/docs/extracting-components  
Extracting Components - Tailwind CSS  
https://tailwindcss.com/docs/adding-new-utilities  
Adding New Utilities - Tailwind CSS  
https://tailwindcss.com/docs/functions-and-directives  
Functions & Directives - Tailwind CSS  

## Why Utility based?

https://tailwindcss.com/docs/utility-first  
Utility-First - Tailwind CSS  
  
https://fullstackradio.com/75  
75: Diana Mounter - Design Systems and Utility Classes at GitHub · Full Stack Radio  
https://medium.com/@johnpolacek/by-the-numbers-a-year-and-half-with-atomic-css-39d75b1263b4  
By The Numbers: A Year and Half with Atomic CSS | by John Polacek | Medium  
https://acss.io/  
Atomic CSS  
https://www.algolia.com/blog/redesigning-our-docs-part-4-building-a-scalable-css-architecture/  
Redesigning Our Docs – Part 4 – Building a Scalable CSS Architecture | Algolia Blog  
https://ricostacruz.com/til/another-look-at-tailwind  
A closer look at Tailwind CSS  
  
## Preprocessors

Tailwind uses PostCSS for processing the CSS

~/alpha/public/web/style/postcss.md

https://duckduckgo.com/?t=canonical&q=does+tailwind+css+rely+on+sass%3F&ia=web  
does tailwind css rely on sass? at DuckDuckGo  
https://polished.js.org/  
polished | A lightweight toolset for writing styles in JavaScript  
https://tailwindcss.com/docs/using-with-preprocessors#app  
Using with Preprocessors - Tailwind CSS  

https://duckduckgo.com/?t=canonical&q=tailwind+css+postcss&ia=web  
tailwind css postcss at DuckDuckGo  

## Apply & Hover

Generally, it's easier to *not* use apply. Creating arbitrary semantic class names for styles is something that tailwind cuts out of the picture. 

When using @apply in css class definitions, including tailwind's `hover:` functions will not work as desired.

Instead, create a separate css class something:hover and apply the class definitions specific to hover there

For an example, see
web-ui-api-db/ui/components/common/navigation.vue

https://github.com/tailwindlabs/tailwindcss/issues/2848

## Installation

https://tailwindcss.com/docs/installation  
Installation - Tailwind CSS  
https://tailwindcss.com/docs/configuring-variants  
Configuring Variants - Tailwind CSS  

https://tailwindcss.com/docs/guides/vue-3-vite

in `tailwind.config.js`, configure the `purge` option

```
purge: ['./index.html', './src/**/*.{vue,js,ts,jsx,tsx}'],
```

After adding in Tailwind to the main CSS file (e.g. `tailwind.css` or `index.css`), include some defaults:

```
@tailwind base;
@tailwind components;
@tailwind utilities;

@layer base {
  h1 {
    @apply text-3xl;
  }
  h2 {
    @apply text-2xl;
  }
  h3 {
    @apply text-xl;
  }
  h4 {
    @apply text-lg;
  }
}
```

https://tailwindcss.com/docs/adding-base-styles


### Vue 2

Using Vue CLI:

```
npm uninstall tailwindcss postcss autoprefixer
```

then

```
vue add tailwind 
```

## Integration

I often want to use Tailwind in projects with existing Component Libraries. Sometime the other libraries even contain their own utility classes. 

Using PurgeCSS may help the two co-exist:

https://www.scottbrady91.com/general/adding-tailwind-utility-classes-to-your-bootstrap-website



## See also

https://tailwindcss.com/docs/just-in-time-mode  
Just-in-Time Mode - Tailwind CSS  

https://tailwindcss.nuxtjs.org/examples/basic  
Basic Usage Example - Nuxt TailwindCSS  


https://duckduckgo.com/?t=canonical&q=tailwind+css&ia=web  
tailwind css at DuckDuckGo  

