# Data Visualization (Data Viz)

Start with the question you're trying to answer. 

example: How many people visited on a given day? 

Then consider how you would like to present that data. 

Use whatever tool is available to you. 


There are a few basic charts / layouts that are common for a reason. It helps to understand how they map to data. 

Pie chart..
you count some number of things in different categories. 
You want to show it. 

Just pass those numbers to a PieChart object... it should be able to do the rest. 

Defining the series is important.  
The key is really in creating series  

Summarize your data. 

Consider the basics. What data do you want to show? 

The trick seems to be in getting the data converted into a format that works well for the visualization. 

https://duckduckgo.com/?t=ffab&q=javascript+prepare+data+for+visualization+&ia=web  
javascript prepare data for visualization at DuckDuckGo  
http://jsdatav.is/intro.html  
Data Visualization with JavaScript  
https://www.edwardtufte.com/tufte/books_vdqi  
Edward Tufte: Books - The Visual Display of Quantitative Information  
https://python.plainenglish.io/data-visualization-with-python-and-javascript-c1c28a7212b2  
Data visualization with Python and JavaScript | by Veronika Rovnik | Python in Plain English  

## Chart js

https://duckduckgo.com/?q=chart.js+vs+plotly&hps=1&atb=v343-1&ia=web  
chart.js vs plotly at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=charts.js&atb=v343-1&ia=web  
charts.js at DuckDuckGo  
https://www.chartjs.org/docs/latest/getting-started/installation.html  
Installation | Chart.js  
https://www.chartjs.org/docs/latest/samples/scales/linear-step-size.html  
Linear Scale - Step Size | Chart.js  
https://github.com/chartjs/Chart.js  
chartjs/Chart.js: Simple HTML5 Charts using the `<canvas>` tag  
https://github.com/topics/html5-charts  
html5-charts · GitHub Topics  
https://github.com/timqian/chart.xkcd  
timqian/chart.xkcd: xkcd styled chart lib  

## Plotly

https://duckduckgo.com/?t=ffab&q=plotly+github&atb=v343-1&ia=web  
plotly github at DuckDuckGo  
https://github.com/plotly  
Plotly  
https://github.com/plotly/plotly.js  
plotly/plotly.js: Open-source JavaScript charting library behind Plotly and Dash  


## Libraries

Many libraries to help with the visualization side. (especially in Javascript)

https://github.com/topics/visualization  
visualization · GitHub Topics · GitHub  

TODO:

Compare and contrast Plotly vs. Echarts vs Apex Charts vs Vega-Lite

weight of library + dependencies (download size)  
ease of configuration options  
work with vue?  
API similarities and differences and pros and cons

### Plotly / D3 / VisX

Plotly is built on top of D3 and has a high level API for common charts. 

https://plotly.com/javascript/getting-started/#start-plotting

```
npm install plotly.js-dist 
yarn add plotly.js-dist
```


```
import Plotly from 'plotly.js-dist'
```


https://github.com/plotly/plotly.js  
GitHub - plotly/plotly.js: Open-source JavaScript charting library behind Plotly and Dash  

https://plotly.com/javascript/  
Plotly JavaScript Graphing Library | JavaScript | Plotly  
https://plotly.com/javascript/plotly-fundamentals/  
Fundamentals | JavaScript | Plotly  
https://plotly.com/javascript/setting-graph-size/  
Setting Graph Size | JavaScript | Plotly  
https://plotly.com/javascript/responsive-fluid-layout/  
Responsive / Fluid Layouts | JavaScript | Plotly  
https://plotly.com/javascript/configuration-options/  
Configuration Options | JavaScript | Plotly  
https://plotly.com/javascript/axes/  
Axes | JavaScript | Plotly  
https://plotly.com/javascript/horizontal-bar-charts/  
Horizontal Bar Charts | JavaScript | Plotly  
https://plotly.com/javascript/bar-charts/  
Bar Charts | JavaScript | Plotly  


https://plotly.com/javascript/  
Plotly JavaScript Graphing Library | JavaScript | Plotly  
https://plotly.com/javascript/configuration-options/  
Configuration Options | JavaScript | Plotly  
https://duckduckgo.com/?t=ffab&q=plotly+vue&ia=web  
plotly vue at DuckDuckGo  
https://www.somesolvedproblems.com/2018/05/how-to-use-plotly-in-vue.html  
How To Use Plotly In Vue ~ Random Problems  
https://github.com/statnett/vue-plotly  
GitHub - statnett/vue-plotly: A vue wrapper for plotly.js chart library  
https://github.com/David-Desmaisons/vue-plotly  
GitHub - David-Desmaisons/vue-plotly: 📈 vue wrapper for plotly.js  
  
https://d3js.org/  
D3.js - Data-Driven Documents  
https://github.com/d3/d3  
GitHub - d3/d3: Bring data to life with SVG, Canvas and HTML.  
https://observablehq.com/@d3/learn-d3  
Learn D3: Introduction / D3 / Observable  
https://vega.github.io/vega-lite/  
A High-Level Grammar of Interactive Graphics | Vega-Lite  
https://observablehq.com/@d3/bar-chart-race  
Bar Chart Race / D3 / Observable  
https://github.com/airbnb/visx  
GitHub - airbnb/visx: 🐯 visx | visualization components  
https://duckduckgo.com/?t=ffab&q=d3&ia=web  
d3 at DuckDuckGo  

### Echarts

Very highly starred on githup

https://github.com/apache/echarts  
GitHub - apache/echarts: Apache ECharts is a powerful, interactive charting and data visualization library for browser  
https://echarts.apache.org/en/tutorial.html#ECharts%20Basic%20Concepts%20Overview  
Documentation - Apache ECharts  

### Apex Charts

Looks nice!

https://github.com/apexcharts/apexcharts.js  
GitHub - apexcharts/apexcharts.js: 📊 Interactive JavaScript Charts built on SVG  
https://github.com/apexcharts/vue-apexcharts  
GitHub - apexcharts/vue-apexcharts: 📊 Vue.js component for ApexCharts  

### Vega / Vega-Lite

https://github.com/vega/vega-lite  
GitHub - vega/vega-lite: A concise grammar of interactive graphics, built on Vega.  
https://vega.github.io/  
Vega  
https://github.com/bokeh/bokeh  
GitHub - bokeh/bokeh: Interactive Data Visualization in the browser, from Python  


## Highcharts

https://www.highcharts.com/  
Interactive JavaScript charts for your webpage | Highcharts  

Highcharts has good documentation. A paid license is required for commercial projects. 

https://www.highcharts.com/docs/chart-concepts/series

Reading the Highcharts documentation helps illustrate components common to charts and data visualization. 

https://www.highcharts.com/docs/chart-concepts/understanding-highcharts

### Installation



### Disable chart title

```
title:{
    text:''
}
```

https://stackoverflow.com/questions/15930661/is-there-a-way-to-disable-the-title-and-subtitle-in-highcharts

### Disable high charts logo

I stash this at the top of the options
```
      pieChartOptions: {
        credits: {
          enabled: false,
        },

        chart: {
          plotBackgroundColor: null,
          plotBorderWidth: null,
          plotShadow: false,
          type: 'pie',
        },
```

### Template

Consider starting with a new component so you can group all of the logic in with the chart... 
may want to include it elsewhere

```
<template>
  <div>
    <highcharts :options="usersChartOptions"></highcharts>
  </div>
</template>

<script>
import { Chart } from 'highcharts-vue'

export default {
  name: 'UsersChart',
  components: {
    highcharts: Chart,
  },

  data() {
    return {
      items: [],
      pointStart: null,
      usersChartOptions: {
        credits: {
          enabled: false,
        },
        chart: {
          plotBackgroundColor: null,
          plotBorderWidth: null,
          plotShadow: false,
          // did not seem to have any effect
          // displayErrors: true,
        },
        title: {
          text: 'Users Per Day',
        },
        xAxis: {
          type: 'datetime',
        },
        series: [
          {
            name: 'Number of Users',
            // colorByPoint: true,
            type: 'line',
            pointInterval: 24 * 3600 * 1000, // one day
            data: [],
          },
        ],
      },
    }
  },

  computed: {},

  methods: {
    getItems() {
      this.$http.get(`${this.$config.api}/users/all`).then((response) => {
        this.items = response.data.slice(1)
        console.log('Stats -> Items', this.items)
        this.usersChartOptions.series[0].data = this.usersPerDay()
        this.usersChartOptions.series[0].pointStart = this.pointStart
      })
    },
    usersPerDay() {
      let lookup = {}
      let day = ''
      let ts = 0
      for (var item of this.items) {
        console.log('Checking item', item)
        day = item.createDate.substring(0, 10)
        ts = Date.parse(day)
        // console.log('Timestamp: ', ts)
        if (Object.prototype.hasOwnProperty.call(lookup, ts)) {
          lookup[ts] += 1
        } else {
          lookup[ts] = 1
        }
      }
      console.log('Lookup', lookup)

      let dayInMS = 24 * 3600 * 1000
      let series = []
      // these should be in milliseconds
      let keys = Object.keys(lookup).sort()
      console.log('lookup keys', keys)
      this.pointStart = Number(keys[0])

      let dates = this.range(keys[0], keys[keys.length - 1], dayInMS)
      console.log('dates range', dates)

      // keep track of a running total
      let tally = 0
      for (ts of dates) {
        // console.log('checking for', ts)
        if (Object.prototype.hasOwnProperty.call(lookup, ts)) {
          tally += lookup[ts]
        }
        series.push(tally)
      }
      console.log('Users Chart Series', series)
      return series
    },
    range(start = 0, end = undefined, step = 1) {
      // console.log('range called with', start, end, step)
      start = parseInt(start)
      end = parseInt(end)
      step = parseInt(step)

      let result = [start]
      while (start < end) {
        start += step
        if (start <= end) {
          result.push(start)
        }
      }
      return result
    },
  },
  mounted() {
    this.getItems()
  },
}
</script>

<style></style>
```


https://duckduckgo.com/?t=ffab&q=highcharts+vs+d3&ia=web  
highcharts vs d3 at DuckDuckGo  
https://medium.com/admin-dashboard-themes/highcharts-vs-d3-charts-3688bfb347a8  
HighCharts vs D3 charts. Our quick analysis of HighCharts & D3… | by Sanket Sahu | Admin & Dashboard Themes | Medium  

### Custom formatting

Trying to format a tooltip with custom markup. 

It turned out to be more difficult than expected. I ended up updating the data that is used to generate the graph to be in the desired format from the beginning. Then adjusting labels is easier to do. 



## Other Libraries

### G2

G2 looks like a nice solution. 

https://g2.antv.vision/en/docs/manual/concepts/grammar-of-graphics  
Grammar of Graphics | G2  
https://github.com/antvis/G2  
GitHub - antvis/G2: 📊 A highly interactive data-driven visualization grammar for statistical charts.  
https://github.com/antvis/G2Plot  
GitHub - antvis/G2Plot: An interactive and responsive charting library  

### Misc

https://duckduckgo.com/?t=ffab&q=javascript+data+visualization+library&ia=web  
javascript data visualization library at DuckDuckGo  
https://www.monterail.com/blog/javascript-libraries-data-visualization  
14 JavaScript Data Visualization Libraries in 2021  
https://medium.com/@Elijah_Meeks/d3-is-not-a-data-visualization-library-67ba549e8520  
D3 is not a Data Visualization Library | by Elijah Meeks | Medium  
https://github.com/joelburget/d4  
joelburget/d4: Data-Driven Declarative Documents  
https://tvjs.io/  
TVJS: Advanced charting & trading systems. - TVJS  
https://chartkick.com/vue  
Vue Chartkick - Create beautiful JavaScript charts with one line of Vue  
https://github.com/apexcharts  
ApexCharts  
https://duckduckgo.com/?t=ffab&q=vue+data+visualization+open+source&ia=web  
vue data visualization open source at DuckDuckGo  
https://alligator.io/vuejs/visualization-vue-d3/  
Data Visualization with Vue and D3 ← Alligator.io  
https://medium.com/dailyjs/data-visualization-libraries-for-vue-js-in-2020-c74fb83c5778  
Data visualization libraries for Vue.js in 2020 | by Veronika Rovnik | DailyJS | Medium  
https://en.wikipedia.org/wiki/Pivot_table  
Pivot table - Wikipedia  
  
http://dc-js.github.io/dc.js/  
dc.js - Dimensional Charting Javascript Library  

## Dashboards / Analytics / Business Intelligence

This space has heavy overlap with different tools for working with data. 

Open Tableau alternatives

https://github.com/topics/charts  
charts · GitHub Topics · GitHub  
https://github.com/metabase/metabase  
GitHub - metabase/metabase: The simplest, fastest way to get business intelligence and analytics to everyone in your company  
https://github.com/getredash/redash  
GitHub - getredash/redash: Make Your Company Data Driven. Connect to any data source, easily visualize, dashboard and share your data.  
https://github.com/keen/dashboards  
GitHub - keen/dashboards: Responsive dashboard templates 📊✨  
http://keen.github.io/dashboards/  
Dashboards by Keen IO  


## Python

If you have a python API available, Pandas library can help with this. 

