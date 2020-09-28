# Lab Online Data Plotter
Simple software to plot lab data 

## Overview

The Laboratory Online Data Plotter is a simple shiny application to plot data obtained in experiments at introductory physics labs. I has been developed at the Physics Institute "Gleb Wataghin" of the University of Campinas (Campinas, Brazil). Scatter plots, log-log plots, histograms can be plotted. For the scatter plots, the Least Squares Fit is available for uncertainties in x, y, both, or none. For the histograms, it calculates mean and standard deviation and show normal curves. Detailed instructions can be found in "Notes.html".

## Lauching instructions

Laboratory Online Data Plotter runs on browser tab. It can be lauched from an R editor, like RGui or RStudio. Using RGui:
1. Create a folder named "Plotter" and include the files *ui.R*, *server.R*, and *Notes.html*.
2. Create a folder named "Data" in folder "Plotter".
3. In RGui, change directory to "Plotter"
4. Interpret *ui.R* and *server.R*:

```ruby
source("ui.R")
source("server.R")
```

5. To lauch the application:
>runApp()
6. A browser tab will be openned and you can work!
