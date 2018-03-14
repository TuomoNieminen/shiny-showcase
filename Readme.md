## About

This is a demostration of a customization to a R shiny application, 
which allows the user to toggle the code and a readme file from within the application. 

## Use

You need the latest version of the R program and the following packages.

```
install.packages("shiny", "shinyjs")
```

To run the application, call

```
shiny::runGitHub("shiny-showcase", user = "TuomoNieminen")
```

## Details

The implementation is heavily inspired by [shiny showcase](https://shiny.rstudio.com/articles/display-modes.html) and uses parts of it's [code](https://github.com/rstudio/shiny/blob/master/R/showcase.R).

