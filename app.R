
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("functions.R")
source("showcase.R")
# reassignInPackage(name = "appMetadata", pkgName = "shiny", value = THLappMetadata)


# try to syntax highlight 
# https://stackoverflow.com/questions/47445260/how-to-enable-syntax-highlighting-in-r-shiny-app-with-htmloutput
prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismRdependency <- tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")


# Define UI for application that draws a histogram
ui <- fluidPage(
  prismDependencies,
  prismRdependency,
   
  div(wellPanel(class = "well.sm", style = "width: 100%",
            actionLink("TOGGLE_CODE", "show/hide code")
            )),
  
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
  HTML("<pre><code class='language-r'>
# this is a highlighted comment
test <- function(x) return(x)
       </code></pre>"),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
         )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   x <- reactiveValues(snap = TRUE)
   
   observeEvent(input$TOGGLE_CODE, {
     # shinyjs solution
      # shinyjs::toggleClass(class = "invisible", selector = ".shiny-code-container")
     
     # custom UI
     if(x$snap) {
        insertUI(".container-fluid", where = "beforeEnd", ui = div(id = "showcaseInfo", showcaseAppInfo()))
        insertUI(".container-fluid", where = "beforeEnd", ui =tags$head(tags$script("Prism.highlightAll()")))
     }
     else
       removeUI("#showcaseInfo")
     x$snap <- !x$snap
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

