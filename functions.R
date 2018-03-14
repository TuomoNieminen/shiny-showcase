customUIelement <- function() {
  # try to syntax highlight 
  # https://stackoverflow.com/questions/47445260/how-to-enable-syntax-highlighting-in-r-shiny-app-with-htmloutput
  prismDependencies <- tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css"))
  prismRdependency <- tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
  
  tagList(prismDependencies,
          prismRdependency,
          wellPanel(class = "well.sm", style = "width: 100%",
                    fluidRow(
                      column(6,
                             actionLink("TOGGLE_CODE", "show/hide code")),
                      column(6,
                              actionLink("SHOW_README", "View Readme"))
                    )
          ))
}


customObserver <- function(input) {
  observe({
    code <- reactiveValues(not_rendered = TRUE)
    
    observeEvent(input$TOGGLE_CODE, {
      # shinyjs solution
      # shinyjs::toggleClass(class = "invisible", selector = ".shiny-code-container")
      
      # custom UI
      if(code$not_rendered) {
        insertUI(".container-fluid", where = "beforeEnd", ui = div(id = "showcaseInfo", showcaseAppCode()))
        insertUI(".container-fluid", where = "beforeEnd", ui =tags$head(tags$script("Prism.highlightAll()")))
      }
      else
        removeUI("#showcaseInfo")
      code$not_rendered <- !code$not_rendered
    })
    
    observeEvent(input$SHOW_README, {
      showModal(modalDialog(
        title = NULL,
        showcaseMetadata(),
        hr(),
        showcaseReadme(),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
}


#' Read Shiny app info
#' 
#' Evoke a bash scfript to read shinyAPp meta in given location
#' 
#' @examples
#' shinypath = "~/GitHub/shiny"
#' script = "~/GitHub/read_shiny_info.sh"
#' apps <- getShinyIndex()
#' apps
getShinyIndex <- function(shinypath = "~/GitHub/shiny", script = "~/GitHub/read_shiny_info.sh") {
  call <- paste(script, shinypath)
  json <- system(call, intern=TRUE)
  x <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  
  for(app in seq_along(x)) {
    desc <- x[[app]]$description
    x[[app]]$description <- read.dcf(textConnection(desc))
    names(x)[app] <- x[[app]]$name
    class(x[[app]]) <- "shiny_info"
  }
  structure(x, class = "shiny_index")
}

print.shiny_index <- function(x) 
  for(i in seq_along(x)) {
    cat("[",i, "] ", sep = "")
    print(x[[i]])
  }

print.shiny_info <- function(x, print = TRUE) {
  name <- x$name
  readme <- x$readme
  description <- sapply(colnames(x$description), function(d) paste0(d, ": ", x$description[1, d]))
  if(print) {
    cat(name, "\n", "Readme: ", substr(readme, 1, 25), "...\n", sep = "")
    for(d in description)
      cat(d, "\n")
    cat("\n")
  }
  invisible(list(name = name, readme = readme, description = description))
}


#' Shiny index to markdown
#' 
#' @examples 
#' apps <- getShinyIndex()
#' shinyIndex2markdown(apps)
#' 
shinyIndex2markdown <- function(x)
  paste(sapply(x, appInfo2markdown), collapse = "  \\")


#' App info 2 markdown
appInfo2markdown <- function(x) {
  name <- x$name
  desc <- x$description
  readme <- x$readme
  paste(name, 
        "{expand: more}", 
        "* Author:", getDescriptionField(desc, "Author"), 
        "* Email:", getDescriptionField(desc, "AuthorEmail"),
        "* Last updated:", getDescriptionField(desc, "Deployed"), 
        "* Git:", getDescriptionField(desc, "Git"), 
        "About",
        readme, 
        "{expand}",
        sep = "  \\")
}

#' Get a single description field from a description file by name
getDescriptionField <- function(x, field) {
  cols <- colnames(x)
  if(field %in% colnames(x))
    return(x[1, field])
  else return(NULL)
}