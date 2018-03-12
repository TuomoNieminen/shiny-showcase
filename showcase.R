# This is a custom shiny:showcase definition


# Returns tags containing the application metadata (title and author) in
# showcase mode.
appMetadata <- function(desc) {
  cols <- colnames(desc)
  if ("Title" %in% cols)
    with(tags, h4(class="text-muted shiny-showcase-apptitle", desc[1,"Title"],
                  if ("Author" %in% cols) small(
                    br(), "by",
                    if ("AuthorUrl" %in% cols)
                      a(href=desc[1,"AuthorUrl"], class="shiny-showcase-appauthor",
                        desc[1,"Author"])
                    else
                      desc[1,"Author"],
                    if ("AuthorEmail" %in% cols)
                      a(href=paste("mailto:", desc[1,"AuthorEmail"], sep = ''),
                        class="shiny-showcase-appauthoreemail",
                        desc[1,"AuthorEmail"])
                    else "")
                  else ""))
  else ""
}

navTabsHelper <- function(files, prefix = "") {
  lapply(files, function(file) {
    with(tags,
         li(class=if (tolower(file) %in% c("app.r", "server.r")) "active" else "",
            a(href=paste("#", gsub(".", "_", file, fixed=TRUE), "_code", sep=""),
              "data-toggle"="tab", paste0(prefix, file)))
    )
  })
}

navTabsDropdown <- function(files) {
  if (length(files) > 0) {
    with(tags,
         li(role="presentation", class="dropdown",
            a(class="dropdown-toggle", `data-toggle`="dropdown", href="#",
              role="button", `aria-haspopup`="true", `aria-expanded`="false",
              "www", span(class="caret")
            ),
            ul(class="dropdown-menu", navTabsHelper(files))
         )
    )
  }
}

tabContentHelper <- function(files, path, language) {
  lapply(files, function(file) {
    with(tags,
         div(class=paste("tab-pane",
                         if (tolower(file) %in% c("app.r", "server.r")) " active"
                         else "",
                         sep=""),
             id=paste(gsub(".", "_", file, fixed=TRUE),
                      "_code", sep=""),
             pre(class="shiny-code",
                 # we need to prevent the indentation of <code> ... </code>
                 HTML(format(tags$code(
                   class=paste0("language-", language),
                   paste(shiny:::readUTF8(shiny:::file.path.ci(path, file)), collapse="\n")
                 ), indent = FALSE))))
    )
  })
}

# Returns tags containing the application's code in Bootstrap-style tabs in
# showcase mode.
showcaseCodeTabs <- function(#codelicense
  ){
  rFiles <- list.files(pattern = "\\.[rR]$")
  wwwFiles <- list()
  if (isTRUE(shiny:::.globals$IncludeWWW)) {
    path <- file.path(getwd(), "www")
    wwwFiles$jsFiles <- list.files(path, pattern = "\\.js$")
    wwwFiles$cssFiles <- list.files(path, pattern = "\\.css$")
    wwwFiles$htmlFiles <- list.files(path, pattern = "\\.html$")
  }
  with(tags, div(id="showcase-code-tabs",
                 # a(id="showcase-code-position-toggle",
                 #   class="btn btn-default btn-sm",
                 #   onclick="toggleCodePosition()",
                 #   icon("level-up"),
                 #   "show with app"),
                 ul(class="nav nav-tabs",
                    navTabsHelper(rFiles),
                    navTabsDropdown(unlist(wwwFiles))
                 ),
                 div(class="tab-content", id="showcase-code-content",
                     tabContentHelper(rFiles, path = getwd(), language = "r"),
                     tabContentHelper(wwwFiles$jsFiles,
                                      path = paste0(getwd(), "/www"),
                                      language = "javascript"),
                     tabContentHelper(wwwFiles$cssFiles,
                                      path = paste0(getwd(), "/www"),
                                      language = "css"),
                     tabContentHelper(wwwFiles$htmlFiles,
                                      path = paste0(getwd(), "/www"),
                                      language = "xml")
                 )
                 #,
                 #codeLicense
                 ))
}

# Returns tags containing the showcase application information (readme and
# code).
showcaseAppInfo <- function() {
  # descfile <- shiny:::file.path.ci(getwd(), "DESCRIPTION")
  # hasDesc <- file.exists(descfile)
  # readmemd <- shiny:::file.path.ci(getwd(), "Readme.md")
  # hasReadme <- file.exists(readmemd)
  # if (hasDesc) {
  #   con <- textConnection(shiny:::readUTF8(descfile))
  #   on.exit(close(con), add = TRUE)
  #   desc <- read.dcf(con)
  # }
  with(tags,
       div(class="container-fluid shiny-code-container well",
           id="showcase-well",
           div(class="row",
               # if (hasDesc || hasReadme) {
               #   div(id="showcase-app-metadata", class="col-sm-4",
               #       if (hasDesc) appMetadata(desc) else "",
               #       if (hasReadme) div(id="readme-md"))
               # } else "",
               div(id="showcase-code-inline",
                   class="col-sm-10 col-sm-offset-1",
                   showcaseCodeTabs(
                     # if (hasDesc && "License" %in% colnames(desc)) {
                     #   small(class="showcase-code-license text-muted",
                     #         "Code license: ",
                     #         licenseLink(desc[1,"License"]))
                     # } else ""
                     )))))
}


