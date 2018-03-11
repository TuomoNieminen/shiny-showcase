#' From R.utils 
#' https://github.com/HenrikBengtsson/R.utils/blob/feeab63c4aa99a0b9a5e7e887c20585029d9514d/R/reassignInPackage.R
#' 
#' @RdocDefault reassignInPackage
#'
#' @title "Re-assigns a new value to an existing object in a loaded package"
#'
#' \description{
#'  @get "title".
#' }
#' 
#' @synopsis 
#'
#' \arguments{
#'   \item{name}{The name of the object to be replaced."}
#'  \item{pkgName}{The name of the package where the object lives."}
#'   \item{value}{The new value to be assigned.}
#'   \item{keepOld}{If @TRUE, the old value is kept as attribute
#'     \code{oldValue} in the new object.}
#'   \item{...}{Not used.}
#' }
#'
#' \value{
#'   Returns (invisibly) the new object.
#' }
#'
#' @author
#'
#' \seealso{
#'   See \code{assignInNamespace()} in @see "utils::getFromNamespace".
#' }
#' 
reassignInPackage <-  function(name, pkgName, value, keepOld=TRUE, ...) {
  # Get the environment where to look for the function to replace
  envName <- sprintf("package:%s", pkgName);
  if (!envName %in% search())
    throw("Package not loaded: ", pkgName);
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Patch
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get the object to be replaced
  
  # Workaround for the fact that getAnywhere() is not accepting a string!  
  expr <- substitute(getAnywhere(name), list(name=name));
  obj <- eval(expr);
  
  pos <- which(obj$where == sprintf("namespace:%s", pkgName));
  if (length(pos) == 0) {
    throw("Argument 'name' does not refer to an existing object: ", name);
  }
  oldValue <- obj$objs[[pos]];
  
  # Get environment of this object
  env <- environment(oldValue);
  
  # Assign this environment to the new object
  environment(value) <- env;
  
  # Keep the old value?
  if (keepOld)
    attr(value, "oldValue") <- oldValue;
  
  unlockBindingT <- base::unlockBinding;
  unlockBindingT(name, env);
  assignInNamespaceT <- utils::assignInNamespace;
  assignInNamespaceT(name, value, ns=pkgName, envir=env);
  assign(name, value, envir=env);
  lockBinding(name, env);
  
  invisible(value);
}

#' THL replacement for shiny:appMetadata
#' 
#' This function is used to REPLACE the shiny AppMetadata function in the shiny NAMESPACE
#' using reassignInPackage. Note that this is obviously not good practise in general, but was deemed 
#' reasonable here since we're only replacsing a small helper function (selecting fields from a 
#' DESCRIPTIN file and creating html nodes to display in the app in showcase mode)
#' 
#' @examples 
#' reassignInPackage(name = "appMetadata", pkgName = "shiny", value = THLappMetadata)
#' 
THLappMetadata <- function(desc) {
  res <- ""
  try(cols <- colnames(desc))
  # this is the original part of appMetadata, taken from
  # https://github.com/rstudio/shiny/blob/master/R/showcase.R
  res <- tryCatch({
    if ("Title" %in% cols)
      res <- with(tags, 
                  h4(class="text-muted shiny-showcase-apptitle", desc[1,"Title"],
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
                     else "")
      )}, error = function(e) {
        warning("The  original part of appMetadata func resulted in error. Cannot parse DESCRIPTION file.")
        return(res)
      })
  
  # These are THL additions
  res <- tryCatch({
    if("Git" %in% cols) {
      div(res, p(paste("Git:", desc[1, "Git"])))}},
    error = {
      warning("the thl addition to appMetadata resulted in error. Cannot parse DESCRIPTION file.")
      return(res)})
  res
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