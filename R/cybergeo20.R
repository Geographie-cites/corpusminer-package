
#' Launch cybergeo 20 app
#' 
#' @param \dots forwarded to \code{\link[shiny]{runApp}}
cybergeo20 <- function(...){
  path <- system.file( "shiny", package = "corpusminer")
  runApp( path, ... )  
}
