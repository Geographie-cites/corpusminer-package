
#' Launch cybergeo 20 app
#'
#' @param \dots forwarded to \code{\link[shiny]{runApp}}
#' @importFrom shiny runApp
#' @export
cybergeo20 <- function(...){
  path <- system.file( "shiny", package = "corpusminer")
  runApp( path, ... )
}
