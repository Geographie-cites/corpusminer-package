# functions used in Overview app

#' @importFrom dplyr mutate between
#' @export
subset_articles <- function( articles, year_range ){
   filter( articles, between( year, year_range[1], year_range[2]) )
}
