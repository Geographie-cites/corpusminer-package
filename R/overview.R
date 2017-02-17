# functions used in Overview app

#' @importFrom dplyr mutate between
#' @importFrom tibble data_frame
#' @export
subset_articles <- function( articles, year_range ){
   filter( articles, between( year, year_range[1], year_range[2]) )
}

pretty_years_interval <- function( years ){
  if( years[1] == years[2] ){
    years[1]
  } else {
    paste( years[1], years[2], sep = " - ")
  }
}

# constants used by overview app
overview_plot_prefix <- c(
  "A" = "Countries authoring Cybergeo articles",
  "S" = "Countries studied in Cybergeo articles",
  "L" = "Countries studied by locals in Cybergeo articles"
)
overview_plot_col <- c(A = "orange", S = "#1C6F91", L = "#df691a")

#' Overview map plot
#' 
#' @param world 
#' @param articles 
#' @param years 
#' @param indicator 
#'
#' @importFrom dplyr select starts_with
#' @importFrom graphics par plot title
#' @export
plot_overview_map <- function( world, articles, years, indicator = c("A", "S", "L") ){

  indicator <- match.arg(indicator)
  prefix <- paste0( indicator , "_" )

  # data for the chosen indicator
  data_indicator <- select( articles, starts_with(prefix) )

  # extract the countries
  countries <- substr( names(data_indicator), 1, 3)
  counts <- colSums(data_indicator)

  # reorder counts to match the data in the world map
  # maybe not useful
  counts <- counts[ match(countries, world$CNTR_ID) ]

  plot_title <- paste( overview_plot_prefix[indicator], pretty_years_interval(years), sep = " | ")
  col  <- ifelse( counts > 0, overview_plot_col[indicator], "lightgrey")

  par( bg = "#2b3e50", mar = c(0,0,1,0) )
  plot( world, col = col, border = "white", lwd = 0.7)
  title( plot_title, col.main = "white")


}
