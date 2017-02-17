# functions used in Overview app

#' @importFrom dplyr mutate between select
#' @importFrom tibble data_frame
#' @importFrom magrittr %>%
#' @importFrom stringr str_count
#' @export
subset_articles <- function( articles, year_range ){
  articles %>%
    select( id, year, authors, citedby, citing, matches( "^._" ) ) %>%
    filter( between( year, year_range[1], year_range[2]) ) %>%
    mutate( nauthors = 1 + str_count(authors, "," ) )
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
#' @importFrom graphics par title
#' @importMethodsFrom sp plot
#' @export
plot_overview_map <- function( world, articles, years, indicator = c("A", "S", "L") ){

  indicator <- match.arg(indicator)

  countries <- as.character( world@data$CNTR_ID )
  columns <- paste( indicator, countries, sep = "_")

  # data for the chosen indicator
  counts <-  colSums( articles[, columns ] )

  # title and colors
  plot_title <- paste( overview_plot_prefix[indicator], pretty_years_interval(years), sep = " | ")
  col  <- ifelse( counts > 0, overview_plot_col[indicator], "lightgrey")

  # plot
  par( bg = "#2b3e50", mar = c(0,0,1,0) )
  plot( world, col = col, border = "white", lwd = 0.7)
  title( plot_title, col.main = "white")
}

#' @importFrom utils head
overview_summary_country <- function(x){
  best <- head( sort(x, decreasing = TRUE), 5L)
  paste( substr( names(best), 3, 5 ), " (", best, ")", collapse = ", ", sep = "")
}

#' @importFrom dplyr select starts_with
#' @importFrom tibble data_frame
overview_stats <- function( data ){

  nPapers <- nrow(data)
  nAuthors <- sum(data$nauthors)

  authoring <- colSums( select(data, starts_with("A_")) )
  nAuthoringCountries <- sum( authoring > 0)
  AC5 <- overview_summary_country( authoring )

  studied <- colSums( select(data, starts_with("S_")) )
  nStudiedCountries <- sum( studied > 0)
  SC5 <- overview_summary_country( studied )

  citedby <- sum(data$citedby, na.rm = TRUE)
  citing <- sum(data$citing, na.rm = TRUE)

  data_frame(
    Indicator = c("Number of scientific articles", "Number of authors", "Number of countries authoring",
      "Top countries authoring", "Number of countries studied", "Top countries studied",
      "Number of citations from other articles", "Number of citations of other articles"
    ),
    Value = c(nPapers, nAuthors, nAuthoringCountries, AC5, nStudiedCountries, SC5, citedby, citing)
  )

}
