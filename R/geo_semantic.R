

#' hierarchical clustering of countries
#'
#' This function produces a hierarchical clustering of countries with respect
#' to their frequency of themes it is used in the geosemantic tab to display
#' the groups of countries by themes and the corresponding average profiles of themes
#'
#' @param themes_By_country_bf dataframe in which lines represent country codes and
#'        columns represent the number of articles for each theme
#' @param numberOfGroups an integer giving the number of classes for the clustering
#' @param themes the list of themes of the analysis
#' @param groups_Country a vector of group IDs for each country
#'
#' @importFrom stats dist hclust cutree
#' @export
cahCountriesBasedOnTerms = function(themes_By_country_bf, numberOfGroups, themes){
  themesScaled <- scale(themes_By_country_bf[,themes])
  rownames(themesScaled) <- themes_By_country_bf[,1]
  d.themes <- dist(themesScaled)
  cah.themes <- hclust(d.themes, method = "ward.D2")
  cutree(cah.themes, k=numberOfGroups)
}

#' Aggregate countries based on terms
#'
#' This function summarises the number of articles by theme for each country it is used
#' in the reactive object 'clusterCountries' for every analysis at the country level
#'
#' @param themesFile a dataframe in which lines represent articles and columns include themes and country codes
#' @param themes the list of themes of the analysis
#' @param countries_to_aggregate the list of countries to aggregate articles by (taken from the shapeFile)
#'
#' @return a dataframe in which lines represent country codes and columns represent the number of articles for each theme
#' @export
aggregateCountriesBasedOnTerms <- function(themesFile, themes, countries_to_aggregate){

  themes_By_country_bf <- data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] <- NA
  themes_By_country_bf$n <- NA
  for (c in countries_to_aggregate){
    articles_to_aggregate <- themesFile[themesFile[,c] == 1, themes]
    if (nrow(articles_to_aggregate)){
      nArticles <- dim(articles_to_aggregate)[1]
      themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] <- colSums(articles_to_aggregate) / nArticles
      themes_By_country_bf[themes_By_country_bf$CountryID == c, "n"] <- nArticles
    }
  }

  themes_By_country_bf <- themes_By_country_bf[complete.cases(themes_By_country_bf),]
  themes_By_country_bf$CountryID <- substr(themes_By_country_bf$CountryID, 3,4)

  themes_By_country_bf
}


#' Computes the average frquencies of themes by cah group
#'
#' it is used in the legend of the cah map
#'
#' @param x a dataframe of theme frequency by country (themes_By_country_bf)
#'          in which lines represent country codes and columns represent
#'          the number of articles for each themes
#' @param y a vector of group numbers the length of the dataframe rows
#' @return a dataframe in which lines represent cah groups of country and columns represent the frequency of articles for each theme
#' @export
stat.comp <- function(x, y){
  K <- length(unique(y))
  n <- length(x)
  m <- mean(x)
  TSS <- sum((x-m)^2)
  nk <- table(y)
  mk <- tapply(x,y,mean)
  BSS <- sum(nk* (mk-m)^2)
  result <- c(mk,100.0*BSS/TSS)
  names(result) <- c( paste("G",1:K),"% epl.")
  return(result)
}

load_geo_semantic_data <- function(terms_file, articles, countries){

  terms <- read.table( terms_file, sep = ",", dec = ".")

  A <- paste0( "A_", countries )
  S <- paste0( "S_", countries )

  data <- left_join(
      terms,
      select( articles, id, firstauthor, one_of(A), one_of(S)),
      by = "id"
    ) %>%
    filter( complete.cases(.) ) %>%
    mutate( firstauthor = as.character(firstauthor))

  themes <- setdiff( names(terms), "id")

  summary_terms <- function( data, themes, variables ){
    agg  <- aggregateCountriesBasedOnTerms( data, themes, variables)
    d <- dist( scale(agg[,-1]) )
    hc <- hclust( d, method = "ward.D2")

    list( data = agg, dist = d, hc = hc, themes = themes )
  }

  list(
    Authoring = summary_terms( data, themes, A),
    Studied   = summary_terms( data, themes, S)
  )
}
