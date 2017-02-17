
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
#' @importFrom stats dist hclust
#' @export
cahCountriesBasedOnTerms = function(themes_By_country_bf, numberOfGroups, themes){
  themesScaled <- scale(themes_By_country_bf[,themes])
  rownames(themesScaled) <- themes_By_country_bf[,1]
  d.themes <- dist(themesScaled)
  cah.themes <- hclust(d.themes, method = "ward.D2")
  groups_Country <- cutree(cah.themes, k=numberOfGroups)
  groups_Country
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
aggregateCountriesBasedOnTerms = function(themesFile, themes, countries_to_aggregate){
  themes_By_country_bf <- data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] <- NA
  themes_By_country_bf$n <- NA
  
  for (c in countries_to_aggregate){
    articles_to_aggregate <- themesFile[themesFile[,c] == 1,themes]
    if (!is.null(articles_to_aggregate)){
      nArticles <- dim(articles_to_aggregate)[1]
      themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] <- colSums(articles_to_aggregate) / nArticles
      themes_By_country_bf[themes_By_country_bf$CountryID == c, "n"] <- nArticles
    }
  }
  
  themes_By_country_bf <- themes_By_country_bf[complete.cases(themes_By_country_bf),]
  themes_By_country_bf$CountryID <- substr(themes_By_country_bf$CountryID, 3,4)
  
  themes_By_country_bf
}
