
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
