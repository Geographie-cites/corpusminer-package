% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\docType{data}
\name{geo_semantic_data}
\alias{geo_semantic_data}
\title{geo semantic data}
\format{list with components "Keywords", "Citations" and "Semantic". 
Each of these is a list with components "Authoring" and "Studied". 
Each is a list with
- data. a data frame summarising keyword groups for each country
- dist. a distance matrix between countries
- hc. result of hclust 
- themes. List of themes}
\usage{
geo_semantic_data
}
\description{
used in the geo semantic module
}
\keyword{datasets}
