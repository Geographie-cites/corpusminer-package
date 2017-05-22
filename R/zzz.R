#' @import ggplot2
#' @importFrom igraph V V<- degree neighborhood simplify layout_in_circle
#' @importFrom igraph get.data.frame graph_from_data_frame get.edge.ids
#' @importFrom igraph E subgraph.edges head_of tail_of layout_as_tree
#' @importFrom igraph vcount ecount induced.subgraph
#' 
#' @importFrom dplyr filter mutate bind_rows %>% left_join group_by
#' @importFrom dplyr summarise summarise_at tbl_df do n arrange n_distinct
#' @importFrom dplyr select vars one_of rename mutate_each funs starts_with
#' @importFrom dplyr matches between distinct ungroup
#' 
#' @importFrom utils citation read.table read.csv
#' 
#' @importFrom stats complete.cases
#' 
#' @importFrom graphics axis barplot
#' 
#' @importFrom grDevices dev.off svg
#' 
#' @importFrom stringr str_sub
#' 
#' @importFrom lubridate parse_date_time
#' 
NULL

globalVariables( c(".", "X", "XVAL", "DIST", "name", "IDEGO") )
globalVariables( c("article_id", "pattern", "count", "nbauth") )
globalVariables( c("linknum", "kwcount", "id", "SCHID", "group") )
globalVariables( c("from", "to", "obsfreq", "theofreq", "relresid", "EXPECTED_WEIGHT") )
globalVariables( c("RESIDUALS", "firstauthor", "year") )
globalVariables( c("authors", "citedby", "citing", "ym", "sentence", "term") )
globalVariables( c("title", "title_en", "freq", "keyword", "word") )
globalVariables( c("cyb"))


#' network of keywords
"NETKW"

#' articles
#' 
#' @format data frame with columns
#' - id: article id
#' - date: date
#' - title: article title
#' - title_en: english title
#' - authors: authors
#' - langue: language
"articles"

#' Data about cybergeo articles
#' 
#' Used by the citation application
#' 
#' @format tibble with columns
#' - id cybergeo id of the article
#' - SCHID SCHID of the article
#' - title title
#' - title_en english title
#' - keywords_en english keywords
#' - keywords_fr french keywords
#' - authors autors, separated by ;
#' - date date
#' - langue language of the article
#' - translated 
#' - DatePublication
#' - Rubrique
#' - Auteur
#' - TypeDocument
#' - Disponibilite
#' - VisuTot
#' - Visu07
#' - Visu08
#' - Visu09
#' - Visu10
#' - Visu11
#' - Visu12 
#' - Visu13
#' - Visu14
#' - citedby
#' - citing
#' - kwcount
"citation_cybergeodata"


#' Metadata about articles from the neighborhood of cybergeo articles
#' 
#' @format data frame with columns
#' - id schid of the article
#' - title title
#' - year year
#' - cyb logical, TRUE if this is a cybergeo article
#' - keyword keywords
"citation_data"

#' citation edges
#' 
#' @format data frame with columns
#' - from schid of the article that cites
#' - to schid of the cited article
"citation_edges"

#' Metadata about keywords
#' 
#' @format data frame with columns
#' - word keyword
#' - freq frequency
#'- group semantic group
"citation_keyword_data"


#' geo semantic data
#'
#' used in the geo semantic module 
#'  
#' @format list with components "Keywords", "Citations" and "Semantic". 
#' Each of these is a list with components "Authoring" and "Studied". 
#' Each is a list with
#' - data. a data frame summarising keyword groups for each country
#' - dist. a distance matrix between countries
#' - hc. result of hclust 
#' - themes. List of themes
"geo_semantic_data"


#' data about articles used in the overview app
"overview_ARTICLES"

#' sentences data
#' 
#' @format data frame with columns:
#' - article_id: id of the article
#' - sentence: sentence
#' - id: id of the sentence within the article
"sentences"


#' terms data
#' 
#' @format data frame with columns
#' - id id of the term
#' - article_id cybergeo id of the article
#' - term word
#' - count number of time the term occurs in the article
"terms"


#' world map data
#' 
#' @format SpatialPolygonsDataFrame object
"world"

