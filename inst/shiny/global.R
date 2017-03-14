##############################
# Shiny App: Cybergeo20
# Packages and functions
##############################

# load packages ----

library(shiny)
library(rgdal)
# library(plyr)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(igraph)
library(dplyr)
library(RSQLite)
library(svgPanZoom)
library(wordcloud)
library(scales)
library(lubridate)
library(stringr)


# load data ----

#### Geographical data

countries = as.character(world@data$CNTR_ID)
geo_semantic_data <- list(
  Keywords  = load_geo_semantic_data( "app_data/keyword_themes.csv", ARTICLES, countries ),
  Citations = load_geo_semantic_data( "app_data/citation_themes.csv", ARTICLES, countries ),
  Semantic  = load_geo_semantic_data( "app_data/semantic_themes.csv", ARTICLES, countries )
)

######## PO :
##   Regexp terms in full textes

#-- Loading data --------------------------------------------------------------

# Read the terms dataframe
terms <- read.table(
  "data/terms.csv",
  sep = ";",
  quote = "",
  comment.char = "",
  header = TRUE,
  stringsAsFactors = FALSE
) %>%
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  ) %>%
  dplyr::select(id, article_id, term, count)

# Read the sentences dataframe
sentences <- read.table(
  "data/sentences.csv",
  sep = "|",
  quote = "",
  comment.char = "",
  header = TRUE,
  stringsAsFactors=FALSE
) %>%
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  )

# Read the metadata of articles
articles <- read.table(
  "data/cybergeo.csv",
  sep = ",",
  quote = "\"",
  comment.char = "",
  header = TRUE
) %>%
  tbl_df() %>%
  dplyr::rename(titre = title_en, auteurs = authors) %>%
  dplyr::mutate(citation = paste(sep = ". ", auteurs, substr(date,1,4), titre)) %>%
  dplyr::select(id, date, citation, langue)



####################
### Juste ---
#
#  --  Archi for cit. nw exploration  --
#
#   - data/semanticnw.RData is not loaded as huge ; replaced by sqlite
#    -> for performance, can be fully loaded is speed is prefered over memory
#   - load datatable for cybergeo articles ; request in local sqlite db for connections
#   - get the ego nw, and display info for neighbors
#   - display semantic info : keywords, corresponding communities.
#   - one tab with sem nw visu : svg viz
#
#


##
#  Notations / id conventions : vars and ids prefixed with "citation"

#' ---- DATA ----

#'
#'  citation nw cybergeo table
load('data/citation_cybergeodata.RData')

#'
#'  kws domains dico
load('data/citation_kwthemdico.RData')

#'
#'  sqlite connection : citation nw
citationdbcit = dbConnect(SQLite(),"data/CitationNetwork.sqlite3")

#'
#'  sqlite connection : keywords
citationdbkws = dbConnect(SQLite(),"data/CitationKeywords.sqlite3")
