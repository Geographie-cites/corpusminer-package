##############################
# Shiny App: Cybergeo20
# Packages and functions
##############################

# load packages ----

# library(shiny)
# library(rgdal)
# # library(plyr)
# library(mapproj)
# library(maptools)
# library(RColorBrewer)
# library(RCurl)
# library(ggplot2)
# library(reshape2)
# library(grid)
# library(igraph)
# library(dplyr)
# library(RSQLite)
# library(svgPanZoom)
# library(wordcloud)
# library(scales)
# library(lubridate)
# library(stringr)

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

# #'
# #'  sqlite connection : citation nw
# citationdbcit = dbConnect(SQLite(),"data/CitationNetwork.sqlite3")
#
# #'
# #'  sqlite connection : keywords
# citationdbkws = dbConnect(SQLite(),"data/CitationKeywords.sqlite3")
