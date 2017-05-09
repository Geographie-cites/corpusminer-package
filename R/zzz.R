#' @import leaflet
#' @import ggplot2
#' @import svgPanZoom
#' @import RSQLite
#' @importFrom igraph V V<- degree neighborhood simplify layout_in_circle
#' @importFrom igraph get.data.frame graph_from_data_frame get.edge.ids
#' @importFrom igraph E subgraph.edges head_of tail_of layout_as_tree
#' @importFrom igraph vcount ecount induced.subgraph
#' @importFrom dplyr filter mutate bind_rows %>% left_join group_by
#' @importFrom dplyr summarise summarise_at tbl_df do n arrange n_distinct
#' @importFrom dplyr select vars one_of rename mutate_each funs starts_with
#' @importFrom dplyr matches between distinct ungroup
#' @importFrom utils citation read.table read.csv
#' @importFrom stats complete.cases
#' @importFrom graphics axis barplot
#' @importFrom grDevices dev.off svg
#' @importFrom stringr str_sub
#' @importFrom lubridate parse_date_time
NULL

globalVariables( c(".", "X", "XVAL", "DIST", "name", "IDEGO") )
globalVariables( c("article_id", "pattern", "count", "nbauth") )
globalVariables( c("linknum", "kwcount", "id", "SCHID", "group") )
globalVariables( c("from", "to", "obsfreq", "theofreq", "relresid", "EXPECTED_WEIGHT") )
globalVariables( c("RESIDUALS", "firstauthor", "year") )
globalVariables( c("authors", "citedby", "citing", "ym", "sentence", "term") )

#' @export
"pattern_list"
