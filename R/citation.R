#' load citation edges given an reference id
#'
#' @param con database connection
#' @param id edge id
#' 
#' @importMethodsFrom DBI dbGetQuery
citationLoadEdges <- function(con, id){
  query <- sprintf( "SELECT * FROM edges WHERE `from`='%s' OR `to`='%s' ;", id, id )
  dbGetQuery(con,query)
}

#' load neighbors keywords given an id
#'
#' @param con_citation connection to the citation db
#' @param con_keywords connection to the keywords db
#' @param id article id
#' 
#' @return keywords
#' @importMethodsFrom DBI dbGetQuery
citationLoadKeywords <- function(con_citation, con_keywords, id){
  # load edges
  to_query   <- sprintf( "SELECT `to` FROM edges WHERE `from`='%s' ;", id )
  to_id      <- dbGetQuery(con_citation,to_query)[,1]
  
  from_query <- sprintf( "SELECT `from` FROM edges WHERE `to`='%s' ;", id )
  from_id    <- dbGetQuery(con_citation,from_query)[,1]
  
  ids <- c(id, to_id, from_id)
  
  query <- paste( 
    "SELECT * FROM keywords WHERE ", 
    paste( "`id`='", ids, "'", sep = "", collapse = " OR " )
  )
  res <- dbGetQuery(con_keywords, query)
  keywords <- strsplit( res$keywords, ";")
  names(keywords) <- res$id
  keywords
}

#' visualize an ego network given edges
#' @importFrom igraph V E head_of tail_of  graph_from_data_frame 
#' @importMethodsFrom igraph plot
#' @export
citationVisuEgo <- function(edges){
  if(!is.null(edges)){
    if(nrow(edges)>0){
      citsubgraph <- graph_from_data_frame(edges,directed=TRUE)
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$cyb   <- E(citsubgraph)$fromcyb
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$cyb   <- E(citsubgraph)$tocyb
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$title <- E(citsubgraph)$fromtitle
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$title <- E(citsubgraph)$totitle
      lay <- layout_as_tree(citsubgraph,circular=FALSE)
      lay[lay[,2]==0,2]  <- -sample.int(length(which(lay[,2]==0)),replace=FALSE)-2
      #lay[lay[,2]==2,1] <-  sample.int(10,size=length(which(lay[,2]==2)))-5#((-length(which(lay[,2]==2))/2):(length(which(lay[,2]==2))/2))*5/length(which(lay[,2]==2))
      lay[lay[,2]==2,1]  <-  sample.int(length(which(lay[,2]==2)))-5
      lay[lay[,2]==2,2]  <- 4+sample.int(length(which(lay[,2]==2)),replace=FALSE)
      palette <- c("#df691a","#1C6F91")
      par(bg = "#4e5d6c")
      plot(citsubgraph,
           edge.color="#df691a",
           edge.arrow.size = 1,
           vertex.label=V(citsubgraph)$title,
           vertex.color=palette[V(citsubgraph)$cyb+1],
           vertex.frame.color="#1C6F91",
           vertex.label.color = "#ebebeb",
           layout=lay
      )
    }
  }
}

#' plots word clouds, one for the keywords of the ref itself, the other for the provided keywords (neighborhood)
#' 
#' @importFrom graphics par
#' @importFrom wordcloud wordcloud
citationWordclouds<-function(id, keywords){
  if(id != "0" && !is.null(keywords)){
    # at least kws for the paper, so no need to check emptyness
    par(mfrow=c(1,2), bg = "#4e5d6c")
    wordcloud(words=keywords[[id]],
              freq=citationkwfreqs[keywords[[id]]],
              colors=unlist(semanticcolors[citationkwthemdico[keywords[[id]]]]),
              ordered.colors = TRUE
    )
    
    allkws=unlist(keywords)
    wordcloud(words=allkws,
              freq=citationkwfreqs[allkws],
              colors=unlist(semanticcolors[citationkwthemdico[allkws]]),
              ordered.colors = TRUE
    )
  }
}
