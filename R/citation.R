#' load citation edges given an reference id
citationLoadEdges<-function(id){
  res=data.frame()
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `from`='",id,"';")))
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `to`='",id,"';")))
  return(res)
}

#' @name  citationLoadKeywords
#' @description load neighbors keywords given an id
citationLoadKeywords<-function(id){
  # load edges
  toids=dbGetQuery(citationdbcit,paste0("SELECT `to` FROM edges WHERE `from`='",id,"';"))[,1]
  fromids=dbGetQuery(citationdbcit,paste0("SELECT `from` FROM edges WHERE `to`='",id,"';"))[,1]
  ids=c(id,toids,fromids)
  req = "SELECT * FROM keywords WHERE "
  for(i in ids[1:(length(ids)-1)]){req=paste0(req,"`id`='",i,"' OR ")}
  req=paste0(req,"`id`='",ids[length(ids)],"';")
  res=dbGetQuery(citationdbkws,req)
  l = sapply(res$keywords,function(s){strsplit(s,";")})
  names(l)<-res$id
  return(l)
}



#'
#' @name citationVisuEgo
#' @description visualize an ego network given edges
citationVisuEgo<-function(edges){
  if(!is.null(edges)){
    if(nrow(edges)>0){
      citsubgraph = graph_from_data_frame(edges,directed=TRUE)
      #show(citsubgraph)
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$fromcyb
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$tocyb
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$fromtitle
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$totitle
      lay=layout_as_tree(citsubgraph,circular=FALSE)
      lay[lay[,2]==0,2]=-sample.int(length(which(lay[,2]==0)),replace=FALSE)-2
      #lay[lay[,2]==2,1]= sample.int(10,size=length(which(lay[,2]==2)))-5#((-length(which(lay[,2]==2))/2):(length(which(lay[,2]==2))/2))*5/length(which(lay[,2]==2))
      lay[lay[,2]==2,1]= sample.int(length(which(lay[,2]==2)))-5
      lay[lay[,2]==2,2]=4+sample.int(length(which(lay[,2]==2)),replace=FALSE)
      palette=c("#df691a","#1C6F91")
      par(bg = "#4e5d6c")
      plot(citsubgraph,edge.color="#df691a",edge.arrow.size = 1,
           vertex.label=V(citsubgraph)$title,vertex.color=palette[V(citsubgraph)$cyb+1],
           vertex.frame.color="#1C6F91",vertex.label.color = "#ebebeb",
           layout=lay
      )
      #16283 22232 23337 23502 26325 24841 26026 22270 24798 25354 26969
    }
  }
}





#'
#' @name citationWordclouds
#' @description plots word clouds, one for the keywords of the ref itself, the other for the provided keywords (neighborhood)
citationWordclouds<-function(id,keywords){
  if(id!="0"&!is.null(keywords)){
    # at least kws for the paper, so no need to check emptyness
    par(mfrow=c(1,2))
    par(bg = "#4e5d6c")
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
