
## semantic network



#'
#' @title Semantic Network construction
#' @name constructSemanticNetwork
#' @description Construct semantic coocurrence graph directly from nw table in mongo. Kw dico is reconstructed here (not that efficient in R)
#' 
constructSemanticNetwork<-function(relevantcollection,kwcollection,nwcollection,edge_th,target,mongohost){
  mongo <- mongo.create(host=mongohost)
  # 
  relevant <- mongo.find.all(mongo,relevantcollection)
  dico <- mongo.find.all(mongo,kwcollection)
  
  relevant = data.frame(keyword=sapply(relevant,function(d){d$keyword}),
                        cumtermhood=sapply(relevant,function(d){d$cumtermhood}),
                        docfreq=sapply(relevant,function(d){d$docfrequency}),
                        tidf=sapply(relevant,function(d){d$tidf})
  )
  
  srel = as.tbl(relevant)
  srel$keyword = as.character(srel$keyword)
  
  rel = list()
  for(i in 1:length(srel$keyword)){rel[[srel$keyword[i]]]=i}
  
  # construct kw dico : ID -> keywords
  keyword_dico = list()
  for(i in 1:length(dico)){
    if(i%%100==0){show(paste0('dico : ',i/length(dico),'%'))}
    #kws = unique(dico[[i]]$keywords)
    kws = dico[[i]]$keywords
    #show(kws)
    if(length(kws)>0){
      #kws = kws[sapply(kws,function(w){w %in% srel$keyword})]
      keyword_dico[[dico[[i]]$id]]=kws
    }
  }
  
  # construct now edge dataframe
  edges <- mongo.find.all(mongo,nwcollection)
  e1=c();e2=c();weights=c()
  for(i in 1:length(edges)){
    if(i%%1000==0){show(paste0('edges : ',i/length(edges),'%'))}
    w=edges[[i]]$weight
    if(w>=edge_th){
      e = strsplit(edges[[i]]$edge,";")[[1]]
      if(e[1]!=e[2]){# avoid self loops, weight info is already contained in doc frequency of nodes
        e1=append(e1,e[1]);e2=append(e2,e[2]);weights=append(weights,w)
      }
    }
  }
  
  res = list()
  res$g = graph_from_data_frame(data.frame(from=e1,to=e2,weight=weights),directed=FALSE,vertices = relevant)
  res$keyword_dico=keyword_dico
  
  save(res,file=paste0(target,'.RData'))
  
}







#'
#' @title Graph Filtering
#' @name filterGraph
#' @description filter nodes : grep -v -f file for nodes names
filterGraph<-function(graph,file){
  words<-unlist(read.csv(file,stringsAsFactors=FALSE,header=FALSE))
  g=graph
  for(w in 1:length(words)){
    g=induced.subgraph(g,which(V(g)$name!=words[w]))
  }
  return(g)
}


