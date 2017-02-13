
#'
#' @title Semantic Network construction
#' @name constructSemanticNetwork
#' @description Construct semantic coocurrence graph directly from nw table in mongo. 
#'    Kw dico is reconstructed here (not that efficient in R)
#' @param relevantcollection : name of the collection containing relevant keywords
#' @param kwcollection : name of the collection containing keywords
#' @param nwcollection : name of collection containing all edges
#' @param edge_th : threshold for edge selection
#' @param target : name of output file
#' @param mongohost : adress of mongodb
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



#'
#' @title Community Extraction
#' @name extractSubGraphCommunities
#' @description Filter a graph on degree (kmin,kmax), document frequency (freqmin,freqmax) and co-occurence (edge_th)
#'  and computes optimal communities
#'  @param ggiant igraph; graph
#'  @param kmin ; minimal filtering degree
#'  @param kmax ; maximal filtering degree
#'  @param freqmin ; minimal filtering frequency
#'  @param freqmax ; maximal filtering frequency
#'  @param edge_th ; edge weight threshold
#'  
extractSubGraphCommunities<-function(ggiant,kmin,kmax,freqmin,freqmax,edge_th){
  dd = V(ggiant)$docfreq
  d = degree(ggiant)
  gg=induced_subgraph(ggiant,which(d>kmin&d<kmax&dd>freqmin&dd<freqmax))
  gg=subgraph.edges(gg,which(E(gg)$weight>edge_th))
  clust = clusters(gg);cmax = which(clust$csize==max(clust$csize))
  gg = induced.subgraph(gg,which(clust$membership==cmax))
  com = cluster_louvain(gg)
  return(list(gg=gg,com=com))
}




#'
#' @description Summary of a subgraph
summarySubGraphCommunities<-function(sub){
  gg=sub$gg;com=sub$com
  show(paste0('Vertices : ',length(V(gg))))
  show(paste0('Communities : ',length(sizes(com))))
  show(paste0('Modularity : ',modularity(com)))
  show(paste0('Balance : ',sum((sizes(com)/length(V(gg)))^2)))
  show(sizes(com))
}






#'
#'  @name computeThemProbas
#'  @description Compute thematic probability matrix
computeThemProbas<-function(gg,com,keyword_dico){
  # construct kw -> thematic dico
  thematics = list()
  for(i in 1:length(V(gg))){
    thematics[[V(gg)$name[i]]]=com$membership[i]
  }
  
  them_probas = matrix(0,length(names(keyword_dico)),length(unique(com$membership)))
  for(i in 1:length(names(keyword_dico))){
    if(i%%100==0){show(i)}
    kwcount=0
    for(kw in keyword_dico[[names(keyword_dico)[i]]]){if(kw %in% names(thematics)){
      j=thematics[[kw]]
      them_probas[i,j]=them_probas[i,j]+1;kwcount=kwcount+1
    }}
    if(kwcount>0){them_probas[i,]=them_probas[i,]/kwcount}
  }
  return(them_probas)
}






#'
#' @title Network sensitivity analysis
#' @name networkSensitivity
#' @description Network measures for filtering parameter ranges
#'
networkSensitivity <- function(db,filters,freqmaxvals,freqminvals,kmaxvals,ethvals,outputfile){
  
  
  load(paste0('processed/',db,'.RData'))
  g=res$g;
  keyword_dico=res$keyword_dico
  rm(res);gc()
  
  for(filt in filters){
    g = filterGraph(g,filt)
  }
  
  # Q : work on giant component ?
  # 
  clust = clusters(g);cmax = which(clust$csize==max(clust$csize))
  ggiant = induced.subgraph(g,which(clust$membership==cmax))
  
  kmin = 0
  
  modularities = c();
  comnumber=c();
  dmax=c();
  eth=c();
  csizes=c();
  gsizes=c();
  gdensity=c();
  cbalance=c();
  freqsmin=c();freqsmax=c()
  for(freqmax in freqmaxvals){
    for(freqmin in freqminvals){
      for(kmax in kmaxvals){
        for(edge_th in ethvals){
          show(paste0('kmax : ',kmax,' e_th : ',edge_th,' ; freqmin : ',freqmin,' ; freqmax : ',freqmax))
          dd = V(ggiant)$docfreq
          d = degree(ggiant)
          gg=induced_subgraph(ggiant,which(d>kmin&d<kmax&dd>freqmin&dd<freqmax))
          gg=subgraph.edges(gg,which(E(gg)$weight>edge_th))
          clust = clusters(gg);cmax = which(clust$csize==max(clust$csize))
          gg = induced.subgraph(gg,which(clust$membership==cmax))
          com = cluster_louvain(gg)
          # measures
          gsizes=append(gsizes,length(V(gg)));
          gdensity=append(gdensity,2*length(E(gg))/(length(V(gg))*(length(V(gg))-1)))
          csizes=append(csizes,length(clust$csize))
          modularities = append(modularities,modularity(com))
          comnumber=append(comnumber,length(communities(com)))
          cbalance=append(cbalance,sum((sizes(com)/length(V(gg)))^2))
          dmax=append(dmax,kmax);eth=append(eth,edge_th)
          freqsmin=append(freqsmin,freqmin);freqsmax=append(freqsmax,freqmax)
        }
      }
    }
  }
  
  d = data.frame(degree_max=dmax,edge_th=eth,vertices=gsizes,components=csizes,modularity=modularities,communities=comnumber,density=gdensity,comunitiesbalance=cbalance,freqmin=freqsmin,freqmax=freqsmax)
  
  save(d,file=outputfile)
  
}











