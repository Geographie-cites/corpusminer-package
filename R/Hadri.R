
###################################################################
######### Fonctions pour module d'analyse des mots-clés ###########
###################################################################

#####################################
# Fonctions pour l'application shiny
#####################################


#' plot the communities 
#' 
#' plot the communities. result of any community detection algo, here Louvain method
#'
#' @param g 
#' @param comm 
#' @param vertcol 
#' @param vertsize 
#' @param vfacsize 
#' @param edgesize 
#' @param efacsize 
#' @param textsize 
#' 
#' @importFrom igraph layout_in_circle
#' @importFrom graphics plot
#' 
#' @export
VisuComm <- function(g, # igraph network
                     comm, # scalar, character, name of the community
                     vertcol, # scalar, character, vertex color 
                     vertsize, # scalar, numeric, vertex size
                     vfacsize, # scalar, numeric, expansion factor for vertex size
                     edgesize, # scalar, numeric, edge width
                     efacsize, # scalar, numeric, expansion factor for edge width
                     textsize) # scalar, numeric, font size
{  
  par(bg = "#4e5d6c")
  # circle layout with sampled coordinates
  oriCoords <- layout_in_circle(g)
  corrCoords <- oriCoords[sample(seq(1, nrow(oriCoords), 1), size = nrow(oriCoords), replace = FALSE), ]
  
  plot(g,
       edge.color = "#df691a",
       edge.width = efacsize * edgesize,
       edge.curved = F,
       edge.arrow.mode = "-",
       edge.arrow.size = 0.01,
       vertex.color = vertcol,
       vertex.frame.color = "#df691a",
       vertex.label = V(g)$name,
       vertex.label.color = "#ebebeb",
       vertex.label.family = "sans-serif",
       vertex.label.cex = textsize / 10,
       vertex.size = vfacsize * vertsize,
       layout = corrCoords
  )
}


#' plot the semantic field of a selected keyword
#'
#' plot the semantic field of a selected keyword (inverse proportional distance to pseudo-chi2 distance)
#' 
#' @param g 
#' @param kw 
#' @param chidist 
#' @param textsizemin 
#' @param textsizemax 
#'
#' @importFrom ggplot2 theme_bw theme ggplot geom_line geom_text scale_colour_manual scale_size_continuous coord_polar
#' @importFrom igraph get.data.frame
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join group_by mutate
#' @export
VisuSem <- function(g, # igraph network
                    kw, # scalar, character, name of the keyword
		    chidist, # scalar, character, name of the field storing pseudo-chi2 distance
                    textsizemin, # scalar, numeric, minimum font size
                    textsizemax) # scalar, numeric, maximum font sizes
{
  # make theme empty
  theme_empty <- theme_bw() +
    theme(plot.background = element_rect(fill = "#4e5d6c"),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#4e5d6c"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.background = element_rect(fill = "#4e5d6c"))
  
  # graph layout
  tabPoints <- get.data.frame(x = g, what = "vertices")
  tabLinks <- get.data.frame(x = g, what = "edges")
  tabLinks$NODES <- ifelse(tabLinks$from == kw, tabLinks$to, tabLinks$from)
  tabPoints <- tabPoints %>% left_join(x = ., y = tabLinks, by = c("name" = "NODES"))
  
  # compute distance from ego
  tabPoints$DIST <- 1 / tabPoints[[chidist]]
  thresRadis <- seq(0, 0.1 + max(tabPoints$DIST, na.rm = TRUE), 0.1)
  tabPoints$X <- cut(tabPoints$DIST, breaks = thresRadis, labels = thresRadis[-1], include.lowest = TRUE, right = FALSE)
  tabPoints <- tabPoints %>% group_by(X) %>% mutate(NPTS = n())
  
  # get x values
  tabPoints <- tabPoints %>% do(GetXvalues(df = .))
  tabPoints[tabPoints$name == kw, c("XVAL", "DIST")] <- c(0, 0)
  
  # prepare plot
  tabPoints$IDEGO <- ifelse(tabPoints$name == kw, 2, 1)
  tabCircle <- data.frame(XVAL = c(0, 360), DIST = 1)
  
  # draw plot
  circVis <- ggplot() + 
    geom_line(data = tabCircle, aes(x = XVAL, y = DIST), color = "#df691a") + 
    geom_text(data = tabPoints, aes(x = XVAL, y = DIST, label = name, fontface = IDEGO, color = factor(IDEGO), size = nbauth)) +
    scale_colour_manual("Type", values = c("#ebebeb", "#df691a")) +
    scale_size_continuous("Number of articles", range = c(textsizemin, textsizemax)) +
    coord_polar(theta = "x") +
    theme_empty
  
  return(circVis)
}



#####################################
# Fonctions de second niveau (internes) 
#####################################


#' sample x values for polar coordinates for the semantic field visualization (VisuSem function)
#'
#' @param df 
#'
#' @return
#' @noRd
GetXvalues <- function(df){
  initVal <- sample(x = 0:360, size = 1, replace = FALSE)
  tempRange <- seq(initVal, initVal + 360, 360/unique(df$NPTS))
  tempRange <- tempRange[-length(tempRange)]
  df$XVAL <- ifelse(tempRange > 360, tempRange - 360, tempRange) 
  return(df)
}

#' create an ego subgraph for the semantic field visualization (VisuSem function)
#'
#' @param g 
#' @param kw 
#'
#' @return
#' @noRd
#' @importFrom igraph get.edge.ids subgraph.edges
SemanticField <- function(g,  # igraph network
			  kw) # scalar, character, selected keyword
{  
  # list of neighbors
  neiNodes <- unlist(neighborhood(g, order = 1, nodes = V(g)[V(g)$name == kw], mode = "all"))
  pairedNodes <- unlist(paste(which(V(g)$name == kw), neiNodes[-1], sep = ","))
  collapseNodes <- paste(pairedNodes, collapse = ",")
  vecNodes <- as.integer(unlist(strsplit(collapseNodes, split = ",")))
  
  # get edges and create graph
  edgeIds <- get.edge.ids(g, vp = vecNodes)
  gSem <- subgraph.edges(g, eids = edgeIds, delete.vertices = TRUE)
  
  return(gSem)
}



#####################################
# Fonctions de pré-production
#####################################

#' Clean a corpus
#' 
#' function to be lapplied to a list of vectors (each vector is the set of keywords for a given article). 
#' Clean the keywords list (no punctuation, no digits, trim white spaces)
#'
#' @param mystr 
#'
#' @return
#' @noRd
#' @importFrom stringr str_to_lower str_trim
CleanCorpus <- function(mystr){
  mystr <- str_to_lower(mystr)
  mystr <- gsub("[[:punct:]]", "", mystr)
  mystr <- gsub("[[:digit:]]", "", mystr)
  mystr <- gsub("\\s+", " ", str_trim(mystr))
  return(mystr)
}


#' Make edges list
#'
#' function to be lapplied to a list of vectors (each vector is the set of keywords for a given article). 
#' Convert a list of keywords into a list of edges for creating a semantic network. 
#' Called inside MakeNetwork function (peut-être pas une bonne idée ??)
#'
#' @param x 
#'
#' @return
#' @noRd
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate
MakeEdgesList <- function(x) # vector, character, keywords list for one article
{
  if (length(x) == 1){
    return(paste(x, x, sep = "_"))
  } else {
    crossCntry <- expand.grid(ID1 = as.vector(x), 
                              ID2 = as.vector(x), 
                              stringsAsFactors = FALSE)
    crossCntry <- crossCntry %>% 
      filter(ID1 != ID2) %>% 
      mutate(EDGE = ifelse(ID1 < ID2, paste(ID1, ID2, sep = "_"), paste(ID2, ID1, sep = "_")))
    return(unique(crossCntry$EDGE))
  }
} 

# Description: 

#' make network
#'
#' create an igraph network from the list of edges created by function MakeEdgesList, enrich the network with keywords degree and the corresponding number of articles
#' @param colist 
#'
#' @noRd
#' 
#' @importFrom reshape2 colsplit
#' @importFrom igraph graph_from_data_frame V degree
MakeNetwork <- function(colist) # list of vectors (each vector is the set of keywords for a given article)
{
  # get edges
  edgesList <- lapply(colist, MakeEdgesList) %>% unlist()
  edgesTab <- data.frame(table(edgesList), stringsAsFactors = FALSE)
  edgesTab <- data.frame(colsplit(edgesTab$edgesList, pattern = "_", names = c("ITEM1", "ITEM2")), 
                         obsfreq = edgesTab$Freq, 
                         stringsAsFactors = FALSE)
  
  # build network
  netWork <- graph_from_data_frame(edgesTab, directed = FALSE) %>% simplify(graph = ., remove.loops = TRUE, remove.multiple = FALSE)
  
  # enrich network
  countItems <- as.data.frame(table(unlist(colist)), stringsAsFactors = FALSE)
  idMatch <- match(V(netWork)$name, countItems$Var1)
  V(netWork)$nbitems <- countItems$Freq[idMatch]
  V(netWork)$degree <- degree(netWork)
  
  return(netWork)
}
