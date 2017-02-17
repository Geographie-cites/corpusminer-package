
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
#' @param g igraph network
#' @param vsize.prop One of "uni", "poi" or "deg" to control on what nodes are proportional
#' @param vsize.fac Expansion factor for vertex size
#' @param vsize.default default value for vertex size, e.g. 1 for direct plot or 30 for svg rendering
#' @param esize.prop One of "nbl" or "rel" to control on what edges are proportional
#' @param esize.fac Expansion factor for edge sizes
#' @param vertex.label.cex
#' @param edge.color
#' @param edge.curved
#' @param edge.arrow.mode
#' @param edge.arrow.size
#' @param vertex.color
#' @param vertex.frame.color
#' @param vertex.label.color
#' @param vertex.label.family
#' @param bg background
#'
#' @importFrom igraph layout_in_circle
#' @importFrom graphics plot
#' @export
#'
VisuComm <- function(g,
                     vsize.prop, vsize.fac = .5, vsize.default = 1,
                     esize.prop, esize.fac = .5,
                     vertex.label.cex,
                     edge.color = "#df691a",
                     edge.curved = FALSE,
                     edge.arrow.mode = "-",
                     edge.arrow.size = 0.01,
                     vertex.color = "#2b3e50",
                     vertex.frame.color = "#df691a",
                     vertex.label.color = "#ebebeb",
                     vertex.label.family = "sans-serif", 
                     bg = "#4e5d6c"
                     ){

  # sample layout
  corrCoords <- layout_in_circle(g)
  corrCoords <- corrCoords[ sample(nrow(corrCoords)), ]

  # calculate edgesize and vertsize depending on
  edges <- E(g)
  vertices <- V(g)
  vertsize <- vsize.default
  if(vsize.prop == "uni" && esize.prop == "rel"){
    edgesize <- edges$relresid
  } else if(vsize.prop == "uni" && esize.prop == "nbl"){
    edgesize <- edges$obsfreq
  } else if(vsize.prop == "poi" && esize.prop == "rel"){
    vertsize <- vertices$nbauth
    edgesize <- edges$relresid
  } else if(vsize.prop == "poi" && esize.prop == "nbl"){
    vertsize <- vertices$nbauth
    edgesize <- edges$obsfreq
  } else if(vsize.prop == "deg" && esize.prop == "rel"){
    vertsize <- vertices$degbeg
    edgesize <- edges$relresid
  } else if(vsize.prop == "deg" && esize.prop == "nbl"){
    vertsize <- vertices$degbeg
    edgesize <- edges$obsfreq
  }

  par(bg = bg)

  plot(g,
       edge.color          = edge.color,
       edge.width          = edgesize * esize.fac,
       edge.curved         = edge.curved,
       edge.arrow.mode     = edge.arrow.mode,
       edge.arrow.size     = edge.arrow.size,
       vertex.color        = vertex.color,
       vertex.frame.color  = vertex.frame.color,
       vertex.label        = V(g)$name,
       vertex.label.color  = vertex.label.color,
       # vertex.label.family = vertex.label.family,
       vertex.label.cex    = vertex.label.cex,
       vertex.size         = vertsize * vsize.fac,
       layout              = corrCoords
  )
}


#' plot the semantic field of a selected keyword
#'
#' plot the semantic field of a selected keyword (inverse proportional distance to pseudo-chi2 distance)
#'
#' @param g igraph network
#' @param kw scalar, character, name of the keyword
#' @param chidist scalar, character, name of the field storing pseudo-chi2 distance
#' @param textsizemin scalar, numeric, minimum font size
#' @param textsizemax scalar, numeric, maximum font sizes
#' 
#' @importFrom ggplot2 theme_bw theme ggplot geom_line geom_text scale_colour_manual scale_size_continuous coord_polar element_rect  element_blank
#' @importFrom igraph get.data.frame
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join group_by mutate
#' @export
VisuSem <- function(g, kw, chidist, textsizemin, textsizemax) {

  assert_that( is.string(kw), is.string(chidist), is.number(textsizemin), is.number(textsizemax) )

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
#' @param g igraph network
#' @param kw scalar, character, selected keyword
#'
#' @return
#' @noRd
#' @importFrom igraph get.edge.ids subgraph.edges neighborhood
SemanticField <- function(g, kw){
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
  mystr <- gsub("[[:punct:][:digit:]]", " ", mystr)
  mystr <- gsub("\\s+", " ", str_trim(mystr))
  return(mystr)
}


#' Make edges list
#'
#' function to be lapplied to a list of vectors (each vector is the set of keywords for a given article).
#' Convert a list of keywords into a list of edges for creating a semantic network.
#' Called inside MakeNetwork function (peut-être pas une bonne idée ??)
#'
#' @param x vector, character, keywords list for one article
#'
#' @return
#' @noRd
#' @importFrom utils combn
MakeEdgesList <- function(x) {
  x <- sort(unique(x))

  if (length(x) == 1){
    return(paste(x, x, sep = "_"))
  } else {
    combs <- combn(length(x), 2)
    edges <- paste( x[combs[1,]], x[combs[2,]], sep = "_")
    edges
  }
}

#' make network
#'
#' create an igraph network from the list of edges created by function MakeEdgesList, enrich the network with keywords degree and the corresponding number of articles
#'
#' @param colist list of vectors (each vector is the set of keywords for a given article)
#'
#' @noRd
#'
#' @importFrom reshape2 colsplit
#' @importFrom igraph graph_from_data_frame V degree
MakeNetwork <- function(colist){
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

#' Describe network
#'
#' Describe number of vertices and and edges
#' @param g a network
#' @export
#' @importFrom igraph vcount ecount
describe_network <- function(g){
  sprintf("<strong>Description :</strong> The network has <strong>%d vertices</strong> (number of keywords) and <strong>%d edges</strong> (number of edges between keywords).", vcount(g), ecount(g))
}


#' create information table for nodes
#'
#' @param g a network
#'
#' @return A data frame with columns \code{KEYWORDS}, \code{NB_ARTICLES} and \code{DEGREE}
#' @export
#' @importFrom tibble data_frame
#' @importFrom igraph V
info_table_nodes <- function(g){
  v <- V(g)
  data_frame( KEYWORDS = v$name, NA_ARTICLES = v$nbauth, DEGREE = v$degbeg)
}


#' create information table for edges
#'
#' @param g a network
#'
#' @return A data frame with columns code{KEYWORD1}, \code{KEYWORD2}, \code{OBSERVED_WEIGHT}, \code{EXPECTED_WEIGHT} and \code{RESIDUALS}
#' @export
#'
#' @importFrom igraph get.data.frame
#' @importFrom dplyr rename mutate_each funs
#' @importFrom magrittr %>%
info_table_edges <- function(g){
  get.data.frame(g) %>%
    rename( KEYWORD1 = from, KEYWORD2 = to, OBSERVED_WEIGHT = obsfreq, EXPECTED_WEIGHT = theofreq, RESIDUALS = relresid ) %>%
    mutate_each( funs(round(., 2) ), EXPECTED_WEIGHT, RESIDUALS )
}
