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
#' @param vertex.label.cex see \code{\link[igraph]{igraph.plotting}}
#' @param edge.color see \code{\link[igraph]{igraph.plotting}}
#' @param edge.curved see \code{\link[igraph]{igraph.plotting}}
#' @param edge.arrow.mode see \code{\link[igraph]{igraph.plotting}}
#' @param edge.arrow.size see \code{\link[igraph]{igraph.plotting}}
#' @param vertex.color see \code{\link[igraph]{igraph.plotting}}
#' @param vertex.frame.color see \code{\link[igraph]{igraph.plotting}}
#' @param vertex.label.color see \code{\link[igraph]{igraph.plotting}}
#' @param vertex.label.family see \code{\link[igraph]{igraph.plotting}}
#' @param bg see \code{\link[igraph]{igraph.plotting}}
#'
#' @importFrom graphics plot par
#' @export
VisuComm <- function(g,
                     vsize.prop, vsize.fac = .5, vsize.default = 1,
                     esize.prop, esize.fac = .5,
                     vertex.label.cex,
                     edge.color = "gray90", # "#df691a",
                     edge.curved = FALSE,
                     edge.arrow.mode = "-",
                     edge.arrow.size = 0.01,
                     vertex.color = "gray70",
                     vertex.frame.color = "#df691a",
                     vertex.label.color = "black",
                     vertex.label.family = "sans-serif",
                     bg = "white" # "#4e5d6c"
){

  # sample layout
  corrCoords <- layout_in_circle(g)
  
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

  par(bg = bg, mar = c(0.5,0.5,0.5,0.5) )

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
#' @export
VisuSem <- function(g, kw, chidist, textsizemin, textsizemax) {

  # make theme empty
  theme_empty <- theme_bw() +
    theme( # plot.background = element_rect(fill = "#4e5d6c"),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          # panel.background = element_rect(fill = "#4e5d6c"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
      )

  # graph layout
  tabPoints <- get.data.frame(x = g, what = "vertices")
  tabLinks <- get.data.frame(x = g, what = "edges")
  tabLinks$NODES <- ifelse(tabLinks$from == kw, tabLinks$to, tabLinks$from)
  tabPoints <- tabPoints %>% left_join(tabLinks, by = c("name" = "NODES"))

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
    # scale_colour_manual("Type", values = c("#ebebeb", "#df691a")) +
    scale_colour_manual("Type", values = c("grey90", "#df691a")) +
    scale_size_continuous("Number of articles", range = c(textsizemin, textsizemax)) +
    coord_polar(theta = "x") +
    theme_empty

  return(circVis)
}



#####################################
# Low-level functions
#####################################


#' sample x values for polar coordinates for the semantic field visualization (VisuSem function)
#'
#' @param df data frame
#'
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
#' @noRd
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
# Pre-processing functions
#####################################

#' Clean a corpus
#'
#' function to be lapplied to a list of vectors (each vector is the set of keywords for a given article).
#' Clean the keywords list (no punctuation, no digits, trim white spaces)
#'
#' @param mystr string to clean
#'
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
info_table_nodes <- function(g){
  v <- V(g)
  data_frame( KEYWORDS = v$name, ARTICLES = v$nbauth, DEGREE = v$degbeg)
}


#' create information table for edges
#'
#' @param g a network
#'
#' @return A data frame with columns code{KEYWORD1}, \code{KEYWORD2}, \code{OBSERVED_WEIGHT}, \code{EXPECTED_WEIGHT} and \code{RESIDUALS}
#' @export
info_table_edges <- function(g){
  get.data.frame(g) %>%
    tbl_df %>%
    rename( KEYWORD1 = from, KEYWORD2 = to, OBSERVED_WEIGHT = obsfreq, EXPECTED_WEIGHT = theofreq, RESIDUALS = relresid ) %>%
    mutate_each( funs(round(., 2) ), EXPECTED_WEIGHT, RESIDUALS )
}


#' extract subgraph for a given community
#'
#' @param g full graph
#' @param community community
#'
#' @export
extract_community_graph <- function( g, community ){
  vertices <- V(g)
  induced.subgraph(g, vids=vertices[vertices$clus == community])
}


#### ------------ shiny module

#' @param id module id
#' @rdname cybergeo_module_keyword
#' @importFrom shiny navbarMenu tabPanel htmlOutput tags splitLayout fluidRow wellPanel radioButtons sliderInput column plotOutput selectInput downloadButton withMathJax includeMarkdown 
#' @importFrom dplyr mutate_at
#' @export
cybergeo_module_keyword_UI <- function(id, NETKW ){
  ns <- NS(id)

  choices_communities <- sort( unique( V(NETKW)$clus ))
  choices_keywords <- sort( unique( V(NETKW)$name ))

  navbarMenu("Keyword network",

    tabPanel("Data summary",
      htmlOutput( ns("textfull") ),
      tags$hr(),
      
      splitLayout(
        DT::dataTableOutput( ns("contentsnodes") ), 
        DT::dataTableOutput( ns("contentsedges") )
      )
    ),

    tabPanel("Communities",
      fluidRow(
        column(3,
          wellPanel(
            selectInput(ns("commid"), label = "Choose a community",
              choices = choices_communities, selected = "", multiple = FALSE
            ), 
            tags$strong("Graphic options"),
            radioButtons(ns("vsizecom"), "Nodes proportional to:",
              list("Uniforme" = "uni", "Number of articles" = "poi","Nodes degree" = "deg")
            ),
            radioButtons(ns("esizecom"), "Edges proportional to:",
              list("Observed weight" = "nbl","Residuals" = "rel")
            ),
            sliderInput(ns("vfacsizecom"), label = "Nodes size",
              min = 0, max = 1, value = 0.5, step = 0.05
            ),
            sliderInput(ns("efacsizecom"), label = "Edges size",
              min = 0, max = 2, value = 1, step = 0.1
            ),
            sliderInput(ns("tsizecom"), label = "Font size",
              min = 1, max = 15, value = 10, step = 1
            )
          )
        ),
        column(5,
          plotOutput(ns("plotcomm"), width = "100%", height = 600),
          downloadButton(ns("downcomm"), "Download plot")
        ), 
        column(4, 
          DT::dataTableOutput(ns("tablecomm") )
        )
      )
    ),
    tabPanel("Semantic area",
      selectInput(ns("kwid2"), label = "Choose a keyword",
        choices = choices_keywords, selected = "", multiple = FALSE
      ),
      fluidRow(
        column(3, wellPanel(
          tags$strong("Graphic options"),
          sliderInput(ns("tsizesemmin"), label = "Font size (min)",
            min = 1, max = 10, value = 4, step = 0.5
          ),
          sliderInput(ns("tsizesemmax"), label = "Font size (max)",
            min = 1, max = 10, value = 6, step = 0.5
          ),
          downloadButton(ns("downsem"), "Download plot")
        )),
        column(9, plotOutput(ns("plotsem"), width = "100%", height = "800px"))
      )
    ),
    tabPanel("User guide", withMathJax(),includeMarkdown( "doc/README_HC.md"))
  )
}

#' keyword module
#' 
#' @param input input
#' @param output output
#' @param session session
#' @param NETKW keyword graph
#'
#' @importFrom shiny renderPlot reactive renderText downloadHandler
#' @importFrom DT datatable
#' @export
cybergeo_module_keyword <- function( input, output, session, NETKW ){

  # select community
  SelectComm <- reactive( extract_community_graph(NETKW, input$commid) )

  # create semantic field
  SelectSemField <- reactive( SemanticField(NETKW, kw = input$kwid2) )

  output$textfull <- renderText({
    describe_network( NETKW )
  })

  nodes <- info_table_nodes( NETKW )
  edges <- info_table_edges( NETKW )
  
  output$contentsnodes <- DT::renderDataTable(
    datatable( nodes )
  )

  selected_keywords <- reactive({
    idx <- input$contentsnodes_rows_selected
    if( !is.null(idx) ){
      nodes$KEYWORDS[idx]
    }
  })
  
  
  output$contentsedges <- DT::renderDataTable({
    kw <- selected_keywords()
    if( !is.null(kw) ){
      edges <- filter( edges, KEYWORD1 %in% kw | KEYWORD2 %in% kw )
    }
    
    datatable( edges, selection = "none")
  })

  output$plotcomm <- renderPlot({
    VisuComm( SelectComm(),
      vsize.prop = input$vsizecom, vsize.fac = input$vfacsizecom, vsize.default = 1,
      esize.prop = input$esizecom, esize.fac = input$efacsizecom,
      vertex.label.cex = input$tsizecom / 10
    )
  })
  
  output$tablecomm <- DT::renderDataTable({
    data <- get.data.frame(SelectComm()) %>% 
      mutate_at( vars(theofreq, relresid), funs(round(., 2)) )
    
    datatable( data  )
  })

  output$downcomm <- downloadHandler(
    filename = "Community.svg",
    content = function(file) {
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)

      VisuComm( SelectComm(),
                vsize.prop = input$vsizecom, vsize.fac = input$vfacsizecom, vsize.default = 30,
                esize.prop = input$esizecom, esize.fac = input$efacsizecom,
                vertex.label.cex = input$tsizecom / 10
      )
      dev.off()
    }
  )

  output$plotsem <- renderPlot({
    VisuSem(SelectSemField(), kw = input$kwid2, 
      chidist = "relresid", 
      textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax
    )
  })

  output$downsem <- downloadHandler(
    filename = "Semantic.svg",
    content = function(file) {
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)
      VisuSem(SelectSemField(), kw = input$kwid2, 
        chidist = "relresid", textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax)
      dev.off()
    }
  )


}
