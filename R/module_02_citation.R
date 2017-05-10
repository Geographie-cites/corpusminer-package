#' #' load citation edges given an reference id
#' #'
#' #' @param con database connection
#' #' @param id edge id
#' citationLoadEdges <- function(con, id){
#'   query <- sprintf( "SELECT * FROM edges WHERE `from`='%s' OR `to`='%s' ;", id, id )
#'   dbGetQuery(con,query)
#' }

#' load neighbors keywords given an id
#'
#' @param con_citation connection to the citation db
#' @param con_keywords connection to the keywords db
#' @param id article id
#'
#' @return keywords
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
#' 
#' @importFrom graphics par
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

#' @importFrom svgPanZoom svgPanZoomOutput
#' @importFrom wordcloud2 wordcloud2Output
#' @export
cybergeo_module_citation_UI <- function(id, citation_cybergeodata ){
  ns <- NS(id)

  navbarMenu("Citation network",

    tabPanel("Citation Network",
      # select article to visualize
      fluidRow(
        column(4, 
          h4("Data Selection"),
          tags$p(class="text-justify","Search and select a cybergeo paper in the table."),
          DT::dataTableOutput( ns("citationcybergeo") )
        ), 
        column(8, 
          # citation ego network
          h4("Citation network neighborhood"),
          # tags$p(class="text-justify","This graph shows the citation neighborhood of the selected paper"),
          plotOutput( ns("citationegoplot"), width = "100%" ), 
          
          # word clouds of semantic content
          h4("Semantic content"),
          # tags$p(class="text-justify","This graph shows the semantic content (color legend in user guide) of the paper (left) and its neighborhood (right)."),
          
          splitLayout( 
            wordcloud2Output( ns("cloud_ref_keywords"), height = "400px" ), 
            wordcloud2Output( ns("cloud_provided_keywords"), height = "400px" )
          )
          
        )
        
      )
    ),

    # svg viusalization of the full semantic network
    tabPanel("Semantic Network",
      h4("Full Semantic Network"),
      svgPanZoomOutput(ns("citationsemanticnw"), width = "100%", height = "100%")
    ),

    # user guide
    tabPanel("User guide",
      includeMarkdown( "doc/CitationNetwork.md")
    )

  )

}


#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2
#' @importFrom DT datatable
#' 
#' @export
cybergeo_module_citation <- function( input, output, session, citation_cybergeodata){

  citationdbcit <- dbConnect(SQLite(), system.file("sqlite", "CitationNetwork.sqlite3", package = "corpusminer"))
  citationdbkws <- dbConnect(SQLite(), system.file("sqlite", "CitationKeywords.sqlite3", package = "corpusminer"))
  
  filtered_data <- citation_cybergeodata %>%
    filter(linknum > 0 | kwcount > 0) %>%
    select(id, title, authors)
  
  ## selection datatable
  output$citationcybergeo <- DT::renderDataTable({
    datatable( filtered_data, selection = "single", rownames = FALSE )  
  })
  
  id <- reactive({
    filtered_data$id[ input$citationcybergeo_rows_selected ]
  })
  
  schid <- reactive({
    filtered_data$SCHID[ input$citationcybergeo_rows_selected ]
  })
  
  keywords <- reactive({
    citationLoadKeywords(citationdbcit, citationdbkws, schid())
  })
  
  edges <- reactive({
    query <- sprintf( "SELECT * FROM edges WHERE `from`='%s' OR `to`='%s' ;", id, id )
    dbGetQuery(citationdbcit,query)
  })

  
  #     wordcloud(
  #       words=keywords[[id]],
  #       freq=citationkwfreqs[keywords[[id]]],
  #       colors=unlist(semanticcolors[citationkwthemdico[keywords[[id]]]]),
  #       ordered.colors = TRUE
  #     )
  output$cloud_ref_keywords <- renderWordcloud2({
    data <- data_frame( word = letters, freq = rep(1, 26))
    wordcloud2(data)
  })

  #     allkws=unlist(keywords)
  #     wordcloud(
  #       words=allkws,
  #       freq=citationkwfreqs[allkws],
  #       colors=unlist(semanticcolors[citationkwthemdico[allkws]]),
  #       ordered.colors = TRUE
  #     )
  output$cloud_provided_keywords <- renderWordcloud2({
    data <- data_frame( word = letters, freq = rep(1, 26))
    wordcloud2(data)
  })
  
  # render citation graph around selected article
  # output$citationegoplot = renderPlot({
  #   citationVisuEgo( edges() )
  # })
  
  output$citationsemanticnw <- renderSvgPanZoom({
    svgPanZoom(
      system.file( 'shiny', 'data-raw', 'semantic.svg', package = "corpusminer" ),
      zoomScaleSensitivity=1, minZoom=2, maxZoom=20, contain=TRUE
    )
  })


}


