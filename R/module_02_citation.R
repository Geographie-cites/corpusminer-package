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
#' @importFrom purrr map_lgl
#' @importFrom tidyr unnest
#' @return keywords
citationLoadKeywords <- function( schid, edges, data ){
  
  # collect the id of the article that
  # - cite the one identified by schid
  # - are cited by schid
  # - are schid
  id_tab <- bind_rows(
    edges %>% filter( from == schid ) %>% select(to) %>% rename(id=to), 
    edges %>% filter( to == schid ) %>% select(from) %>% rename(id=from), 
    data_frame( id = schid )
  ) %>% distinct(id)
  
  # extract keywords for these articles
  get <- function(id_tab){
    res <- data %>% 
      select(id, keyword ) %>% 
      right_join(id_tab, by = "id") %>% 
      filter( map_lgl(keyword, ~!is.null(.)) )
    
    if( nrow(res)) unnest(res) else data_frame( id = numeric(0), keyword = character(0))

  }
  
  list( all = get(id_tab), id = get(data_frame(id=schid)) )
  
}

#' visualize an ego network given edges
#' 
#' @param edges edges
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

#' ui component for citation module
#' 
#' @param id module id
#' @importFrom svgPanZoom svgPanZoomOutput
#' @importFrom wordcloud2 wordcloud2Output
#' @rdname cybergeo_module_citation
#' @export
cybergeo_module_citation_UI <- function(id){
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
            # h4("Citation network neighborhood"),
            # # tags$p(class="text-justify","This graph shows the citation neighborhood of the selected paper"),
            # plotOutput( ns("citationegoplot"), width = "100%" ), 
          
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


#' citation shiny module 
#' 
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2
#' @importFrom DT datatable
#' 
#' @export
cybergeo_module_citation <- function( input, output, session, citation_cybergeodata, citation_keyword_data, citation_edges, citation_data ){

  filtered_data <- citation_cybergeodata %>%
    filter(linknum > 0 | kwcount > 0) %>%
    select(id, SCHID, title, authors)
  
  ## selection datatable
  output$citationcybergeo <- DT::renderDataTable({
    datatable( select(filtered_data, title, authors), selection = "single", rownames = FALSE )  
  })
  
  # id <- eventReactive(input$citationcybergeo_rows_selected, {
  #   filtered_data$id[ input$citationcybergeo_rows_selected ]
  # })
  
  schid <- eventReactive(input$citationcybergeo_rows_selected, {
    filtered_data$SCHID[ input$citationcybergeo_rows_selected ]
  })
  
  keywords <- reactive({
    citationLoadKeywords( schid(), citation_edges, citation_data )
  })
  
  keywords_id <- reactive({
    keywords()$id
  })
  
  keywords_all <- reactive({
    keywords()$all
  })
  
  
  edges <- reactive({
    query <- sprintf( "SELECT * FROM edges WHERE `from`='%s' OR `to`='%s' ;", id, id )
    dbGetQuery(citationdbcit,query)
  })

  
  #     wordcloud(
  #       words=keywords[[id]],
  #       freq=citation_data$freq[keywords[[id]]],
  #       colors=unlist(semanticcolors[citation_data$group[keywords[[id]]]]),
  #       ordered.colors = TRUE
  #     )
  output$cloud_ref_keywords <- renderWordcloud2({
    kw <- keywords_id() %>% 
      rename(word=keyword) %>% 
      left_join( citation_keyword_data, by = "word") 
    
    col <- unname(semanticcolors[ kw$group ])
    data <- select(kw, word, freq) %>% as.data.frame()
    wordcloud2(data, color = col)
  })

  #     allkws=unlist(keywords)
  #     wordcloud(
  #       words=allkws,
  #       freq=citation_data$freq[allkws],
  #       colors=unlist(semanticcolors[citation_data$group[allkws]]),
  #       ordered.colors = TRUE
  #     )
  output$cloud_provided_keywords <- renderWordcloud2({
    kw <- keywords_all() %>% 
      rename(word=keyword) %>% 
      distinct(word) %>% 
      left_join( citation_keyword_data, by = "word")
    
    col <- unname(semanticcolors[ kw$group ])
    
    data <- select(kw, word, freq) %>% as.data.frame()
    wordcloud2(data, color = col)
  })
  
  # render citation graph around selected article
  # output$citationegoplot = renderPlot({
  #   citationVisuEgo( edges() )
  # })
  
  output$citationsemanticnw <- renderSvgPanZoom({
    svgPanZoom(
      system.file( 'data-raw', 'semantic.svg', package = "corpusminer" ),
      zoomScaleSensitivity=1, minZoom=2, maxZoom=20, contain=TRUE
    )
  })


}


