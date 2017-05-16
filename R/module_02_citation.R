
#' load neighbors keywords given an id
#'
#' @param con_citation connection to the citation db
#' @param con_keywords connection to the keywords db
#' @param id article id
#'
#' @importFrom dplyr right_join
#' @importFrom purrr map_lgl
#' @importFrom tidyr unnest
#' @return keywords
citationLoadKeywords <- function( schid, edges, data, citation_keyword_data ){
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
  get <- function(tab){
    res <- data %>% 
      select(id, keyword ) %>% 
      rename( word = keyword ) %>% 
      right_join(tab, by = "id") %>% 
      select( word ) %>% 
      filter( map_lgl(word, ~!is.null(.)) )
      
    
    if( nrow(res)) {
      res %>% 
        unnest() %>% 
        distinct() %>% 
        left_join( citation_keyword_data, by = "word") %>% 
        filter( !is.na(freq) )
    } else {
      data_frame( word = character(0), freq = integer(0), group = character(0))
    }

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
        column(3, 
          h4("Data Selection"),
          tags$p(class="text-justify","Search and select a cybergeo paper in the table."),
          DT::dataTableOutput( ns("citationcybergeo") )
        ), 
        column(9, 
          # citation ego network
          h4("Citation network neighborhood"),
          # tags$p(class="text-justify","This graph shows the citation neighborhood of the selected paper"),
          
          splitLayout(
            plotOutput( ns("citationegoplot"), width = "100%" ), 
            tabsetPanel(
              tabPanel( "cited" , DT::dataTableOutput(ns("citation_cited")) ), 
              tabPanel( "citing", DT::dataTableOutput(ns("citation_citing")) )
            )
          ),
          
          # word clouds of semantic content
          h4("Semantic content"),
          
          splitLayout( 
            div(
              htmlOutput(ns("desc_ref_keywords")),
              wordcloud2Output( ns("cloud_ref_keywords"), height = "400px" )
            ), 
            div(
              htmlOutput(ns("desc_provided_keywords")),
              wordcloud2Output( ns("cloud_provided_keywords"), height = "400px" )  
            )
            
          ), 
          
          sliderInput(ns("wordcloud_size"), label  = "size", min = 0, max = 2, step = .1, value = .3)
          
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
#' @importFrom dplyr bind_cols everything
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
  
  schid <- eventReactive(input$citationcybergeo_rows_selected, {
    as.numeric(filtered_data$SCHID[ input$citationcybergeo_rows_selected ])
  })
  
  keywords <- reactive({
    citationLoadKeywords( schid(), citation_edges, citation_data, citation_keyword_data )
  })
  
  keywords_id <- reactive({
    keywords()$id
  })
  
  keywords_all <- reactive({
    keywords()$all
  })
  
  
  edges <- reactive({
    id <- schid()
    
    data <- filter( citation_edges, from == id | to == id )
    
    from <- citation_data %>% 
      select(id, cyb, title) %>% 
      right_join( select(data, from), by = c("id" = "from")) %>% 
      rename( fromcyb = cyb, fromtitle = title, from = id)
      
      
    to <- citation_data %>% 
      select(id, cyb, title) %>% 
      right_join( select(data, to), by = c("id" = "to")) %>% 
      rename( tocyb = cyb, totitle = title, to = id) 
    
    res <- bind_cols( from, to) %>% 
      select(from,to, everything())
    
    
  })

  # render citation graph around selected article
  output$citationegoplot <- renderPlot({
    data <- edges()
    citationVisuEgo( data )
  })
  
  output$citation_cited <- DT::renderDataTable({
    req(schid())
    
    data <- filter( citation_edges, from == schid() ) %>% 
      select(to) %>% 
      left_join( citation_data, by = c("to" = "id")) %>% 
      select(title)
    
    DT::datatable(data)
  })
  
  output$citation_citing <- DT::renderDataTable({
    req(schid())
    data <- filter( citation_edges, to == schid() ) %>% 
      select(from) %>% 
      left_join( citation_data, by = c("from" = "id")) %>% 
      select(title)
    
    DT::datatable(data)
  })
  
  
  output$desc_ref_keywords <- renderUI({
    kw <- keywords_id() 
    nk <- length(unique(kw$word))
    ng <- length(unique(kw$group))
    
    div(
      "Keywords in the selected article" , 
      br(),
      span( sprintf("%d keywords in %d semantic groups", nk, ng) , style = "font-size: smaller; color: gray" )
    )
    
  })  
  output$cloud_ref_keywords <- renderWordcloud2({
    kw <- keywords_id() 
    col <- unname(semanticcolors[ kw$group ])
    data <- select(kw, word, freq) %>% as.data.frame()
    
    wordcloud2(data, size = input$wordcloud_size , color = col, shuffle = FALSE)
  })

  output$desc_provided_keywords <- renderUI({
    kw <- keywords_all() 
    nk <- length(unique(kw$word))
    ng <- length(unique(kw$group))
    
    div(
      "Keywords in the neighborhood", 
      br(),
      span( sprintf("%d keywords in %d semantic groups", nk, ng), style = "font-size: smaller; color: gray" )
    )
    
  })  
  
  output$cloud_provided_keywords <- renderWordcloud2({
    kw <- keywords_all() 
    col <- unname(semanticcolors[ kw$group ])
    
    data <- select(kw, word, freq) %>% as.data.frame()
    wordcloud2(data, size =  input$wordcloud_size, color = col, shuffle = FALSE )
  })
  
  
  output$citationsemanticnw <- renderSvgPanZoom({
    svgPanZoom(
      system.file( 'data-raw', 'semantic.svg', package = "corpusminer" ),
      zoomScaleSensitivity=1, minZoom=2, maxZoom=20, contain=TRUE
    )
  })


}


