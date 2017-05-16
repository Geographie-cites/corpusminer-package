
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
          
          splitLayout(
            div(
              textOutput(ns("text_citing")),
              DT::dataTableOutput(ns("citation_citing"))  
            ), 
            div(
              textOutput(ns("text_cited")), 
              DT::dataTableOutput(ns("citation_cited"))  
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
#' @importFrom DT datatable formatStyle styleEqual JS
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
  
  cited_by <- reactive({
    id <- req(schid())
    
    filter( citation_edges, from == id ) %>% 
      select(to) %>% 
      left_join( citation_data, by = c("to" = "id")) %>% 
      select(cyb, title)
    
  })
  
  citing <- reactive({
    id <- req(schid())
    filter( citation_edges, to == id ) %>% 
      select(from) %>% 
      left_join( citation_data, by = c("from" = "id")) %>% 
      select(cyb, title)
  })
  
  output$citation_cited <- DT::renderDataTable({
    DT::datatable(cited_by(), selection = "none", options = list(pageLength = 5) ) %>% 
      formatStyle( "cyb", target = "row", backgroundColor = JS("value ? 'gray' : 'white' ") )
  })
  
  output$text_cited <- renderText({
    data <- cited_by() %>% 
      group_by(cyb) %>% 
      summarise( n = n() )
    n_all <- sum(data$n)
    n_cyb <- sum(data$n[data$cyb])
    
    sprintf( "Article citing %d articles (%d from cybergeo)", n_all, n_cyb)
  })
  
  
  output$citation_citing <- DT::renderDataTable({
    DT::datatable(citing(), selection = "none", options = list(pageLength = 5)) %>% 
      formatStyle( "cyb", target = "row", backgroundColor = JS("value ? 'gray' : 'white' ") )
  })
  
  output$text_citing <- renderText({
    data <- citing() %>% 
      group_by(cyb) %>% 
      summarise( n = n() )
    n_all <- sum(data$n)
    n_cyb <- sum(data$n[data$cyb])
    
    sprintf( "Article cited by %d articles (%d from cybergeo)", n_all, n_cyb)
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


