
#' shiny module for the semantic tab
#' @import wordcloud2
#' 
#' @param id see \code{\link[shiny]{callModule}}
#' @param pattern_list pattern list
#' @export
cybergeo_module_semantic_UI <- function(id, pattern_list){
  ns <- NS(id)

  tabPanel("Full-text Semantic network",
    fluidRow(
      column(2, 
        "Enter comma separated patterns to filter terms from the text of the articles", 
        
        fluidRow(
          column(10, textInput( ns("patterns_input"), label = NULL , width = "100%" ) ), 
          column(2, actionButton( ns("patterns_button"), label = " ", width = "100%", icon = icon("filter") ))
        ), 
        
        "Example patterns: ", 
        tags$p( 
          paste( pattern_list, collapse = ", " ), 
          
          style  = "font-size: smaller; color: gray"
        )
      ), 
      column(5, plotOutput(ns("chronogram"), height = "400px")), 
      column(5, wordcloud2Output(ns("cloud"), height = "400px"))
    ), 
    fluidRow( 
      column(6, DT::dataTableOutput(ns("phrases"))), 
      column(6, DT::dataTableOutput(ns("citations")))
    )
    
  )
}

step <- function(.){
  incProgress(1)
}

#' Shiny module server function for the semantic tab
#' @param input input
#' @param output output
#' @param session session
#' @param pattern_list pattern list
#' @import DT
#' @export
cybergeo_module_semantic <- function( input, output, session, pattern_list, terms, articles, sentences ){

  patterns <- reactive({
    input$patterns_button
    
    txt <- isolate(input$patterns_input)
    str_trim( str_split( txt, "," )[[1]] )
  })
  
  terms_matched <- reactive({
    patterns <- patterns()
    
    withProgress({
      bind_rows(lapply(patterns, function(pattern){
        res <- filter( terms, str_detect(term, pattern) ) %>%
          mutate( pattern = pattern )
        incProgress(1)
        res
      }))  
    }, min = 0, max = length(patterns), value = 0, message = "extracting terms")
    
  })
  
  titles_matched <- reactive({
    withProgress({
      
      citations <- terms_matched()  %>%
        select(article_id) %>%
        distinct() %>%
        left_join(articles, by = c("article_id" = "id")) %>%
        arrange(date) %>%
        select(citation)
      
      incProgress(1)
      
      datatable(citations, options = list(pageLength = 5) )
      
    }, min = 0, max = 1, value = 0)
  })
  
  articles_matched <- reactive({ 
    withProgress({
    
      terms_matched() %>%
        group_by(article_id, pattern) %>%
        summarise(count = sum(count)) %>%
        left_join(articles, by = c("article_id" = "id")) %>%
        mutate(ym = str_sub(date, 1, 4)) %>%
        group_by(ym, pattern) %>%
        summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
        ungroup() %>%
        mutate(date = parse_date_time(ym, "%y")) %>%
        select(date, pattern, articles, terms)
    
    }, min = 0, max = 1 , value = 0, message = "retrieve matching article" )
  })
  
  phrases <- reactive({
    patterns <- patterns()
   
    withProgress(min=0, max=length(patterns), value=0, message = "filtering sentences",  {
      data <- if( identical( patterns, "" ) ){
        select( sentences, -id )
      } else {
        datasets <- lapply( patterns , function( pattern ){
          res <- sentences %>% 
            select(sentence, article_id) %>% 
            filter(grepl(pattern, sentence, ignore.case = TRUE, perl = TRUE) ) %>% 
            mutate(sentence = gsub( sprintf( "(%s)", pattern), "<strong>\\1</strong>", sentence, ignore.case = TRUE ) )
          
          incProgress(1)
          res
      })
      bind_rows(datasets)   
    }
    datatable( data, escape = FALSE, options = list(pageLength = 5) )

  })
})
  
  chronogram <- reactive({
    matched <- articles_matched()
    ggplot(matched, aes(date, articles)) +
      geom_bar(stat = "identity") +
      facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
      labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
  })
  
  cloud <- reactive({
    withProgress(min = 0, max = 6, value = 0, message = "generating data for word cloud", {
      
      x <- terms_matched(); incProgress(1)
      x <- group_by( x, term ); incProgress(1)
      x <- summarise(x, freq = sum(count)); incProgress(1)
      x <- arrange( x, desc(freq) ) ; incProgress(1)
      x <- head( x, 500); incProgress(1)
      x <- rename( x, world = term)

      wordcloud2( x , shape = "square")  
      
    })
  })
  
  
  
  # Compute the Outputs
  output$chronogram <- renderPlot(chronogram())
  output$cloud <- renderWordcloud2(cloud())

  output$phrases <- DT::renderDataTable( phrases() )
  output$citations <- DT::renderDataTable( titles_matched() )

  outputOptions(output, "cloud", priority = -1 )
  
}
