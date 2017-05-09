
#' shiny module for the semantic tab
#' @import wordcloud2
#' 
#' @param id see \code{\link[shiny]{callModule}}
#' @param pattern_list pattern list
#' @importFrom purrr map
#' @export
cybergeo_module_semantic_UI <- function(id, pattern_list){
  ns <- NS(id)

  tabPanel("Full-text Semantic network",
    absolutePanel(id = ns("controls"), fixed= TRUE, top = 10 , right = 20, style = "z-index:1000", 
      selectizeInput( ns("patterns"), label = NULL, 
        multiple = TRUE, choices = c("geo", pattern_list), selected = "geo", 
        options = list( "plugins" = list( "remove_button"), "create" = TRUE, persist = TRUE)
      )
    ),
    fluidRow(
      column(6, plotOutput(ns("chronogram"), height = "400px")), 
      column(6, wordcloud2Output(ns("cloud"), height = "400px"))
    ), 
    fluidRow( 
      column(6, DT::dataTableOutput(ns("citations"))), 
      column(6, DT::dataTableOutput(ns("phrases")))
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
#' @importFrom DT renderDataTable datatable
#' @importFrom stringr str_detect str_replace
#' @importFrom tidyr separate
#' @export
cybergeo_module_semantic <- function( input, output, session, pattern_list, terms, articles, sentences ){

  patterns <- reactive({
    input$patterns
  })
  
  terms_matched <- reactive({
    patterns <- patterns()
      
    withProgress({
      if( is.null(patterns) ){
      res <- mutate( terms, pattern = "" )
        incProgress(1)
        res
      } else {
        bind_rows(lapply(patterns, function(pattern){
          res <- filter( terms, str_detect(term, pattern) ) %>%
            mutate( pattern = pattern )
          incProgress(1)
          res
        }))    
      }
    }, min = 0, max = length(patterns), value = 0, message = "extracting terms")
    
  })
  
  titles_matched <- reactive({
    citations <- terms_matched()  %>%
      select(article_id) %>%
      distinct() %>%
      left_join(articles, by = c("article_id" = "id")) %>%
      arrange(date) %>%
      select( article_id, authors, title, title_en )
    
    datatable(citations, options = list(pageLength = 5), rownames = FALSE )
  })
  
  articles_matched <- reactive({ 
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
  })
  
  phrases <- reactive({
    patterns <- patterns()
   
    withProgress(min=0, max=length(patterns), value=0, message = "filtering sentences",  {
      data <- if( is.null( patterns ) ){
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
    datatable( data, escape = FALSE, options = list(pageLength = 5), rownames = FALSE  )

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

  output$phrases <- renderDataTable( phrases() )
  output$citations <- renderDataTable( titles_matched() )

  outputOptions(output, "cloud" )
  
}
