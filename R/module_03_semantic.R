
#' shiny module for the semantic tab
#' 
#' @importFrom wordcloud2 wordcloud2Output
#' 
#' @param id see \code{\link[shiny]{callModule}}
#' 
#' @importFrom shiny NS tabPanel absolutePanel selectizeInput splitLayout plotOutput 
#' @importFrom purrr map
#' @export
cybergeo_module_semantic_UI <- function(id){
  ns <- NS(id)

  tabPanel("Full-text Semantic network",
    absolutePanel(id = ns("controls"), fixed= TRUE, top = 10 , right = 20, style = "z-index:1000", 
      selectizeInput( ns("patterns"), label = NULL, 
        multiple = TRUE, choices = c("geo", pattern_list), selected = "geo", 
        options = list( "plugins" = list( "remove_button"), "create" = TRUE, persist = TRUE)
      )
    ),
    splitLayout(
      plotOutput(ns("chronogram"), height = "400px"), 
      wordcloud2Output(ns("cloud"), height = "400px")
    ), 
    splitLayout(
      DT::dataTableOutput(ns("citations")), 
      DT::dataTableOutput(ns("phrases"))
    )
  )
}

#' semantic module
#' 
#' @param input input
#' @param output output
#' @param session session
#' @param terms terms data set
#' @param articles articles data set
#' @param sentences sentences data set 
#' 
#' @importFrom dplyr desc
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2
#' @importFrom DT datatable
#' @importFrom stringr str_detect str_replace
#' @importFrom tidyr separate
#' @importFrom shiny withProgress incProgress renderPlot outputOptions
#' @export
cybergeo_module_semantic <- function( input, output, session, terms, articles, sentences ){

  terms_matched <- reactive({
    patterns <- input$patterns
      
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
    terms_matched()  %>%
      select(article_id) %>%
      distinct() %>%
      left_join(articles, by = c("article_id" = "id")) %>%
      arrange(article_id) %>%
      select( article_id, authors, title, title_en )
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
  
  all_phrases <- reactive({
    patterns <- input$patterns
    
    withProgress(min=0, max=length(patterns), value=0, message = "filtering sentences",  {
      if( !is.null( patterns ) ){
        datasets <- lapply( patterns , function( pattern ){
          res <- sentences %>% 
            filter(grepl(pattern, sentence, ignore.case = TRUE, perl = TRUE) ) %>% 
            mutate(sentence = gsub( sprintf( "(%s)", pattern), "<strong>\\1</strong>", sentence, ignore.case = TRUE ) )
          
          incProgress(1)
          res
        })
        sentences <- bind_rows(datasets)  %>% 
          arrange( article_id )
      }
    })
    sentences
  })
  
  phrases <- reactive({
    sentences <- all_phrases()
    
    selected_title <- input$citations_rows_selected
    if( !is.null(selected_title) ){
      selected_id <- titles_matched()$article_id[ selected_title ]
      sentences <- filter( sentences, article_id == selected_id)
    }
    
    sentences
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
      wordcloud2( as.data.frame(x) , shape = "square")  
    })
  })
  
  
  
  # Compute the Outputs
  output$chronogram <- renderPlot(chronogram())
  output$cloud <- renderWordcloud2(cloud())

  output$citations <- DT::renderDataTable( {
    datatable( titles_matched() , options = list(pageLength = 5), rownames = FALSE, selection = "single" ) 
  })
  output$phrases <- DT::renderDataTable( {
    datatable( phrases(), escape = FALSE, options = list(pageLength = 5), rownames = FALSE  )  
  })
  
}
