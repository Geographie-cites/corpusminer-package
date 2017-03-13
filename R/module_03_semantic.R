#' Matched terms list
#' 
#' Compute a dataframe of matched terms
#' 
#' @param patterns a string vector of regexp patterns to match
#' @param terms terms dataset
#' 
#' @return a data frame of matched terms
#' 
#' @importFrom tibble data_frame
terms_matched <- function(patterns, terms) {
  
  datasets <- lapply(patterns, function(pattern){
    indices <- grep( pattern, x = terms$term, ignore.case = TRUE, perl = TRUE)
    data_frame( id = indices, pattern = pattern)
  })
  bind_rows(datasets) %>%
    left_join(terms, by = "id") 
}

#' Matched articles list
#' @name titles_matched
#' @description Compute a list of articles containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a string vector of articles containing a matched term
titles_matched <- function(patterns) {
  citations <- terms_matched(patterns) %>%
    dplyr::select(article_id) %>%
    dplyr::unique() %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::arrange(date) %>%
    dplyr::select(citation)
  return(citations$citation)
}

#' @title Matched sentences list
#' @name phrases
#' @description Compute a list of sentences containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a vector of sentences containing a matched term
phrases <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, sentences$sentence, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      dplyr::bind_rows(data)
  }
  data <- data %>%
    dplyr::left_join(sentences, by = c("id")) %>%
    dplyr::select(sentence)
  return(data$sentence)
}

#' @title Metadata for each matched terms
#' @name terms_matched_cloud
#' @description Compute the metadata of each terms in order to build a wordcloud
#' @param patterns: a string vector of regexp patterns to match
#' Returns:
terms_matched_cloud <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(term) %>%
    #    summarise(articles = n_distinct(article_id), terms = sum(count))
    dplyr::summarise(articles = sum(count))
}

#' @title Metadata of each articles containing a matched term
#' @name articles_matched
#' @description Compute the metadata of each articles containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a dataframe of articles metadata
articles_matched <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(article_id, pattern) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::mutate(ym = str_sub(date, 1, 4)) %>%
    dplyr::group_by(ym, pattern) %>%
    dplyr::summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = parse_date_time(ym, "%y")) %>%
    dplyr::select(date, pattern, articles, terms)
}

#' @title Chronogramme
#' @name chronogram
#' @description Compute a chronogram graphic of articles
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a graphic
chronogram <- function(patterns) {
  ggplot(articles_matched(patterns), aes(date, articles)) +
    geom_bar(stat = "identity") +
    facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
    labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
}

#' @title Cloud of terms
#' @name cloud
#' @description Compute a word cloud of matched terms
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a graphic
cloud <- function(patterns) {
  words <- terms_matched_cloud(patterns)
  wordcloud(
    words$term,
    words$articles,
    scale = c(10,1),
    rot.per = 0
  )
}

### ------- module shiny

#' @export
cybergeo_module_semantic_UI <- function(id, pattern_list){
  ns <- NS(id)
  
  modes <- c( "Single Pattern Analysis" = "one", "Multiple Pattern Analysis" = "multi" )
  condition <- sprintf( "input['%s'] == 'multi'", ns("mode") )
  tabPanel("Full-text Semantic network",
    column( 3,
       # Select an option in order to compute the interface
       # (one or more patterns) and the outputs
       selectInput(ns("mode"), "Mode", modes),
  
       textInput( ns("pattern_input") , "Pattern"),
  
       conditionalPanel( condition,
         actionButton( ns("add_pattern"), "Add to Selection"),
         checkboxGroupInput( ns("patterns_selection"), "Pattern Selection", pattern_list)
       )


    ),
    column( 9,
      tabsetPanel(
        # show a chronogram of the number of match per year
        tabPanel("Chronogram", plotOutput(ns("chronogram"), height = "700px") ),
  
        # show a wordcloud of the matched items
        tabPanel( "Word Cloud", plotOutput(ns("cloud"), height = "700px") ),
  
        # show the sentences including a term that matches a pattern
        tabPanel("Sentences", verbatimTextOutput(ns("phrases"))),
  
        # show the articles including a term that matches a pattern
        tabPanel("Citations", verbatimTextOutput(ns("citations")))
       )
    )
  )
}

#' @export
cybergeo_module_semantic <- function( input, output, session, pattern_list ){
  
  patterns <- reactive({
    switch( input$mode,
      one = input$pattern_input,
      multi = input$patterns_selection
    )
  })
  
  # Ask for a new pattern to add in the list
  observeEvent( input$add_pattern, {
    new_pattern <- input$pattern_input
    pattern_list <<- c(new_pattern, pattern_list)
    updateTextInput(session, "pattern_input", value = "")

    updateCheckboxGroupInput(session, "patterns_selection",
      choices = pattern_list,
      selected = c(new_pattern, input$patterns_selection)
    )
  })
  
  # Compute the Outputs
  output$chronogram <- renderPlot(chronogram(patterns()))
  output$cloud <- renderPlot(cloud(patterns()))
  output$citations <- renderPrint(titles_matched(patterns()))
  output$phrases <- renderPrint(phrases(patterns()))

}
