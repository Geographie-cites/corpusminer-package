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
#' @export
terms_matched <- function(patterns, terms) {
  datasets <- lapply(patterns, function(pattern){
    indices <- grep( pattern, x = terms$term, ignore.case = TRUE, perl = TRUE)
    data_frame( id = indices, pattern = pattern)
  })
  bind_rows(datasets) %>%
    left_join(terms, by = "id")
}

#' Matched articles list
#'
#' Compute a list of articles containing a matched term
#' @name titles_matched
#' @param patterns a string vector of regexp patterns to match
#' @return a string vector of articles containing a matched term
#' @export
titles_matched <- function(patterns, terms, articles) {
  citations <- terms_matched(patterns) %>%
    select(article_id) %>%
    distinct() %>%
    left_join(articles, by = c("article_id" = "id")) %>%
    arrange(date) %>%
    select(citation)
  return(citations$citation)
}

#' Matched sentences list
#'
#' Compute a list of sentences containing a matched term
#' @name phrases
#' @param patterns a string vector of regexp patterns to match
#' @return a vector of sentences containing a matched term
#'
#' @export
phrases <- function(patterns, sentences) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, sentences$sentence, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      bind_rows(data)
  }
  data <- data %>%
    left_join(sentences, by = "id")
  return(data$sentence)
}

#' Metadata for each matched terms
#'
#' Compute the metadata of each terms in order to build a wordcloud
#' @name terms_matched_cloud
#' @param patterns a string vector of regexp patterns to match
#' @export
terms_matched_cloud <- function(patterns) {
  terms_matched(patterns) %>%
    group_by(term) %>%
    summarise(articles = sum(count))
}

#' Metadata of each articles containing a matched term
#'
#' Compute the metadata of each articles containing a matched term
#' @name articles_matched
#' @param patterns a string vector of regexp patterns to match
#' @param articles articles data frame
#' @return a dataframe of articles metadata
#' @export
articles_matched <- function(patterns, articles, terms) {
  terms_matched(patterns, terms) %>%
    group_by(article_id, pattern) %>%
    summarise(count = sum(count)) %>%
    left_join(articles, by = c("article_id" = "id")) %>%
    mutate(ym = str_sub(date, 1, 4)) %>%
    group_by(ym, pattern) %>%
    summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
    ungroup() %>%
    mutate(date = parse_date_time(ym, "%y")) %>%
    select(date, pattern, articles, terms)
}

#' Chronogramme
#'
#' Compute a chronogram graphic of articles
#' @name chronogram
#' @param patterns: a string vector of regexp patterns to match
#' @export
chronogram <- function(patterns, articles, terms) {
  matched <- articles_matched(patterns, articles, terms)
  ggplot(matched, aes(date, articles)) +
    geom_bar(stat = "identity") +
    facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
    labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
}

#' Cloud of terms
#'
#' Compute a word cloud of matched terms
#' @name cloud
#' @param patterns a string vector of regexp patterns to match
#' @export
cloud <- function(patterns) {
  words <- terms_matched_cloud(patterns)
  wordcloud( words$term, words$articles, scale = c(10,1), rot.per = 0)
}

### ------- module shiny

#' shiny module for the semantic tab
#'
#' @param id see \code{\link[shiny]{callModule}}
#' @param pattern_list pattern list
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

#' Shiny module server function for the semantic tab
#' @param input input
#' @param output output
#' @param session session
#' @param pattern_list pattern list
#'
#' @export
cybergeo_module_semantic <- function( input, output, session, pattern_list, terms, articles, sentences ){

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
  output$chronogram <- renderPlot(chronogram(patterns(), articles, terms))
  output$cloud <- renderPlot(cloud(patterns()))
  output$citations <- renderPrint(titles_matched(patterns(), terms, articles))
  output$phrases <- renderPrint(phrases(patterns(), sentences))

}
