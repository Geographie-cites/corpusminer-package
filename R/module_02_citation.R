#' load citation edges given an reference id
#'
#' @param con database connection
#' @param id edge id
#'
#' @importMethodsFrom DBI dbGetQuery
citationLoadEdges <- function(con, id){
  query <- sprintf( "SELECT * FROM edges WHERE `from`='%s' OR `to`='%s' ;", id, id )
  dbGetQuery(con,query)
}

#' load neighbors keywords given an id
#'
#' @param con_citation connection to the citation db
#' @param con_keywords connection to the keywords db
#' @param id article id
#'
#' @return keywords
#' @importMethodsFrom DBI dbGetQuery
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

#' plots word clouds, one for the keywords of the ref itself, the other for the provided keywords (neighborhood)
#'
#' @importFrom graphics par
#' @importFrom wordcloud wordcloud
citationWordclouds<-function(id, keywords){
  if(id != "0" && !is.null(keywords)){
    # at least kws for the paper, so no need to check emptyness
    par(mfrow=c(1,2), bg = "#4e5d6c")
    wordcloud(words=keywords[[id]],
              freq=citationkwfreqs[keywords[[id]]],
              colors=unlist(semanticcolors[citationkwthemdico[keywords[[id]]]]),
              ordered.colors = TRUE
    )

    allkws=unlist(keywords)
    wordcloud(words=allkws,
              freq=citationkwfreqs[allkws],
              colors=unlist(semanticcolors[citationkwthemdico[allkws]]),
              ordered.colors = TRUE
    )
  }
}


#' @export
cybergeo_module_citation_UI <- function(id, citation_cybergeodata ){
  ns <- NS(id)

  navbarMenu("Citation network",

    tabPanel("Citation Network",
      # select article to visualize
      fluidRow(
        h4("Data Selection"),
        tags$p(class="text-justify","Search and select a cybergeo paper in the table."),
        dataTableOutput( ns("citationcybergeo") )
      ),

      # citation ego network
      fluidRow(
        h4("Citation network neighborhood"),
        tags$p(class="text-justify","This graph shows the citation neighborhood of the selected paper"),
        selectInput( ns("citationselected"), label = "Select a publication by id",
          choices = c("",sort(citation_cybergeodata$id[citation_cybergeodata$linknum>0],decreasing = TRUE)),
          selected = "", multiple = FALSE
        ),
        plotOutput( ns("citationegoplot"), width = "100%", height = "800px")
      ),

      # word clouds of semantic content
      fluidRow(
        h4("Semantic content"),
        tags$p(class="text-justify","This graph shows the semantic content (color legend in user guide) of the paper (left) and its neighborhood (right)."),
        selectInput( ns("citationsemanticselected"), label = "Select a publication by id",
          choices = c("",sort(citation_cybergeodata$id[citation_cybergeodata$kwcount>0],decreasing = TRUE)),
          selected = "", multiple = FALSE
        ),
        plotOutput( ns("citationesemanticplot"), width = "100%", height = "800px")
      )
    ),

    # svg viusalization of the full semantic network
    tabPanel("Semantic Network",
      h4("Full Semantic Network"),
      svgPanZoomOutput(ns("citationsemanticnw"), width = "100%", height = "100%")
    ),

    # user guide
    tabPanel("User guide",
      includeMarkdown("doc/CitationNetwork.md")
    )

  )

}

#' @importFrom RSQLite dbConnect SQLite
#' @export
cybergeo_module_citation <- function( input, output, session, citation_cybergeodata){

  citationdbcit <- dbConnect(SQLite(), system.file("sqlite", "CitationNetwork.sqlite3", package = "corpusminer"))
  citationdbkws <- dbConnect(SQLite(), system.file("sqlite", "CitationKeywords.sqlite3", package = "corpusminer"))

  # global vars (needed e.g. to avoid numerous db request with reactive functions)
  citationGlobalVars <- reactiveValues(
    citationSelected = "0",
    citationSemanticSelected = "0"
  )

  ## selection datatable
  output$citationcybergeo <- renderDataTable({
    citation_cybergeodata %>%
      filter(linknum > 0 | kwcount > 0) %>%
      select(id, SCHID, title, authors)
  })

  citationSelectedCybergeoArticle <- reactive({
    input$citationcybergeo_rows_selected
  })

  # observer make data update requests
  observe({
    selected <- citationSelectedCybergeoArticle()
    selected_hand <- which(citation_cybergeodata$id == input$citationselected)
    if(length(selected_hand)>0){
      selected <- selected_hand
    }
    if(length(selected) == 1){
      if(selected != citationGlobalVars$citationSelected ){
        citationGlobalVars$citationSelected <- selected

        selectedschid <- citation_cybergeodata$SCHID[as.numeric(selected)]

        # make request for edges in sqlitedb
        citationGlobalVars$edges = citationLoadEdges(citationdbcit, selectedschid)
      }
    }
  })


  # similar observer for semantic plot
  observe({
    selected <- citationSelectedCybergeoArticle()
    selected_hand <- which(citation_cybergeodata$id == input$citationsemanticselected)
    if(length(selected_hand)>0){
      selected <- selected_hand
    }
    if(length(selected)==1){
      if(selected != citationGlobalVars$citationSemanticSelected){
        citationGlobalVars$citationSemanticSelected <- selected
        selectedschid <- citation_cybergeodata$SCHID[as.numeric(selected)]
        citationGlobalVars$keywords <- citationLoadKeywords(citationdbcit, citationdbkws, selectedschid)
      }
    }
  })

  # render citation graph around selected article
  output$citationegoplot = renderPlot({
    citationVisuEgo(citationGlobalVars$edges)
  })

  # render wordclouds
  output$citationesemanticplot = renderPlot({
    schid <- citation_cybergeodata$SCHID[citationGlobalVars$citationSemanticSelected]
    citationWordclouds(schid,citationGlobalVars$keywords)
  })

  output$citationsemanticnw<-renderSvgPanZoom({
    svgPanZoom(
      'data/semantic.svg',
      zoomScaleSensitivity=1, minZoom=2, maxZoom=20, contain=TRUE
    )
  })


}
