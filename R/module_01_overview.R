# functions used in Overview app

#' @importFrom tibble data_frame
#' @importFrom stringr str_count
#' @export
subset_articles <- function( articles, year_range ){
  articles %>%
    select( id, year, authors, citedby, citing, matches( "^._" ) ) %>%
    filter( between( year, year_range[1], year_range[2]) ) %>%
    mutate( nauthors = 1 + str_count(authors, "," ) )
}

pretty_years_interval <- function( years ){
  if( years[1] == years[2] ){
    years[1]
  } else {
    paste( years[1], years[2], sep = " - ")
  }
}

# constants used by overview app
overview_plot_prefix <- c(
  "A" = "Countries authoring Cybergeo articles",
  "S" = "Countries studied in Cybergeo articles",
  "L" = "Countries studied by locals in Cybergeo articles"
)
overview_plot_col <- c(A = "orange", S = "#1C6F91", L = "#df691a")

#' @importFrom utils head
overview_summary_country <- function(x){
  best <- head( sort(x, decreasing = TRUE), 5L)
  paste( names(best), " (", best, ")", collapse = ", ", sep = "")
}


subset_map_data <- function( world, articles, indicator){
  countries <- as.character( world@data$CNTR_ID )
  columns <- paste( indicator, countries, sep = "_")
  
  counts <-  colSums( articles[, columns ] )
  names(counts) <- gsub( "^.*_", "", names( counts) )
  
  counts <- counts[counts>0]
  
  keep <- match( names(counts), countries )
  w <- world[ keep, ]
  w
}

#' @importFrom leaflet leaflet addTiles setView addPolygons labelOptions highlightOptions
leaflet_overview <- function(world, articles, indicator = c("A", "S", "L"), authoring, studied ){
  indicator <- match.arg(indicator)
  col  <- overview_plot_col[[indicator]]
  w <- subset_map_data(world, articles, indicator)
  
  countries <- w@data$CNTR_ID
  country_names <- w@data$NAME
  nAuthoring <- authoring[countries]
  nStudied <- studied[countries]
  
  labels <- sprintf( "<strong>%s</strong><br/> %d articles authored<br/>studied by %d articles", country_names, nAuthoring, nStudied ) %>% lapply(HTML)
  
  leaflet(w) %>%
    addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
    setView(lng = 0, lat= 20, zoom=3) %>% 
    addPolygons( color = "black", weight = 1, fillColor = col, fill = TRUE, fillOpacity = 0.6,
      highlightOptions = highlightOptions(weight = 2, fillOpacity = 1,bringToFront = TRUE), 
      label = labels, 
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )
  
}

#' @importFrom leaflet leafletOutput
#' @export
cybergeo_module_overview_UI <- function(id){
  ns <- NS(id)
  
  tabPanel( "Overview",
    
    div( class = "outer", 
      leafletOutput( ns("leaflet"), width="100%", height="100%" )
    ), 
    absolutePanel( id = ns("controls"), class = "panel panel-default panel-side", 
      fixed = TRUE, draggable = TRUE, 
      top = 60, left = "auto", right = 20, bottom = "auto", 
      width = 350, height= "auto", 
      
      div( class = "panel-side", 
        h4( "Time range"), 
        sliderInput( ns("dateRange"), label = NULL,
          min = 1996, max = 2015, value = c(1996,2015),
          step = 1, animate=TRUE
        ), 
        
        div(
          textOutput( ns("nArticles"), inline = TRUE) , 
          " scientific articles from ", 
          textOutput( ns("nAuthors"), inline = TRUE) , 
          " authors"
        ), 
        br(), 
        
        div( 
          textOutput( ns("nAuthoring"), inline = TRUE) , 
          " authoring countries "
        ),
        
        div( style = "font-size: smaller", 
          "Top 5 : ", 
          textOutput(ns("top5Authoring"), inline = TRUE )
        ), 
        br(), 
        
        div( 
          textOutput( ns("nStudied"), inline = TRUE) , 
          " studied countries "
        ),
        div( style = "font-size: smaller", 
          "Top 5 : ", 
          textOutput(ns("top5Studied"), inline = TRUE )
        ), 
        br(), 
        
        div(
          textOutput( ns("nCitedBy"), inline = TRUE) ,
          " citations from other articles "
        ),
        
        div(
          textOutput( ns("nCiting"), inline = TRUE) ,
          " citations of other articles "
        ),
        
        h4( "Indicator to map"), 
        selectInput( ns("whatMapped"), label = NULL,
          choices=c("Authoring countries" = "A", "Countries Studied"= "S", "Countries Studies by Locals"= "L"),
          multiple=FALSE
        )
      )
    )
  )
  
}

#' @importFrom leaflet renderLeaflet
#' @export
cybergeo_module_overview <- function( input, output, session, world, articles ){
  
  # subset of the data from ARTICLES in the date interval
  data_overview <- reactive({
    subset_articles( articles, input$dateRange )
  })

  authoring <- reactive({
    x <- colSums( select(data_overview(), starts_with("A_")) )
    names(x) <- gsub( "^.*_", "", names(x))
    x
  })
  nAuthoringCountries <- reactive({
    sum( authoring() > 0)
  })
  
  studied <- reactive({
    x <- colSums( select(data_overview(), starts_with("S_")) )
    names(x) <- gsub( "^.*_", "", names(x))
    x
  })
  
  nStudiedCountries <- reactive({
    sum( studied() > 0)
  })
  
  citedby <- reactive({
    sum(data_overview()$citedby, na.rm = TRUE)
  })
  
  citing <- reactive({
    sum(data_overview()$citing, na.rm = TRUE)
  })
  
  # the main leaflet output
  output$leaflet <- renderLeaflet({
    leaflet_overview(world, data_overview(), input$whatMapped, authoring = authoring(), studied = studied() )
  })
  
  # various output in the absolute panel
  output$nArticles <- renderText( nrow(data_overview()) )
  output$nAuthors  <- renderText( sum(data_overview()$nauthors))
  
  output$nAuthoring <- renderText( nAuthoringCountries() )
  output$nStudied   <- renderText( nStudiedCountries() )
  
  output$nCitedBy <- renderText( citedby() )
  output$nCiting  <- renderText( citing() )
  
  output$top5Authoring <- renderText( overview_summary_country(authoring()) )
  
  output$top5Studied <- renderText( overview_summary_country(studied()) )

}
