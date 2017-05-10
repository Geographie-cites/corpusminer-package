


#### ------------ module shiny

#' @export
cybergeo_module_geosemantic_UI <- function(id){
  ns <- NS(id)
  
  tabPanel("Geo-semantic Networks",
    div( class = "outer", 
      leafletOutput( ns("leaflet"), width="100%", height="100%" )
    ), 
    
    absolutePanel( id = ns("controls"), class = "panel panel-default panel-side", 
      fixed = TRUE, draggable = TRUE, 
      top = 60, left = "auto", right = 20, bottom = "auto", 
      width = 350, height= "auto", 
      
      div( class = "panel-side", 
        selectInput(ns("semanticMethod"), label = "Semantic Method", choices = c("Citations", "Keywords", "Semantic"), multiple = F),
        selectInput(ns("aggregationMethod"), label = "Set of Countries",choices = c("Authoring", "Studied"), selected = "Studied", multiple = F),
        sliderInput(ns("nClassifGroups"), label = "Number of Clusters", min = 1, max = 8, value = 4, step = 1)
      )
    )
  )
  
}

#' @export
#' @importFrom sp plot
cybergeo_module_geosemantic <- function( input, output, session, geo_semantic_data, world ){
  
  clusterCountries <- reactive({
    geo_semantic_data[[ c(input$semanticMethod, input$aggregationMethod ) ]]
  })
  
  cahCountries <- reactive({
    clusters <- clusterCountries()
    data_frame(
      ID = clusters$data[,1],
      group = cutree( clusters$hc, k = input$nClassifGroups )
    )
  })
  
  output$leaflet <- renderLeaflet({
    countries <- as.character( world@data$CNTR_ID )
    ngroups <- input$nClassifGroups
    
    clusters <- clusterCountries()
    
    cah  <- cahCountries()
    groups <- cah$group
    
    # only keep part of the map we need
    keep    <- match( cah$ID, countries )
    w <- world[ keep, ]
    
    data <- mutate( clusters$data, group = groups ) %>%
      group_by( group ) %>%
      mutate( numberArticlesInGroup = sum(n), group_size = n() )
    
    # summarise themes per group
    leg <- data %>%
      summarise_at(vars(one_of(clusters$themes)), mean) %>%
      select(-group) %>%
      as.matrix
    
    # render the plot that is used in the label of countries of each group
    plot_files <- lapply( 1:ngroups, function(i){
      tf <- tempfile(fileext=".png") 
      on.exit(unlink(tf))
      png( tf, bg = "transparent" )
      par( mar = c(4,10,1,1) )
      barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes", las = 1)
      axis(1)
      dev.off()
      
      session$fileUrl( file = tf, contentType = "image/png")
    })
    
    # make label with information on each country and the group it belongs to
    labels <- sprintf( "<strong>%s</strong> (%d articles) <br/> cluster %d (%d countries, %d articles)<br/> <img src='%s'>", 
      w@data$NAME, data$n, groups, data$group_size, data$numberArticlesInGroup, plot_files[groups]) %>% lapply(HTML)
    
    # leaflet map
    leaflet(w) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 0, lat= 20, zoom=3) %>% 
      addPolygons( color = "black", weight = 1, fillColor = paletteCybergeo[groups], fill = TRUE, fillOpacity = .8, 
        highlight = highlightOptions(weight = 2, fillOpacity = 1, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
    
  })
    
  
}