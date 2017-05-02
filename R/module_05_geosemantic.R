

#' hierarchical clustering of countries
#'
#' This function produces a hierarchical clustering of countries with respect
#' to their frequency of themes it is used in the geosemantic tab to display
#' the groups of countries by themes and the corresponding average profiles of themes
#'
#' @param themes_By_country_bf dataframe in which lines represent country codes and
#'        columns represent the number of articles for each theme
#' @param numberOfGroups an integer giving the number of classes for the clustering
#' @param themes the list of themes of the analysis
#' @param groups_Country a vector of group IDs for each country
#'
#' @importFrom stats dist hclust cutree
#' @export
cahCountriesBasedOnTerms = function(themes_By_country_bf, numberOfGroups, themes){
  themesScaled <- scale(themes_By_country_bf[,themes])
  rownames(themesScaled) <- themes_By_country_bf[,1]
  d.themes <- dist(themesScaled)
  cah.themes <- hclust(d.themes, method = "ward.D2")
  cutree(cah.themes, k=numberOfGroups)
}

#' Aggregate countries based on terms
#'
#' This function summarises the number of articles by theme for each country it is used
#' in the reactive object 'clusterCountries' for every analysis at the country level
#'
#' @param themesFile a dataframe in which lines represent articles and columns include themes and country codes
#' @param themes the list of themes of the analysis
#' @param countries_to_aggregate the list of countries to aggregate articles by (taken from the shapeFile)
#'
#' @return a dataframe in which lines represent country codes and columns represent the number of articles for each theme
#' @export
aggregateCountriesBasedOnTerms <- function(themesFile, themes, countries_to_aggregate){
  
  themes_By_country_bf <- data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] <- NA
  themes_By_country_bf$n <- NA
  for (c in countries_to_aggregate){
    articles_to_aggregate <- themesFile[themesFile[,c] == 1, themes]
    if (nrow(articles_to_aggregate)){
      nArticles <- dim(articles_to_aggregate)[1]
      themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] <- colSums(articles_to_aggregate) / nArticles
      themes_By_country_bf[themes_By_country_bf$CountryID == c, "n"] <- nArticles
    }
  }
  
  themes_By_country_bf <- themes_By_country_bf[complete.cases(themes_By_country_bf),]
  themes_By_country_bf$CountryID <- substr(themes_By_country_bf$CountryID, 3,4)
  
  themes_By_country_bf
}

#' @export
load_geo_semantic_data <- function(terms_file, articles, countries){
  
  terms <- read.table( terms_file, sep = ",", dec = ".")
  
  A <- paste0( "A_", countries )
  S <- paste0( "S_", countries )
  
  data <- left_join(
    terms,
    select( articles, id, firstauthor, one_of(A), one_of(S)),
    by = "id"
  ) %>%
    filter( complete.cases(.) ) %>%
    mutate( firstauthor = as.character(firstauthor))
  
  themes <- setdiff( names(terms), "id")
  
  summary_terms <- function( data, themes, variables ){
    agg  <- aggregateCountriesBasedOnTerms( data, themes, variables)
    d <- dist( scale(agg[,-1]) )
    hc <- hclust( d, method = "ward.D2")
    
    list( data = agg, dist = d, hc = hc, themes = themes )
  }
  
  list(
    Authoring = summary_terms( data, themes, A),
    Studied   = summary_terms( data, themes, S)
  )
}



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
      w@data$CNTR_ID, data$n, groups, data$group_size, data$numberArticlesInGroup, plot_files[groups]) %>% lapply(HTML)
    
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