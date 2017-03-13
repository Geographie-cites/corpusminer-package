

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


#' Computes the average frquencies of themes by cah group
#'
#' it is used in the legend of the cah map
#'
#' @param x a dataframe of theme frequency by country (themes_By_country_bf)
#'          in which lines represent country codes and columns represent
#'          the number of articles for each themes
#' @param y a vector of group numbers the length of the dataframe rows
#' @return a dataframe in which lines represent cah groups of country and columns represent the frequency of articles for each theme
#' @export
stat.comp <- function(x, y){
  K <- length(unique(y))
  n <- length(x)
  m <- mean(x)
  TSS <- sum((x-m)^2)
  nk <- table(y)
  mk <- tapply(x,y,mean)
  BSS <- sum(nk* (mk-m)^2)
  result <- c(mk,100.0*BSS/TSS)
  names(result) <- c( paste("G",1:K),"% epl.")
  return(result)
}

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
  
  navbarMenu("Geo-semantic Networks",
    tabPanel("Geo-semantic Networks",
      fluidRow(
        column(4, selectInput(ns("semanticMethod"), label = "Semantic Method", choices = c("Citations", "Keywords", "Semantic"), multiple = F)),
        column(4, selectInput(ns("aggregationMethod"), label = "Set of Countries",choices = c("Authoring", "Studied"), selected = "Studied", multiple = F)),
        column(4, sliderInput(ns("nClassifGroups"), label = "Number of Clusters", min = 1, max = 8, value = 4, step = 1), animate=T)
      ),
      plotOutput(ns("termsXCountriesMap")),
      plotOutput(ns("termsXCountriesLegend"))
    ),
    tabPanel("User guide", includeMarkdown("doc/GeoSemanticNetworks.md") )
  )
  
}

#' @export
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
  
  output$termsXCountriesMap = renderPlot({
    groupsOfCountries <- input$nClassifGroups
    
    country_id <- world@data$CNTR_ID
    
    cahRes <- cahCountries()
    groups <- cahRes$group[ match(country_id, cahRes$ID ) ]
    col <- paletteCybergeo[ groups ]
    
    par(mfrow=c(1,1), mar = c(0,0,1,0), bg="#2b3e50")
    plot(world, col=col, border="white", lwd=0.7)
    title("Groups of countries based on semantic networks", col.main = "white")
  })
  
  output$termsXCountriesLegend = renderPlot({
    clusters <- clusterCountries()
    
    groups <- cahCountries()$group
    
    data <- mutate( clusters$data, group = groups ) %>%
      group_by( group )
    
    nArticlesByGroup <- data %>%
      summarise( n = sum(n, na.rm = TRUE) )
    
    leg <- data %>%
      summarise_at(vars(one_of(clusters$themes)), mean) %>%
      select(-group) %>%
      as.matrix
    
    groupsOfCountries <- input$nClassifGroups
    mfrow <- c( ceiling(groupsOfCountries/2), 2 )
    
    par(mfrow=mfrow, las=2, mar = c(4,10,2,1), bg="#2b3e50")
    for(i in 1:groupsOfCountries){
      barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes", col.lab="white", col.axis="white")
      axis(1, col = "white", col.axis = "white")
      if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"), col.main = "white")
      if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"), col.main = "white")
    }
    
  })
  
  
}