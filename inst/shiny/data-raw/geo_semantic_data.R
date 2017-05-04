library("corpusminer")

countries = as.character(world@data$CNTR_ID)
geo_semantic_data <- list(
  Keywords  = load_geo_semantic_data( "data-raw/themes_keyword.csv", overview_ARTICLES, countries ),
  Citations = load_geo_semantic_data( "data-raw/themes_citation.csv", overview_ARTICLES, countries ),
  Semantic  = load_geo_semantic_data( "data-raw/themes_semantic.csv", overview_ARTICLES, countries )
)
save( geo_semantic_data, file = "data/geo_semantic_data.rda" )
