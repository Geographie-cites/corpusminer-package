shinyServer(function(input, output, session) {

  callModule(cybergeo_module_overview, "overview",
    world = world, articles = overview_ARTICLES
  )

  callModule(cybergeo_module_citation, "citation",
    citation_cybergeodata = citation_cybergeodata, 
    citation_edges = citation_edges, 
    citation_data = citation_data, 
    citation_keyword_data = citation_keyword_data
  )

  callModule(cybergeo_module_semantic, "semantic",
    terms = terms,
    articles = articles,
    sentences = sentences
  )

  callModule(cybergeo_module_keyword, "keyword",
    NETKW = NETKW
  )

  callModule(cybergeo_module_geosemantic, "geosemantic",
    geo_semantic_data = geo_semantic_data,
    world = world
  )

})
