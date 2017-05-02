shinyServer(function(input, output, session) {

  callModule(cybergeo_module_overview, "overview",
    world = world_nozoom, articles = ARTICLES
  )

  callModule(cybergeo_module_citation, "citation",
    citation_cybergeodata = citation_cybergeodata
  )

  callModule(cybergeo_module_semantic, "semantic",
    pattern_list = pattern_list,
    terms = terms,
    articles = articles,
    sentences = sentences
  )

  callModule(cybergeo_module_keyword, "keyword",
    NETKW = NETKW
  )

  callModule(cybergeo_module_geosemantic, "geosemantic",
    geo_semantic_data = geo_semantic_data,
    world = world_nozoom
  )

})
