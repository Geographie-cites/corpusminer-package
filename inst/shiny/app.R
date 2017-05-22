library(shiny)
library(corpusminer)

ui <- shinyUI(navbarPage( "CybergeoNetworks", id = "tabs",
  cybergeo_module_project_UI("project"),
  cybergeo_module_overview_UI("overview") ,
  cybergeo_module_citation_UI("citation") ,
  cybergeo_module_semantic_UI("semantic") ,
  cybergeo_module_keyword_UI("keyword", NETKW = NETKW ),
  cybergeo_module_geosemantic_UI("geosemantic")
))

server <- shinyServer(function(input, output, session) {
  
  callModule(cybergeo_module_overview, "overview",
    world = world, 
    articles = overview_ARTICLES
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

shinyApp(ui = ui, server = server)
