library(shiny)

shinyUI(navbarPage( "CybergeoNetworks", theme = "darkBlue.css",
  cybergeo_module_project_UI("project"),
  cybergeo_module_overview_UI("overview"),
  cybergeo_module_citation_UI("citation", citation_cybergeodata = citation_cybergeodata),
  cybergeo_module_semantic_UI("semantic", pattern_list = pattern_list),
  cybergeo_module_keyword_UI("keyword", NETKW = NETKW ),
  cybergeo_module_geosemantic_UI("geosemantic")
))
