library(shiny)

paletteCybergeo <- c("#1C6F91", "#df691a", "#77c5ba", "orange", "#2db92d", "#e1ff2f", "#ff2313", "#bbab61")

pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction",
  "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité",
  "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")

semanticcolors = list(rgb(204,0,255,maxColorValue=255),rgb(255,102,0,maxColorValue=255), rgb(255,102,0,maxColorValue=255),
  rgb(255,153,0,maxColorValue=255),rgb(0,204,102,maxColorValue=255),rgb(255,0,0,maxColorValue=255),
  rgb(153,153,0,maxColorValue=255),rgb(102,204,0,maxColorValue=255),rgb(0,255,255,maxColorValue=255),
  rgb(255,255,0,maxColorValue=255),rgb(51,102,255,maxColorValue=255),rgb(51,255,51,maxColorValue=255),
  rgb(0,102,0,maxColorValue=255),rgb(0,0,255,maxColorValue=255),rgb(102,51,0,maxColorValue=255)
)
names(semanticcolors)<-c("complex systems","health","crime",
  "statistical methods","remote sensing","political sciences/critical geography",
  "traffic modeling","microbiology","cognitive sciences",
  "spatial analysis","GIS","biogeography",
  "environnment/climate","economic geography","physical geography")

shinyUI(navbarPage( "CybergeoNetworks", id = "tabs",
  cybergeo_module_project_UI("project"),
  cybergeo_module_overview_UI("overview") ,
  cybergeo_module_citation_UI("citation", citation_cybergeodata = citation_cybergeodata) ,
  cybergeo_module_semantic_UI("semantic", pattern_list = pattern_list) ,
  cybergeo_module_keyword_UI("keyword", NETKW = NETKW ),
  cybergeo_module_geosemantic_UI("geosemantic")
))
