library("rdgdal")

world <- readOGR(dsn="data-raw/world_withZoom.shp",
  layer = "world_withZoom", encoding="utf8", verbose = FALSE
)
save( world, file = "data/world.rda")
