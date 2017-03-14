library("rdgdal")

world <- readOGR(dsn="data-raw/world_withZoom.shp", layer = "world_withZoom", encoding="utf8", verbose = F)
save( world, file = "data/world.rda")
