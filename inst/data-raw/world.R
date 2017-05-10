library(rgdal)
library(sp)
library(rgeos)

world_withZoom <- readOGR(dsn=system.file("data-raw/world_withZoom.shp", package = "corpusminer"), layer = "world_withZoom")

w <-readOGR(dsn= system.file( "data-raw/world.shp", package = "corpusminer"))
w <- w[ w@data$CNTR_ID %in% world_withZoom@data$CNTR_ID , ]
w@data$NAME <- world_withZoom@data$NAME[ match(w@data$CNTR_ID, world_withZoom@data$CNTR_ID)  ]
world <- SpatialPolygonsDataFrame(w, data = w@data)

save( world, file = "data/world.rda")
