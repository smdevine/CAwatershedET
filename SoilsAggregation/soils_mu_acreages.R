#script to extract map unit land areas for each watershed based upon shapefiles
library(raster)
library(rgdal)
library(rgeos)
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
SpatialDir <- file.path(mainDir, 'soils_data/SSURGOdata')
results <- file.path(mainDir, 'soils_data/SSURGOrevised')
setwd(SpatialDir)
spatial_dirs <- list.dirs(SpatialDir)

dirnumber <- 2
watershed_name <- basename(spatial_dirs[dirnumber])
print(watershed_name)
outpath <- file.path(results, watershed_name)
print(outpath)
setwd(file.path(SpatialDir, watershed_name))
getwd()
shp <- shapefile(paste(watershed_name, '_mapunits.shp', sep = ''))
shp <- data.frame(shp)
colnames(shp)[4] <- 'mukey'
area_by_mu <- as.data.frame(tapply(shp$hectares, shp$mukey, sum))
colnames(area_by_mu) <- 'hectares'
area_by_mu$mukey <- rownames(area_by_mu)
row.names(area_by_mu) <- NULL
setwd(outpath)
write.csv(area_by_mu, paste(watershed_name, '_mu_area.csv', sep = ''), row.names = FALSE)
print(sum(area_by_mu$hectares))
