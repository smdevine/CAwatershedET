#script to create four climate classes across all watersheds based upon normalized precipitation and temperature for science communication purposes
library(raster)
library(geosphere)
library(rgeos)
library(rgdal)
library(sp)
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
CA_BCM_tmin <- file.path(mainDir, 'CA_BCM_data/tmn1981_2010_ave_HST')
CA_BCM_tmax <- file.path(mainDir, 'CA_BCM_data/tmx1981_2010_ave_HST')
CA_BCM_ppt <- file.path(mainDir, 'CA_BCM_data/ppt1981_2010_ave_HST')
boundariesDIR <- file.path(mainDir, 'watershed_boundaries_revised/final_boundaries_CA_TA_dissolved')
CA_BCM_pet <- file.path(mainDir, 'CA_BCM_data/pet1981_2010_ave_HST')
outpath <- file.path(mainDir, 'watershed_characteristics/watershed_synthesis/CA_BCM_analysis')
setwd(boundariesDIR)
watersheds <- list.files(pattern = glob2rx('*.shp'))
for (i in 1:length(watersheds)) {
  if (i==1) {
    watersheds_all <- shapefile(watersheds[i])
    next
  }
  watersheds_all <- bind(watersheds_all, shapefile(watersheds[i]))
}
watersheds_all <- gUnaryUnion(watersheds_all)
setwd(CA_BCM_tmin)
cabcm_tmin <- raster('tmn1981_2010_ave_HST_1473451234.tif') #avg (1981-2010) tmin by month 
cabcm_tmin <- crop(cabcm_tmin, watersheds_all)
setwd(CA_BCM_tmax)
cabcm_tmax <- raster('tmx1981_2010_ave_HST_1473451213.tif')
cabcm_tmax <- crop(cabcm_tmax, watersheds_all)
cabcm_tavg <- (cabcm_tmax + cabcm_tmin)/2
plot(cabcm_tavg)
plot(watersheds_all, add=T)
setwd(CA_BCM_ppt)
cabcm_ppt <- raster('ppt1981_2010_ave_HST_1473451030.tif')
cabcm_ppt <- crop(cabcm_ppt, watersheds_all)
setwd(CA_BCM_pet)
pet <- raster('pet1981_2010_ave_HST_1473452792.tif')
pet <- crop(pet, watersheds_all)
climate_brick <- brick(cabcm_tavg, cabcm_ppt, pet)
climate_brick <- mask(climate_brick, watersheds_all)
names(climate_brick) <- c('MAAT_C', 'annual_ppt_mm', 'annual_pet_mm')
climate_brick$clim_def <- climate_brick$annual_ppt_mm - climate_brick$annual_pet_mm


#inspect the data
hist(climate_brick$annual_ppt_mm)
hist(climate_brick$MAAT_C)
hist(climate_brick$annual_pet_mm)
hist(climate_brick$clim_def)

#normalize the climate defict info
clim_def_mean <- cellStats(climate_brick$clim_def, mean)
clim_def_sd <- cellStats(climate_brick$clim_def, sd)
clim_def_norm <- (climate_brick$clim_def - clim_def_mean)/clim_def_sd
hist(clim_def_norm)

#normalize the precip data to mean=0 and stdev=1
ppt_mean <- cellStats(climate_brick$annual_ppt_mm, mean)
ppt_sd <- cellStats(climate_brick$annual_ppt_mm, sd)
annual_ppt_norm <- (climate_brick$annual_ppt_mm-ppt_mean)/ppt_sd
hist(annual_ppt_norm)

#normalize logtransformed precip data to mean=0 and stdev=1
ppt_log <- log(climate_brick$annual_ppt_mm)
ppt_logmean <- cellStats(ppt_log, mean)
ppt_logsd <- cellStats(ppt_log, sd)
annual_ppt_lognorm <- (ppt_log-ppt_logmean)/ppt_logsd
hist(annual_ppt_lognorm)

#normalize the temperature data to mean=0 and stdev=1
temp_mean <- cellStats(climate_brick$MAAT_C, mean)
temp_sd <- cellStats(climate_brick$MAAT_C, sd)
annual_temp_norm <- (climate_brick$MAAT_C-temp_mean)/temp_sd
hist(annual_temp_norm)

#convert normalized data to a data.frame and run the kmeans cluster analysis
climate_brick_norm <- brick(annual_temp_norm, annual_ppt_lognorm)
names(climate_brick_norm) <- c('MAAT_norm', 'annual_ppt_lognorm')
climate_norm_df <- as.data.frame(climate_brick_norm)
plot(climate_norm_df)
set.seed(3)
km.out.norm <- kmeans(na.omit(climate_norm_df), 4, nstart = 100)
plot(na.omit(climate_norm_df), col =(km.out.norm$cluster + 1))
climate_clusters <- rep(NA, length(climate_norm_df[,1]))
climate_clusters[!is.na(climate_norm_df[,1])] <- km.out.norm$cluster
raster_object <- raster(extent(climate_brick), resolution=res(climate_brick))
climate_clusters <- setValues(raster_object, climate_clusters)
plot(climate_clusters)
plot(watersheds_all, add=T)

#add climate clusters to unmodified data
climate_brick$climate_class <- climate_clusters
crs(climate_brick$climate_class) <- crs(climate_brick$MAAT_C)
climate_brick1 <- climate_brick[climate_brick$climate_class==1]
summary(climate_brick1)
climate_brick2 <- climate_brick[climate_brick$climate_class==2]
summary(climate_brick2)
climate_brick3 <- climate_brick[climate_brick$climate_class==3]
summary(climate_brick3)
climate_brick4 <- climate_brick[climate_brick$climate_class==4]
summary(climate_brick4)

#write climate brick to file
setwd(outpath)
writeRaster(climate_brick, filename = names(climate_brick), bylayer=TRUE, format='GTiff', overwrite=TRUE)
