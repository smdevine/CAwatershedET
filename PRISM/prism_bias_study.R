#sf package in r
library(raster)
library(Kendall)
prism_historical <- 'D:/PRISM_ppt_stable_4kmM2_189501_198012_bil'
prism_modern <- 'D:/PRISM_ppt_stable_4kmM3_198101_201609_bil' #updated 5.12.2017 with WY2016 precip data
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
PRISMdir <- 'C:/Users/smdevine/Desktop/SpatialData/PRISM/PRISM_CA_ppt_stable'
boundariesDIR <- file.path('C:/Users/smdevine/Desktop/SpatialData/watershed_characteristics/watershed_boundaries_revised/final_boundaries_CA_TA_dissolved')
demDir <- 'C:/Users/smdevine/Desktop/SpatialData/PRISM/DEM'
P_vs_elevDir <- 'C:/Users/smdevine/Desktop/SpatialData/PRISM/results/bias_analysis/P_vs_elev_by_watershed'
rivername_converter <- 'C:/Users/smdevine/Desktop/SpatialData/PRISM/watershed_to_rivername.csv'

#put historical montly 4km prism files into a raster stack and save to file
setwd(prism_historical)
prism_files <- list.files(pattern = glob2rx('*.bil'))
length(prism_files)
prism_stack <- stack(prism_files)
setwd(californiaDir)
ca <- shapefile('california_CA_TA.shp')
ca_gcs <- spTransform(ca, crs(prism_stack))
setwd(PRISMdir)
crop(prism_stack, extent(ca_gcs), filename='PRISM_CA_1895_1980stack.grd', format='raster') #this is with guidance from http://stackoverflow.com/questions/26763013/r-write-rasterstack-and-preserve-layer-names the only solution to preserve layer names without writing layers individually
#do the same with the 1981-2016 monthly prism 4km data set
setwd(prism_modern)
prism_files <- list.files(pattern = glob2rx('*.bil'))
length(prism_files)
prism_stack <- stack(prism_files)
setwd(californiaDir)
ca <- shapefile('california_CA_TA.shp')
ca_gcs <- spTransform(ca, crs(prism_stack))
setwd(PRISMdir)
crop(prism_stack, extent(ca_gcs), filename='PRISM_CA_1981_2016stack.grd', format='raster') #this is with guidance from http://stackoverflow.com/questions/26763013/r-write-rasterstack-and-preserve-layer-names the only solution to preserve layer names without writing layers individually

#merge two prism stacks into one file
setwd(PRISMdir)
prism_stack_modern <- stack("PRISM_CA_1981_2016stack.grd")
prism_stack_historical <- stack("PRISM_CA_1895_1980stack.grd")
prism_stack_all <- stack(prism_stack_historical, prism_stack_modern)
writeRaster(prism_stack_all, 'PRISM_CA_1895_2016stack.grd', format='raster')

#crop prism DEM to CA
setwd(demDir)
elevation_US <- raster("PRISM_us_dem_4km_bil.bil")
setwd(californiaDir)
ca <- shapefile('california_CA_TA.shp')
ca_gcs <- spTransform(ca, crs(elevation_US))
setwd(demDir)
crop(elevation_US, ca_gcs, 'CA_dem_4km.grd', format='raster')

#calculate elevation vs. total water year precip across all years (1895-2016) for each watershed
setwd(demDir)
elevation_CA <- raster('CA_dem_4km.grd')
setwd(PRISMdir)
prism_stack_all <- stack('PRISM_CA_1895_2016stack.grd')
start_months <- seq(from = 10, to = 1461, by=12)
end_months <- seq(from=21, to=1461, by=12)
WY_months <- as.data.frame(cbind(start_months, end_months))
WY_months$water.year <- c(1896:2016) #as opposed to seq(from=1896, to=2016, by=1)
setwd(file.path(PRISMdir, 'water_year_sums'))
for (i in 1:nrow(WY_months)) {
  WY_sum <- sum(prism_stack_all[[WY_months$start_months[i]:WY_months$end_months[i]]])
  writeRaster(WY_sum, filename=paste('PRISM_ppt_stable_CA_WY', as.character(WY_months$water.year[i]), 'total.grd', sep=''))
}
prism_wy_files <- list.files(pattern = glob2rx('*.grd'))
prism_wy_stack <- stack(prism_wy_files) #for some reason names became layer.1, layer.2, etc.
names(prism_wy_stack) <- paste('WY', as.character(WY_months$water.year))
setwd(PRISMdir)
writeRaster(prism_wy_stack, "PRISM_CA_WY1896_WY2016stack.grd", format='raster', overwrite=TRUE)
plot(prism_wy_stack$WY.1896)
setwd(PRISMdir)
prism_wy_stack <- stack("PRISM_CA_WY1896_WY2016stack.grd")
#read in the water year stack
plot(elevation_CA)
plot(ca_gcs, add=T)
setwd(boundariesDIR)
watershed_fnames <- list.files(pattern = glob2rx('*.shp'))
for (i in 2:length(watershed_fnames)) {
  setwd(boundariesDIR)
  watershed_name <- gsub('_CA_TA_dissolved.shp', '', watershed_fnames[i])
  watershed_shp <- shapefile(watershed_fnames[i])
  watershed_shp <- spTransform(watershed_shp, crs(elevation_CA))
  watershed_elev <- extract(x=elevation_CA, y=watershed_shp, cellnumbers=TRUE, df=TRUE)
  colnames(watershed_elev)[3] <- 'elevation.m' 
  watershed_elev$ID <- NULL
  watershed_P <- extract(x=prism_wy_stack, y=watershed_shp, cellnumbers=TRUE, df=TRUE, layer=1, nl=nlayers(prism_wy_stack))
  watershed_P$ID <- NULL
  watershed_P_vs_elev <- merge(watershed_elev, watershed_P, by='cell')
  setwd(P_vs_elevDir)
  write.csv(watershed_P_vs_elev, paste(watershed_name, '_P_vs_elev.csv', sep=''), row.names = FALSE)
}

#read in p vs. elev files to calculate mean precip by year x watershed and orographic effect by year x watershed
setwd(P_vs_elevDir)
p_vs_elev_fnames <- list.files(pattern = glob2rx('*.csv'))
p_vs_elev <- lapply(p_vs_elev_fnames, read.csv, stringsAsFactors=FALSE)
p_vs_elev_fnames <- as.data.frame(p_vs_elev_fnames)
colnames(p_vs_elev_fnames) <- 'filenames'
p_vs_elev_fnames$watershed_name <- gsub('_P_vs_elev.csv', '', p_vs_elev_fnames$filenames)
rivernames <- read.csv(rivername_converter, stringsAsFactors = FALSE)
p_vs_elev_fnames <- merge(p_vs_elev_fnames, rivernames, by='watershed_name')
names(p_vs_elev) <- p_vs_elev_fnames$river_name
#calcuate mean P by water year x watershed
for (i in 1:length(p_vs_elev)) {
  if (i == 1) {
    meanPrecip <- as.matrix(apply(p_vs_elev[[i]][ ,3:ncol(p_vs_elev[[i]])], 2, mean))
    colnames(meanPrecip)[i] <- names(p_vs_elev)[i]
    next
  }
  meanPrecip <- cbind(meanPrecip, apply(p_vs_elev[[i]][ ,3:ncol(p_vs_elev[[i]])], 2, mean))
  colnames(meanPrecip)[i] <- names(p_vs_elev)[i]
}
meanPrecip <- as.data.frame(meanPrecip) #convert to a data.frame
meanPrecip$water.year <- as.integer(gsub("WY.", "", row.names(meanPrecip))) #create a water.year column
meanPrecip <- meanPrecip[ ,c(21, 1:20)] #put water.year into the first column
setwd(file.path(P_vs_elevDir, 'orographic_trends'))
write.csv(meanPrecip, 'meanPrecip1896_2015bywatershed.csv', row.names = FALSE)

#calculate orographic effect by water year x watershed and get results into one data.frame
for (i in 1:length(p_vs_elev)) {
  #rivername <- names(p_vs_elev)[i]
  if (i == 1) {
    for (j in 3:ncol(p_vs_elev[[i]])) {
      if (j == 3) {
        lm_p_vs_elev <- summary(lm(p_vs_elev[[i]][,j] ~ p_vs_elev[[i]][,2])) #j is in reference to the sequential columns of years, 2 is in reference to elevation column
        slopes_P_vs_elev <- cbind(as.integer(gsub("WY.", "", colnames(p_vs_elev[[i]])[j])), round(lm_p_vs_elev$coefficients[2], 5))
        #colnames(slopes_P_vs_elev) <- c('water.year',  names(p_vs_elev)[i])
        next
      }
      lm_p_vs_elev <- summary(lm(p_vs_elev[[i]][,j] ~ p_vs_elev[[i]][,2]))
      slopes_P_vs_elev <- rbind(slopes_P_vs_elev, cbind(as.integer(gsub("WY.", "", colnames(p_vs_elev[[i]])[j])), round(lm_p_vs_elev$coefficients[2], 5)))
    }
    next
  }
  for (j in 3:ncol(p_vs_elev[[i]])) {
    if (j==3) {
      lm_p_vs_elev <- summary(lm(p_vs_elev[[i]][,j] ~ p_vs_elev[[i]][,2]))
      slopes_P_vs_elev_add <- round(lm_p_vs_elev$coefficients[2], 5)
      next
    }
    lm_p_vs_elev <- summary(lm(p_vs_elev[[i]][,j] ~ p_vs_elev[[i]][,2]))
    slopes_P_vs_elev_add <- c(slopes_P_vs_elev_add, round(lm_p_vs_elev$coefficients[2], 5))
  }
  slopes_P_vs_elev <- cbind(slopes_P_vs_elev, slopes_P_vs_elev_add)
}
slopes_P_vs_elev <- as.data.frame(slopes_P_vs_elev)
colnames(slopes_P_vs_elev) <- c('water.year', names(p_vs_elev))
setwd(file.path(P_vs_elevDir, 'orographic_trends'))
write.csv(slopes_P_vs_elev, 'orographic_slopes_1896_2016.csv', row.names = FALSE)

#read in results to calculate temporal trends by watershed with variable start and stop times
setwd(file.path(P_vs_elevDir, 'orographic_trends'))
slopes_P_vs_elev <- read.csv('orographic_slopes_1896_2016.csv', stringsAsFactors = FALSE)
mktest_allyrs <- apply(slopes_P_vs_elev[ ,2:21], 2, MannKendall)
mktest_1905_2016 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1905):nrow(slopes_P_vs_elev), 2:21], 2, MannKendall)
mktest_1920_2016 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1920):nrow(slopes_P_vs_elev), 2:21], 2, MannKendall)
mktest_1950_2016 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1950):nrow(slopes_P_vs_elev), 2:21], 2, MannKendall)
mktest_1970_2016 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1970):nrow(slopes_P_vs_elev), 2:21], 2, MannKendall)
mktest_1980_2016 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1980):nrow(slopes_P_vs_elev), 2:21], 2, MannKendall)
mktest_1970_2010 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1970):which(slopes_P_vs_elev$water.year==2010), 2:21], 2, MannKendall)
mktest_1950_2010 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1950):which(slopes_P_vs_elev$water.year==2010), 2:21], 2, MannKendall)
mktest_1920_2010 <- apply(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1920):which(slopes_P_vs_elev$water.year==2010), 2:21], 2, MannKendall)

#plot the orographic effect trends
for (i in 1:20) {
  lm_model <- lm(slopes_P_vs_elev[ ,i+1] ~ slopes_P_vs_elev$water.year)
  lm_model1920 <- lm(slopes_P_vs_elev[which(slopes_P_vs_elev$water.year==1920):nrow(slopes_P_vs_elev),i+1] ~  slopes_P_vs_elev$water.year[which(slopes_P_vs_elev$water.year==1920):nrow(slopes_P_vs_elev)])
  print(colnames(slopes_P_vs_elev)[i+1])
  print(summary(lm_model))
  print(MannKendall(slopes_P_vs_elev[ ,i+1]))
  plot(slopes_P_vs_elev[ ,i+1] ~ slopes_P_vs_elev$water.year, type='b', main=colnames(slopes_P_vs_elev)[i+1], xlab = 'Water Year', ylab='Orographic Effect (delta mm Precip / m elevation)')
  abline(lm_model, lty='dashed', col='blue', lwd=2)
  abline(lm_model1920, lty='dashed', col='red', lwd=2)
  #legend()
}

#compare mean P and orographic effects by watershed
for (i in 1:20) {
  meanP_vs_orographic <- lm(meanPrecip[ ,i+1] ~ slopes_P_vs_elev[ ,i+1])
  plot(meanPrecip[ ,i+1] ~ slopes_P_vs_elev[ ,i+1], ylab='Total Precip by Water Year (mm)', xlab='Orographic Effect by Water Year (mm Precip/m elevation)', type='p', main=colnames(meanPrecip)[i+1])
  abline(meanP_vs_orographic, lty='dashed', col='blue', lwd=2)
}



#test watershed plotting on PRISM data
setwd(boundariesDIR)
watershed_fnames <- list.files(pattern = glob2rx('*.shp'))
watershed_fnames
kaweah <- shapefile(watershed_fnames[1])
kaweah_gcs <- spTransform(kaweah, crs(prism_stack))
plot(prism_stack_modern$PRISM_ppt_stable_4kmM3_198101_bil, ext=extent(kaweah_gcs))
plot(kaweah_gcs, add=T)
names(prism_stack_modern)

watershed <- shapefile(watershed_fname) #function arg
FRAPveg <- crop(FRAPveg, watershed) #save memory
FRAP_crs <- crs(FRAPveg)
raster_object <- projectExtent(FRAPveg, FRAP_crs)
origin(raster_object) <- origin(FRAPveg)
