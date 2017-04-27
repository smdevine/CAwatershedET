#first purpose of this script is to merge multiple spatial data sets from diverse sources into a common framework watershed by watershed for later modeling purposes (i.e. same raster structure for each individual watershed).  it was deemed not practical to make one mega-raster for the whole study domain because of memory loading problems on my computer...
#the sections commented out in lines 7-170 are functional sections of code.  they were only commented out as new data sources were added that required re-running the function
#second purpose (lines 174-247) was to produce new sets of soil rasters for each watershed by subsituing the soil raster mukey with tabular data.  this required merging SSURGO with STATSGO where necessary as a first step.  soil tabular data was from previous work in other scripts to aggregate soils data.
#third purpose was to summarize mean watershed characteristics for the whole watershed and in different climatic zones of each watershed (lines 251-362)
#fourth purpose was to summarize characteristics across all watersheds by climate zone (lines 363-391)
#fifth purpose was to make some plots for QE presentation
#final result saved on line approx 335 to 'final_results' subdir; modified this subdir on 3/13/17
library(raster)
library(rgdal)
library(rgeos)
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
DEMdir <- file.path(mainDir, 'DEM/data')
#DEMfiles <- #needs to be defined for each watershed
FRAPdir <- file.path(mainDir, 'FRAP')
boundariesDIR <- file.path(mainDir, 'watershed_characteristics/watershed_boundaries_revised/final_boundaries_CA_TA_dissolved')
SSURGOdir <- file.path(mainDir, 'soils_data/SSURGOdata')
STATSGOdir <- file.path(mainDir, 'soils_data/STATSGO2data')
soilcombined <- file.path(mainDir, 'soils_data/combined')
CA_BCMdir <- file.path(mainDir, 'CA_BCM_data')
soilrasterDir <- file.path(mainDir, 'soils_data/soil_rasters')
TopoWxDir <- file.path(mainDir, 'TopoWx')
NLCD_2011 <- 'NLCD/nlcd_2011_landcover_2011_edition_2014_10_10'
NLCDdir <- file.path(mainDir, NLCD_2011)
NLCD_filename <- 'nlcd_2011_landcover_2011_edition_2014_10_10.img'
results <- file.path(mainDir, 'watershed_characteristics')
climate_classDir <- file.path(mainDir, 'watershed_characteristics/watershed_synthesis/CA_BCM_analysis')
soils_summarized <- file.path(mainDir, 'soils_data/SSURGO_summary')
AWS_mapdata <- file.path(mainDir, 'watershed_characteristics/watershed_synthesis/soil_data/AWC')
final_resultsOct2016 <- file.path(results, 'watershed_synthesis/final_summary')
final_results <- file.path(results, 'watershed_synthesis/final_summary.v2')
QE_proposal <- 'C:\\Users\\smdevine\\Desktop\\Qualifying Exam\\figures for proposal'

#get fnames
setwd(soilrasterDir)
# list.files(pattern = glob2rx('*.tif'))
setwd(boundariesDIR)
watersheds <- list.files(pattern = glob2rx('*.shp'))
centroids_df <- as.data.frame(matrix(nrow=length(watersheds), ncol = 3))
colnames(centroids_df) <- c('fname', 'lon', 'lat')
for (i in 1:length(watersheds)) {
  centroids_df$fname[i] <- watersheds[i]
  centroid <- coordinates(gCentroid(shapefile(watersheds[i])))
  centroids_df[i,2:3] <- centroid
}
setwd('C:/Users/smdevine/Desktop/Forest dieoff and hydrology/final_summary/QE_analysis')
write.csv(centroids_df, 'watershed_centroids.csv', row.names = FALSE)

watershed_fname <- 'uf13_CA_TA_dissolved.shp'
# DEM_fnames <- 'n38_w121_1arc_v3.bil'
soil_raster_fname <- 'uf13_mu.tif'
# results_dir <- 'uf15_stats'

#for same reason, the Datum (NAD83) was not saved as part of crs, but I believe this can be redefined without problems when re-reading in these rasters for further summarizing
watershed_stats <- function(watershed_fname, DEM_fnames, soil_raster_fname, results_dir) {
#start function here
#step 1. Read in FRAPveg raster
  setwd(FRAPdir)
  FRAPveg <- raster('frap_veg.bil') #import veg raster from FRAP dataset
  #legend <- read.csv('FRAP_table.csv', stringsAsFactors = FALSE) #import legend for this raster
  #legend <- legend[,-2] #remove count column to eliminate confusion when it's merged with watershed stats.  count column refers to entire CA dataset.

#step 2. Read in watershed boundary file (California Teale Albers projection)
  setwd(boundariesDIR)
  watershed <- shapefile(watershed_fname) #function arg
  FRAPveg <- crop(FRAPveg, watershed) #save memory
  #FRAPveg <- mask(FRAPveg, watershed) #get rid of values outside of watershed boundary, SLOW, perform at end
  #names(FRAP_raster) <- 'Value' #following three lines only necessary to produce FRAP rasters from the legend data in future work
  #FRAP_whrName <- subs(FRAP_raster, legend, by='Value', which='WHRNUM')
  #FRAP_whr13 <- subs(FRAP_raster, legend, by='Value', which='WHR13NUM')
  FRAP_crs <- crs(FRAPveg)
  raster_object <- projectExtent(FRAPveg, FRAP_crs)
  origin(raster_object) <- origin(FRAPveg)

#step 3 - read in DEM files, merge, and project to California Teale Albers
  # setwd(DEMdir)
  # if (length(DEM_fnames) > 1) {
  #   file_list <- DEM_fnames  #function arg
  #   raster_list <- lapply(X=file_list, FUN=raster)
  #   dem_merged <- do.call(merge, raster_list)
  # } else {dem_merged <- raster(DEM_fnames)}
  # wgs <- crs(dem_merged)
  # watershed_wgs <- spTransform(watershed, wgs) #this so we can crop the DEMs before projecting
  # dem_merged_clip <- crop(dem_merged, watershed_wgs)
  # watershed_DEM_CA_TA <- projectRaster(dem_merged_clip, raster_object) #project DEM raster to CA Teale Albers using empty raster defined by FRAPveg data
  # watershed_slope_CA_TA <- terrain(watershed_DEM_CA_TA, opt = 'slope', unit = 'degrees')#calc slope raster
  # watershed_aspect_CA_TA <- terrain(watershed_DEM_CA_TA, opt = 'aspect', unit = 'degrees' )
  #watershed_slope_CA_TA <- mask(watershed_slope_CA_TA, watershed) #mask the slope raster to the watershed boundary, perform at end
  #watershed_aspect_CA_TA <- mask(watershed_aspect_CA_TA, watershed) #mask the aspect raster to the watershed boundary, perform at end
  #watershed_DEM_CA_TA <- mask(watershed_DEM_CA_TA, watershed) #necessary to convert values to NA outside of the watershed boundary, perform at end

#step 4b read in soils data (all of this good to go if needed)
  setwd(soilrasterDir)
  soil_raster <- raster(soil_raster_fname) #function arg
  soil_raster <- projectRaster(soil_raster, raster_object, method = 'ngb') #method 'ngb' is the nearest neighbor method and is appropriate for categorical data
  
#step 5b read in CA_BCM_data (all of this good to go if needed)
  # setwd(CA_BCMdir)
  # CA_BCM_data <- brick('CA_BCM_30yr_summaries.tif')
  # CA_BCM_data <- crop(CA_BCM_data, raster_object)
  # CA_BCM_data <- resample(CA_BCM_data, raster_object)
  # CA_BCM_data <- mask(CA_BCM_data, watershed) #perform at end

#step 6 read in TopoWx normals and calculate Tavg
  # setwd(TopoWxDir)
  # topowx_tmin <- brick('normals_tmin.nc') #avg (1981-2010) tmin by month 
  # topowx_crs <- crs(topowx_tmin)
  # watershed_longlat <- spTransform(watershed, topowx_crs)
  # topowx_tmin <- crop(topowx_tmin, watershed_longlat)
  # topowx_tmin <- mean(topowx_tmin)
  # topowx_tmax <- brick('normals_tmax.nc')
  # topowx_tmax <- crop(topowx_tmax, watershed_longlat)
  # topowx_tmax <- mean(topowx_tmax)
  # topowx_tavg <- (topowx_tmax + topowx_tmin)/2
  # topowx_norms <- brick(topowx_tmin, topowx_tmax, topowx_tavg)
  # names(topowx_norms) <- c('tmin1980_2010topowx.tif', 'tmax1980_2010topowx.tif', 'tavg1980_2010topowx.tif')
  # topowx_norms <- projectRaster(topowx_norms, raster_object) #project topowx raster data using the template created from FRAP raster
  #topowx_norms <- mask(topowx_norms, watershed), perform at end
  #setwd(results)
  #writeRaster(topowx_norms, filename = names(topowx_norms), bylayer=TRUE, format='GTiff')

#step 7: read in NLCD data
  # setwd(NLCDdir)
  # NLCD_raster <- raster(NLCD_filename)
  # albers <- crs(NLCD_raster) #define coordinate system based on raster
  # watershed_albers <- spTransform(watershed, albers) #project watershed shapefile
  # NLCD_raster <- crop(NLCD_raster, watershed_albers) #crop the raster based on watershed extent
  # NLCD_raster <- projectRaster(NLCD_raster, raster_object, method = 'ngb')
  # NLCD_raster <- mask(NLCD_raster, watershed)
  
#step 8. Read in climate class cluster data.
  setwd(climate_classDir)
  climate_classes <- raster('climate_class.tif')
  climate_classes <- projectRaster(climate_classes, raster_object, method = 'ngb')
  climate_classes <- mask(climate_classes, watershed)
#final step
  all_watershed_data <- climate_classes # this was the original save argument before adding TopoWx and NLCD: brick(FRAPveg, watershed_DEM_CA_TA, watershed_slope_CA_TA, watershed_aspect_CA_TA, soil_raster, CA_BCM_data)
  #all_watershed_data <- mask(all_watershed_data, watershed)
  names(all_watershed_data) <- c('climate_classes.tif') #this was the full original argument: c('FRAPveg.tif', 'DEM.tif', 'soil_mukey.tif', 'aprpck1921_1950avg.tif', 'aprpck1951_1980avg.tif', 'aprpck1981_2010avg.tif', 'pet1921_1950avg.tif', 'pet1951_1980avg.tif', 'pet1981_2010avg.tif', 'ppt1921_1950avg.tif', 'ppt1951_1980avg.tif', 'ppt1981_2010avg.tif', 'tmn1921_1950avg.tif', 'tmn1951_1980avg.tif', 'tmn1981_2010avg.tif', 'tmx1921_1950avg.tif', 'tmx1951_1980avg.tif', 'tmx1981_2010avg.tif')
  #all_watershed_data_df <- as.data.frame(all_watershed_data, xy=TRUE, na.rm=TRUE)
  if (file.exists(file.path(results, results_dir)) == FALSE) {
    dir.create(file.path(results, results_dir))
  }
  setwd(file.path(results, results_dir))
  writeRaster(all_watershed_data, filename = names(all_watershed_data), bylayer=TRUE, format='GTiff', overwrite=TRUE) #function arg
  #write.csv(all_watershed_data_df, paste(results_dir, '.csv', sep = ''), row.names=FALSE)
}
#these are the args: (1) watershed_fname (2) DEM_fnames (3) soil_raster_fname (4) results_fname
#uf16 and below need to be re-run to correct CA BCM data
watershed_stats('uf13_CA_TA_dissolved.shp', c('n38_w122_1arc_v3.bil', 'n38_w121_1arc_v3.bil'), 'uf13_mu.tif', 'uf13_stats')
watershed_stats('uf14_CA_TA_dissolved.shp', c('n38_w121_1arc_v3.bil', 'n38_w120_1arc_v3.bil'), 'uf14_mu.tif', 'uf14_stats')
watershed_stats('uf16_CA_TA_dissolved.shp', c('n37_w121_1arc_v3.bil', 'n38_w120_1arc_v3.bil', 'n38_w121_1arc_v3.bil'), 'uf16_mu.tif', 'uf16_stats')
watershed_stats('uf18_CA_TA_dissolved.shp', c('n37_w121_1arc_v3.bil', 'n37_w120_1arc_v3.bil', 'n38_w120_1arc_v3.bil', 'n38_w121_1arc_v3.bil'), 'uf18_mu.tif', 'uf18_stats')
watershed_stats('uf19_CA_TA_dissolved.shp', c('n37_w121_1arc_v3.bil', 'n37_w120_1arc_v3.bil'), 'uf19_mu.tif', 'uf19_stats')
watershed_stats('uf22_CA_TA_dissolved.shp', c('n37_w120_1arc_v3.bil', 'n37_w119_1arc_v3.bil', 'n36_w120_1arc_v3.bil'), 'uf22_mu.tif', 'uf22_stats')
watershed_stats('uf8_CA_TA_dissolved.shp', c('n39_w122_1arc_v3.bil', 'n39_w121_1arc_v3.bil','n40_w122_1arc_v3.bil', 'n40_w121_1arc_v3.bil'), 'uf8_mu.tif', 'uf8_stats')
watershed_stats('uf9_CA_TA_dissolved.shp', c('n39_w122_1arc_v3.bil', 'n39_w121_1arc_v3.bil'), 'uf9_mu.tif', 'uf9_stats')
watershed_stats('uf11_CA_TA_dissolved.shp', c('n38_w122_1arc_v3.bil', 'n38_w121_1arc_v3.bil', 'n38_w120_1arc_v3.bil', 'n39_w121_1arc_v3.bil'), 'uf11_mu.tif', 'uf11_stats')

watershed_stats('uf_sacramento_headwaters_CA_TA_dissolved.shp', c('n41_w123_1arc_v3.bil', 'n40_w123_1arc_v3.bil'), 'uf_sachead_mu.tif', 'uf_sachead_stats')
watershed_stats('uf_mccloud_CA_TA_dissolved.shp', c('n41_w123_1arc_v3.bil', 'n40_w123_1arc_v3.bil', 'n41_w122_1arc_v3.bil'), 'uf_mccloud_mu.tif', 'uf_mccloud_stats')
watershed_stats('uf_Pit_CA_TA_dissolved.shp', c('n41_w123_1arc_v3.bil', 'n40_w123_1arc_v3.bil', 'n41_w122_1arc_v3.bil', 'n40_w122_1arc_v3.bil', 'n40_w121_1arc_v3.bil', 'n41_w121_1arc_v3.bil' ), 'uf_pit_mu.tif', 'uf_pit_stats')
watershed_stats('uf10_CA_TA_dissolved.shp', c('n39_w122_1arc_v3.bil', 'n39_w121_1arc_v3.bil', 'n38_w122_1arc_v3.bil'), 'uf10_mu.tif', 'uf10_stats')
watershed_stats('uf15_CA_TA_dissolved.shp', 'n38_w121_1arc_v3.bil', 'uf15_mu.tif', 'uf15_stats') #have to manually change script when there is just one DEM file
watershed_stats('uf20_CA_TA_dissolved.shp', c('n37_w121_1arc_v3.bil', 'n37_w120_1arc_v3.bil'), 'uf20_mu.tif', 'uf20_stats')
watershed_stats('uf21_CA_TA_dissolved.shp', 'n37_w120_1arc_v3.bil', 'uf21_mu.tif', 'uf21_stats') #have to manually change script when there is just one DEM file
watershed_stats('uf_kings_CA_TA_dissolved.shp', c('n36_w120_1arc_v3.bil', 'n37_w120_1arc_v3.bil', 'n37_w119_1arc_v3.bil', 'n36_w119_1arc_v3.bil'), 'uf_kings_mu.tif', 'uf_kings_stats')
watershed_stats('uf_kaweah_CA_TA_dissolved.shp', c('n36_w120_1arc_v3.bil', 'n36_w119_1arc_v3.bil'), 'uf_kaweah_mu.tif', 'uf_kaweah_stats')
watershed_stats('uf_tule_CA_TA_dissolved.shp', c('n36_w119_1arc_v3.bil', 'n35_w119_1arc_v3.bil'), 'uf_tule_mu.tif', 'uf_tule_stats')
watershed_stats('uf_kern_CA_TA_dissolved.shp', c('n36_w119_1arc_v3.bil', 'n35_w119_1arc_v3.bil', 'n35_w118_1arc_v3.bil'), 'uf_kern_mu.tif', 'uf_kern_stats')

##merging SSURGO data with STATSGO data were necessary and then substituting soil raster mukeys with csv summaries produced in separate soil aggregation scripts: "SSURGO_analysis.v2.R" and "STATSGO_analysis.v2.R"
#need to re-run Pit to account for missing data (i=6)
setwd(results)
rasters_dirs <- list.dirs(results)
for (i in 2:21) {
  rasters_dir <- rasters_dirs[i] #change index to i
  rasters_fnames <- list.files(rasters_dir, pattern = glob2rx('*.tif'))
  setwd(rasters_dir)
  soils_raster <- raster('soil_mukey.tif')
  soils_dirs <- list.dirs(soils_summarized)
  soils_dir <- soils_dirs[i+1] #change index to i+1
  print(soils_dir)
  soils_fnames <- list.files(soils_dir, pattern = glob2rx('*.csv'))
  setwd(soils_dir)
  watershed_name <- basename(soils_dir)
  ssurgo_mu_summary <- read.csv(soils_fnames[8], stringsAsFactors = FALSE) #keep index at 8 to read-in each 'mu summary' file per watershed directory
  print(nrow(ssurgo_mu_summary))
  if (basename(soils_dir)=='uf_pit') {
    ssurgo_mu_summary$data_source <- 'SSURGO'
    setwd(soils_dirs[2]) #keep index at 2 to go back to statsgo2 folder
    statsgo_mu_summary <- read.csv('statsgo2_mu_summary.csv', stringsAsFactors = FALSE)
    statsgo_mu_summary$data_source <- 'STATSGO2'
    combined_mu_summary <- rbind(ssurgo_mu_summary, statsgo_mu_summary)
  }
  if ('NOTCOM(100%)' %in% ssurgo_mu_summary$component_summary==TRUE) {
    ssurgo_mu_summary2 <- ssurgo_mu_summary[-which(ssurgo_mu_summary$component_summary=='NOTCOM(100%)'), ] #get rid of the NOTCOM's
    print(paste('There are', (nrow(ssurgo_mu_summary) - nrow(ssurgo_mu_summary2)), 'mukeys missing SSURGO data that will be filled by STATSGO.'))
    ssurgo_mu_summary2$data_source <- 'SSURGO'
    setwd(soils_dirs[2]) #keep index at 2 to go back to statsgo2 folder
    statsgo_mu_summary <- read.csv('statsgo2_mu_summary.csv', stringsAsFactors = FALSE)
    statsgo_mu_summary$data_source <- 'STATSGO2'
    combined_mu_summary <- rbind(ssurgo_mu_summary2, statsgo_mu_summary)
  }
  if ('NOTCOM(100%)' %in% ssurgo_mu_summary$component_summary == FALSE & basename(soils_dir)!='uf_pit') {
    ssurgo_mu_summary$data_source <- 'SSURGO'
    combined_mu_summary <- ssurgo_mu_summary
    print('No gaps in SSURGO data.')
  }
  raster_mukeys <- unique(soils_raster) #get unique raster mukeys
  combined_mu_summary <- combined_mu_summary[which(combined_mu_summary$mukey %in% raster_mukeys), ] #select only the mukeys that are in the raster
  combined_mu_summary$ABC_pct <- combined_mu_summary$Ahorizon_pct + combined_mu_summary$Bhorizon_pct + combined_mu_summary$Chorizon_pct
  #setwd(soils_dir)
  #write.csv(combined_mu_summary, paste(watershed_name, '_mu_summary_SSURGO_STATSGO.csv', sep = ''), row.names = FALSE)
  aws_df <- combined_mu_summary[ ,c('mukey', 'awc_H2Ocm_wtdavg', 'aws0150wta')]
  #aws_thickness_df <- combined_mu_summary[ ,c('mukey', 'awc_soilthickness_wtdavg', 'Bhorizon_pct', 'Chorizon_pct')]
  setwd(rasters_dir)
  #subs(soils_raster, aws_thickness_df, by='mukey', which='awc_soilthickness_wtdavg', filename='aws_soil_thickness_cm.tif', format='GTiff', overwrite=TRUE)
  #subs(soils_raster, aws_thickness_df, by='mukey', which='Bhorizon_pct', filename='Bhorizon_pct.tif', format='GTiff', overwrite=TRUE)
  #subs(soils_raster, aws_thickness_df, by='mukey', which='Chorizon_pct', filename='Chorizon_pct.tif', format='GTiff', overwrite=TRUE)
  # subs(soils_raster, aws_df, by='mukey', which='awc_H2Ocm_wtdavg', filename='aws_soils.tif', format='GTiff', overwrite=TRUE)
  subs(soils_raster, aws_df, by='mukey', which='aws0150wta', filename='aws0150wta.tif', format='GTiff', overwrite=TRUE)
  uncertainty_df <- combined_mu_summary[ ,c('mukey', 'ABC_pct', 'NAhorizon_pct', 'lithic_pct', 'paralithic_pct', 'rockoutcrop_pct', 'data_source', 'awc_ppct_tot')]
  subs(soils_raster, uncertainty_df, by='mukey', which='awc_ppct_tot', filename='awc_comp_pct.tif', format='GTiff', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='lithic_pct', filename='lithic_soils_pct.tif', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='rockoutcrop_pct', filename='rockoutcrop_soils_pct.tif', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='ABC_pct', filename='ABC_soils_pct.tif', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='paralithic_pct', filename='paralithic_soils_pct.tif', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='NAhorizon_pct', filename='NAhorizon_soils_pct.tif', overwrite=TRUE)
  #subs(soils_raster, uncertainty_df, by='mukey', which='data_source', filename='soils_data_source.tif', overwrite=TRUE)
}

#save combined mu_summary as csv file to soils directory by modifying script above so that subs commands aren't called

#export aws files to soil map data folder
setwd(results)
rasters_dirs <- list.dirs(results)
i=21
for (i in 2:21) {
  raster_dir <- rasters_dirs[i]
  setwd(raster_dir)
  watershed_name <- basename(raster_dir)
  watershed_name <- substr(watershed_name, 1, (nchar(watershed_name)-6))
  aws_raster <- raster('aws_soils.tif')
  #aws_raster <- aggregate(aws_raster, fact=3, fun=mean, na.rm=TRUE)
  setwd(AWS_mapdata)
  writeRaster(aws_raster, paste(watershed_name, '_aws.tif', sep = ''), format='GTiff', overwrite=TRUE)
}

#now read in soil rasters and calculate stats by climate zone
#initialize a data.frame to place the summarized raster data
watershed_characteristics <- as.data.frame(matrix(nrow=20, ncol = 41)) #should be nrow=80 if doing the analysis by climate zone
names(watershed_characteristics) <- c('watershed name', 'climate class', 'area (ha)', 'area (% of watershed)', 'aws cm (mean)', 'aws cm (sd)', 'aws0150cm (mean)', 'aws soil thickness cm (mean)', 'aws soil thickness cm (sd)', 'ABC %', 'B %', 'C %', 'Cr %', 'R %', 'NA deep horizon %', 'rock outcrop %', 'PPT mm 1981-2010 (mean)', 'PPT mm 1981-2010 (sd)', 'Aprpck mm 1921-1950 (mean)', 'Aprpck mm 1951-1980 (mean)', 'Aprpck mm 1981-2010 (mean)', 'MinAAT C 1981-2010 (mean)', 'MinAAT C 1981-2010 (sd)', 'MaxAAT C 1981-2010 (mean)', 'MaxAAT C 1981-2010 (sd)', 'MAAT C 1981-2010 (mean)', 'MAAT C 1981-2010 (sd)', 'PET mm 1921-1950 (mean)', 'PET mm 1951-1980 (mean)', 'PET mm 1981-2010 (mean)', 'PET mm 1980-2010 (sd)', 'elev m (mean)', 'elev m (sd)', 'slope (mean)', 'slope (sd)', 'aspect deg (mean)', 'aspect deg (sd)', 'SSURGO %', 'STATSGO %', 'AWC mapunit NA %', 'AWC component NA %')
setwd(boundariesDIR)
watershed_fnames <- list.files(pattern = glob2rx('*.shp'))
#define a function to summarize data by each climate class using raster brick and cellStats (works better than converting raster Brick to data.frame or matrix and then summarizing)
summary_by_cc <- function(cc_class) { #cc_class is 1, 2, 3, and 4, respectively, except overall summary will be 5
  if (cc_class %in% 1:4) {
    row_num <- j*4-(4-cc_class)
    cc <- soils_stack$climate_class
    cc[cc$climate_class!=cc_class] <- NA
    df <- mask(soils_stack, cc) #much faster to mask on a raster brick than mask on rasters separately
    watershed_characteristics$`climate class`[row_num] <- cc_class
    watershed_cells <- cellStats(df$climate_class, 'sum')/cc_class
    watershed_characteristics$`area (ha)`[row_num] <- (watershed_cells)*900/10000 #because each row represents a 30 m x 30 m raster cell
    watershed_characteristics$`area (% of watershed)`[row_num] <- 100*watershed_characteristics$`area (ha)`[row_num]/watershed_size
  }
  if (cc_class==5) { #modified this so that this will be run as separate script
    row_num <- j
    #row_num <- j*5-(5-cc_class)
    watershed_characteristics$`climate class`[row_num] <- 'overall'
    df <- soils_stack
    df$climate_class[!is.na(df$climate_class)] <- 5
    print(cellStats(df$climate_class, 'sum')/5) #temp check
    watershed_cells <- watershed_size*10000/900 #raster is 30 x 30 m; watershed size in ha
    print(watershed_cells) #temp check
    watershed_characteristics$`area (ha)`[row_num] <- watershed_size
    watershed_characteristics$`area (% of watershed)`[row_num] <- 100
  }
  if (watershed_cells==0) { #will only apply to climate class subsets
    return(watershed_characteristics)
  }
  watershed_characteristics$`aws cm (mean)`[row_num] <- cellStats(df$aws_H2Ocm, 'mean')
  watershed_characteristics$`aws cm (sd)`[row_num] <- cellStats(df$aws_H2Ocm, 'sd')
  watershed_characteristics$`aws0150cm (mean)`[row_num] <- cellStats(df$aws0150wta, 'mean')
  watershed_characteristics$`aws soil thickness cm (mean)`[row_num] <- cellStats(df$aws_soil_thickness_cm, 'mean')
  watershed_characteristics$`aws soil thickness cm (sd)`[row_num] <- cellStats(df$aws_soil_thickness_cm, 'sd')
  watershed_characteristics$`ABC %`[row_num] <- 100*cellStats(df$ABC_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`B %`[row_num] <- 100*cellStats(df$Bhorizon_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`C %`[row_num] <- 100*cellStats(df$Chorizon_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`Cr %`[row_num] <- 100*cellStats(df$Cr_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`R %`[row_num] <- 100*cellStats(df$R_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`NA deep horizon %`[row_num] <- 100*cellStats(df$NA_hor_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`rock outcrop %`[row_num] <- 100*cellStats(df$rock_pct/100, 'sum')/watershed_cells
  watershed_characteristics$`PPT mm 1981-2010 (mean)`[row_num] <- cellStats(df$ppt1981_2010avg, 'mean')
  watershed_characteristics$`PPT mm 1981-2010 (sd)`[row_num] <- cellStats(df$ppt1981_2010avg, 'sd')
  watershed_characteristics$`Aprpck mm 1921-1950 (mean)`[row_num] <- cellStats(df$apr.snpk.mm.1921_1950, 'mean')
  watershed_characteristics$`Aprpck mm 1951-1980 (mean)`[row_num] <- cellStats(df$apr.snpk.mm.1951_1980, 'mean')
  watershed_characteristics$`Aprpck mm 1981-2010 (mean)`[row_num] <- cellStats(df$apr.snpk.mm.1981_2010, 'mean')
  watershed_characteristics$`MinAAT C 1981-2010 (mean)`[row_num] <- cellStats(df$tmn1981_2010avg, 'mean')
  watershed_characteristics$`MinAAT C 1981-2010 (sd)`[row_num] <- cellStats(df$tmn1981_2010avg, 'sd')
  watershed_characteristics$`MaxAAT C 1981-2010 (mean)`[row_num] <- cellStats(df$tmx1981_2010avg, 'mean')
  print('50% done!')
  watershed_characteristics$`MaxAAT C 1981-2010 (sd)`[row_num] <- cellStats(df$tmx1981_2010avg, 'sd')
  df$tavg1981_2010 <- (df$tmx1981_2010avg + df$tmn1981_2010avg)/2
  watershed_characteristics$`MAAT C 1981-2010 (mean)`[row_num] <- cellStats(df$tavg1981_2010, 'mean')
  watershed_characteristics$`MAAT C 1981-2010 (sd)`[row_num] <- cellStats(df$tavg1981_2010, 'sd')
  watershed_characteristics$`PET mm 1981-2010 (mean)`[row_num] <- cellStats(df$pet1981_2010avg, 'mean')
  watershed_characteristics$`PET mm 1980-2010 (sd)`[row_num] <- cellStats(df$pet1981_2010avg, 'sd')
  watershed_characteristics$`PET mm 1921-1950 (mean)`[row_num] <- cellStats(df$pet1921_1950avg, 'mean')
  watershed_characteristics$`PET mm 1951-1980 (mean)`[row_num] <- cellStats(df$pet1951_1980avg, 'mean')
  df$ppt_minus_pet <- df$ppt1981_2010avg - df$pet1981_2010avg
  # watershed_characteristics$`PPT - PET mm (mean)`[row_num] <- cellStats(df$ppt_minus_pet, 'mean')
  # watershed_characteristics$`PPT - PET mm (sd)`[row_num] <- cellStats(df$ppt_minus_pet, 'sd')
  watershed_characteristics$`elev m (mean)`[row_num] <- cellStats(df$elevation, 'mean')
  watershed_characteristics$`elev m (sd)`[row_num] <- cellStats(df$elevation, 'sd')
  watershed_characteristics$`slope (mean)`[row_num] <- cellStats(df$slope, 'mean')
  watershed_characteristics$`slope (sd)`[row_num] <- cellStats(df$slope, 'sd')
  watershed_characteristics$`aspect deg (mean)`[row_num] <- cellStats(df$aspect, 'mean')
  watershed_characteristics$`aspect deg (sd)`[row_num] <- cellStats(df$aspect, 'sd')
  watershed_characteristics$`SSURGO %`[row_num] <- 100*cellStats(df$soils_data_source==1, 'sum')/watershed_cells
  watershed_characteristics$`STATSGO %`[row_num] <- 100*cellStats(df$soils_data_source==2, 'sum')/watershed_cells
  watershed_characteristics$`AWC mapunit NA %`[row_num] <- 100*cellStats(is.na(df$aws_H2Ocm)*df$climate_class/cc_class, 'sum')/watershed_cells
  watershed_characteristics$`AWC component NA %`[row_num] <- 100 - 100*cellStats(df$awc_comp_pct/100, 'sum')/watershed_cells
  invisible(watershed_characteristics)
  #removeTmpFiles(h=0.0000001)
}
#run the function in a loop for each watershed x climate class  
#i <- 10, 2 to 4 first
for (i in 2:21) {
  j <- i-1
  setwd(results)
  rasters_dirs <- list.dirs(results)
  raster_dir <- rasters_dirs[i] #change to i
  print(raster_dir)
  watershed_name <- basename(raster_dir)
  watershed_name <- substr(watershed_name, 1, (nchar(watershed_name)-6))
  setwd(raster_dir)
  soils_stack <- stack('aws_soils.tif', 'aws_soil_thickness_cm.tif', 'ABC_soils_pct.tif', 'Bhorizon_pct.tif', 'Chorizon_pct.tif', 'paralithic_soils_pct.tif', 'lithic_soils_pct.tif', 'NAhorizon_soils_pct.tif', 'rockoutcrop_soils_pct.tif', 'climate_classes.tif', 'ppt1981_2010avg.tif', 'tmn1981_2010avg.tif', 'tmx1981_2010avg.tif', 'slope.tif', 'DEM.tif', 'aspect.tif', 'soils_data_source.tif', 'pet1981_2010avg.tif', 'aws0150wta.tif', 'awc_comp_pct.tif', 'aprpck1921_1950avg.tif', 'aprpck1951_1980avg.tif', 'aprpck1981_2010avg.tif', 'pet1921_1950avg.tif', 'pet1951_1980avg.tif')
  soils_stack <- brick(soils_stack)
  names(soils_stack) <- c('aws_H2Ocm', 'aws_soil_thickness_cm', 'ABC_pct', 'Bhorizon_pct', 'Chorizon_pct', 'Cr_pct', 'R_pct', 'NA_hor_pct', 'rock_pct', 'climate_class', 'ppt1981_2010avg', 'tmn1981_2010avg', 'tmx1981_2010avg', 'slope', 'elevation', 'aspect', 'soils_data_source', 'pet1981_2010avg', 'aws0150wta', 'awc_comp_pct', 'apr.snpk.mm.1921_1950', 'apr.snpk.mm.1951_1980', 'apr.snpk.mm.1981_2010', 'pet1921_1950avg', 'pet1951_1980avg') #will need to calculate water coverage area, which should account for a portion of NA_hor_pct
  setwd(boundariesDIR)
  print(watershed_fnames[j])
  watershed_size <- gArea(shapefile(watershed_fnames[j]))/10000
  watershed_characteristics$`watershed name`[j] <- watershed_name 
  #watershed_characteristics <- summary_by_cc(1)
  #print('climate class 1 done!')
  #watershed_characteristics <- summary_by_cc(2)
  #print('climate class 2 done!')
  #watershed_characteristics <- summary_by_cc(3)
  #print('climate class 3 done!')
  #watershed_characteristics <- summary_by_cc(4)
  watershed_characteristics <- summary_by_cc(5)
  #watershed_characteristics$`climate class`[watershed_characteristics$`climate class`== 1] <- '8 C MAAT, 500 cm precip'
  #watershed_characteristics$`climate class`[watershed_characteristics$`climate class`== 2] <- '5 C MAAT, 1100 cm precip'
  #watershed_characteristics$`climate class`[watershed_characteristics$`climate class`== 3] <- '11 C MAAT, 1400 cm precip'
  #watershed_characteristics$`climate class`[watershed_characteristics$`climate class`== 4] <- '15 C MAAT, 800 cm precip'
  setwd(final_results)
  #write.csv(watershed_characteristics, paste('watersheds_characteristics_summary_', Sys.Date(), '.csv', sep = ""), row.names = FALSE) this is line for saving results by climate class
  write.csv(watershed_characteristics, paste('watershed_mean_characteristics_summary_', Sys.Date(), '.csv', sep = ""), row.names = FALSE) #line for saving results by mean watershed
  print(watershed_characteristics[j, ])
  removeTmpFiles(h=0.00001)
}


#read in summary file to summarize by climate groups
setwd(final_resultsOct2016)
watershed_summary <- read.csv('watersheds_characteristics_summary_2016-10-13.csv', stringsAsFactors = FALSE, na.strings = 'NA')
colnames(watershed_summary) <- c('watershed name', 'climate class', 'area ha', '% of watershed', 'aws cm', 'aws cm (sd)', 'aws0150cm (mean)', 'aws soil thickness cm (mean)', 'aws soil thickness cm (sd)', 'ABC %', 'B %', 'C %', 'Cr %', 'R %', 'NA deep horizon %', 'rock outcrop %', 'PPT mm', 'PPT mm (sd)', 'MinAAT C', 'MinAAT C (sd)', 'MaxAAT C', 'MaxAAT C (sd)', 'MAAT C', 'MAAT C (sd)', 'PET mm (mean)', 'PET mm (sd)', 'PPT - PET mm', 'PPT - PET mm (sd)', 'elev m', 'elev m (sd)', 'slope (mean)', 'slope (sd)', 'aspect deg (mean)', 'aspect deg (sd)', 'SSURGO %', 'STATSGO %', 'AWC mapunit NA %', 'AWC component NA %')

#calculate wtd means across watersheds by climate class
watershed_summary[, 'B-C %'] <- watershed_summary$`B %` + watershed_summary$`C %`
#watershed_summary$`area ha` <- as.numeric(watershed_summary$`area ha`)
watershed_summary$aws_mm <- watershed_summary$`aws cm`*10
#watershed_summary$aws_mm_area <- watershed_summary$aws_mm*watershed_summary$`area ha`
area_5C_total <- sum(watershed_summary$`area ha`[which(watershed_summary$`climate class`=='5 C MAAT, 1100 cm precip')])
area_8C_total <- sum(watershed_summary$`area ha`[which(watershed_summary$`climate class`=='8 C MAAT, 500 cm precip')])
area_11C_total <- sum(watershed_summary$`area ha`[which(watershed_summary$`climate class`=='11 C MAAT, 1400 cm precip')])
area_15C_total <- sum(watershed_summary$`area ha`[which(watershed_summary$`climate class`=='15 C MAAT, 800 cm precip')])
watershed_summary$cc_area <- 0
watershed_summary$cc_area[which(watershed_summary$`climate class`=='5 C MAAT, 1100 cm precip')] <- area_5C_total
watershed_summary$cc_area[which(watershed_summary$`climate class`=='8 C MAAT, 500 cm precip')] <- area_8C_total
watershed_summary$cc_area[which(watershed_summary$`climate class`=='11 C MAAT, 1400 cm precip')] <- area_11C_total
watershed_summary$cc_area[which(watershed_summary$`climate class`=='15 C MAAT, 800 cm precip')] <- area_15C_total
watershed_summary$wtd_function <- watershed_summary$`area ha`/watershed_summary$cc_area
aws_by_cc <- tapply(watershed_summary$aws_mm*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
rock <- tapply(watershed_summary$`rock outcrop %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
R_prelim <- tapply(watershed_summary$`R %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
Cr <- tapply(watershed_summary$`Cr %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
R_soil <- R_prelim - rock
B <- tapply(watershed_summary$`B %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
C <- tapply(watershed_summary$`C %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)
no_data <- tapply(watershed_summary$`NA deep horizon %`*watershed_summary$wtd_function, watershed_summary$`climate class`, sum, na.rm=TRUE)

#exploratory plots outside the 'summary_by_cc' function with soil stack for QE proposal
soils_stack$ppt_minus_pet_1981_2010 <- soils_stack$ppt1981_2010avg - soils_stack$pet1981_2010avg
soils_stack$aws_H2Omm <- soils_stack$aws_H2Ocm*10
soils_stack$delta_aprpck <- soils_stack$apr.snpk.mm.1981_2010 - soils_stack$apr.snpk.mm.1921_1950
setwd(QE_proposal)
png(paste('American_AWS_vs_climdef.png', sep = ''), family = 'Book Antiqua', width = 7, height = 7, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
plot(soils_stack$aws_H2Omm, soils_stack$ppt_minus_pet_1981_2010, maxpixels=2000000, xlab='AWS mm H2O', ylab='P - PET mm', cex.lab=1.3, cex.axis=1.3)
dev.off()
png(paste('American_AWS_vs_aprpck.png', sep = ''), family = 'Book Antiqua', width = 7, height = 7, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
plot(soils_stack$aws_H2Omm, soils_stack$apr.snpk.mm.1981_2010, maxpixels=2000000, xlab='AWS mm H2O', ylab='mean Apr Snowpack 1981-2010 (mm)', cex.lab=1.3, cex.axis=1.3)
dev.off()
png(paste('American_delta_aprpck.png', sep = ''), family = 'Book Antiqua', width = 7, height = 7, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
plot(soils_stack$delta_aprpck)
dev.off()

#extra test code not really needed now but it's a keepsake nonetheless
#function written with converstion of raster brick to matrix (did not work with large rasters)
summary_by_cc <- function(cc_class) { #cc_class is 1, 2, 3, and 4, respectively, except overall summary will be 5
  if (cc_class %in% 1:4) {
    row_num <- j*4-(4-cc_class)
    df <- soils_stack[soils_stack$climate_class==cc_class]
  }
  if (cc_class==5) { #skipping overall watershed for now because it takes too long
    row_num <- j*5-(5-cc_class)
    cc_class <- 'overall'
    watershed_characteristics$`climate class`[row_num] <- cc_class
    df <- as.data.frame(soils_stack[!is.na(soils_stack$climate_class)])
    watershed_characteristics$`area (ha)`[row_num] <- watershed_size
    watershed_characteristics$`area (% of watershed)`[row_num] <- 100
  }
  if (is.null(df)) { #will only apply to climate class subsets
    watershed_characteristics$`climate class` <- cc_class
    watershed_characteristics$`area (ha)` <- 0
    watershed_characteristics$`area (% of watershed)` <- 0
    return(watershed_characteristics)
  }
  watershed_characteristics$`climate class`[row_num] <- cc_class
  watershed_characteristics$`area (ha)`[row_num] <- nrow(df)*900/10000 #because each row represents a 30 m x 30 m raster cell
  watershed_characteristics$`area (% of watershed)`[row_num] <- 100*watershed_characteristics$`area (ha)`[row_num]/watershed_size
  watershed_characteristics$`aws cm (mean)`[row_num] <- mean(df[ ,'aws_H2Ocm'], na.rm=TRUE)
  watershed_characteristics$`aws cm (sd)`[row_num] <- sd(df[ ,'aws_H2Ocm'], na.rm=TRUE)
  watershed_characteristics$`aws0150cm (mean)`[row_num] <- mean(df[ ,'aws0150wta'], na.rm=TRUE)
  watershed_characteristics$`aws soil thickness cm (mean)`[row_num] <- mean(df[ ,'aws_soil_thickness_cm'], na.rm=TRUE)
  watershed_characteristics$`aws soil thickness cm (sd)`[row_num] <- sd(df[ ,'aws_soil_thickness_cm'], na.rm=TRUE)
  watershed_characteristics$`ABC %`[row_num] <- 100*sum(df[ ,'ABC_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`B %`[row_num] <- 100*sum(df[ ,'Bhorizon_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`C %`[row_num] <- 100*sum(df[ ,'Chorizon_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`Cr %`[row_num] <- 100*sum(df[ ,'Cr_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`R %`[row_num] <- 100*sum(df[ ,'R_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`NA deep horizon %`[row_num] <- 100*sum(df[ ,'NA_hor_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`rock outcrop %`[row_num] <- 100*sum(df[ ,'rock_pct']/100, na.rm = TRUE)/nrow(df)
  watershed_characteristics$`PPT mm (mean)`[row_num] <- mean(df[ ,'ppt1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`PPT mm (sd)`[row_num] <- sd(df[ ,'ppt1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`MinAAT C (mean)`[row_num] <- mean(df[ ,'tmn1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`MinAAT C (sd)`[row_num] <- sd(df[ ,'tmn1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`MaxAAT C (mean)`[row_num] <- mean(df[ ,'tmx1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`MaxAAT C (sd)`[row_num] <- sd(df[ ,'tmx1981_2010avg'], na.rm=TRUE)
  tavg1981_2010 <- (df[ ,'tmx1981_2010avg'] + df[ ,'tmn1981_2010avg'])/2
  df <- cbind(df, tavg1981_2010)
  watershed_characteristics$`MAAT C (mean)`[row_num] <- mean(df[ ,'tavg1981_2010'], na.rm=TRUE)
  watershed_characteristics$`MAAT C (sd)`[row_num] <- sd(df[ ,'tavg1981_2010'], na.rm=TRUE)
  watershed_characteristics$`PET mm (mean)`[row_num] <- mean(df[ ,'pet1981_2010avg'], na.rm=TRUE)
  watershed_characteristics$`PET mm (sd)`[row_num] <- sd(df[ ,'pet1981_2010avg'], na.rm=TRUE)
  ppt_minus_pet <- df[ ,'ppt1981_2010avg'] - df[ ,'pet1981_2010avg']
  df <- cbind(df, ppt_minus_pet)
  watershed_characteristics$`PPT - PET mm (mean)`[row_num] <- mean(df[ ,'ppt_minus_pet'], na.rm=TRUE)
  watershed_characteristics$`PPT - PET mm (sd)`[row_num] <- sd(df[ ,'ppt_minus_pet'], na.rm=TRUE)
  watershed_characteristics$`elev m (mean)`[row_num] <- mean(df[ ,'elevation'], na.rm=TRUE)
  watershed_characteristics$`elev m (sd)`[row_num] <- sd(df[ ,'elevation'], na.rm=TRUE)
  watershed_characteristics$`slope (mean)`[row_num] <- mean(df[ ,'slope'], na.rm=TRUE)
  watershed_characteristics$`slope (sd)`[row_num] <- sd(df[ ,'slope'], na.rm=TRUE)
  watershed_characteristics$`aspect deg (mean)`[row_num] <- mean(df[ ,'aspect'], na.rm=TRUE)
  watershed_characteristics$`aspect deg (sd)`[row_num] <- sd(df[ ,'aspect'], na.rm=TRUE)
  watershed_characteristics$`SSURGO %`[row_num] <- 100*nrow(df[df[ ,'soils_data_source']==1, ])/nrow(df)
  watershed_characteristics$`STATSGO %`[row_num] <- 100*nrow(df[df[ ,'soils_data_source']==2, ])/nrow(df)
  watershed_characteristics$`AWC mapunit NA %`[row_num] <- 100*nrow(df[is.na(df[ ,'aws_H2Ocm']), ])/nrow(df)
  watershed_characteristics$`AWC component NA %`[row_num] <- 100 - 100*sum(df[ ,'awc_comp_pct']/100, na.rm = TRUE)/nrow(df)
  invisible(watershed_characteristics)
}

mu_statsgodata <- read.csv()
watershed_vars$soil_awc <- subs(watershed_vars$soil_mukey, mu_data, by='mukey', which='awc_H2Ocm_wtdavg')


test <- as.data.frame(test, xy=TRUE, na.rm=TRUE)
write.csv(test, paste(results_dir, '.csv', sep = ''), row.names=FALSE)
read.csv('')

#extra code from pre-function work
#step 4 - read in soil map units, delete features in ssurgo soils == 'NOTCOM' and then do a union with STATSGO inside the watershed boundary and rasterize
#had to run pit watershed separately because it had holes in the MU polygons
setwd(SSURGOdir)
paths <- list.dirs(SSURGOdir)
soil_fnames <- character()
for (i in 6:6) {
  setwd(paths[i])
  fnames <- list.files()
  soil_fnames[i-1] <- fnames[5]
}
for (i in 6:6){
  setwd(paths[i])
  ssurgo_soils <- shapefile(soil_fnames[i-1]) #arg goes here
  if (gIsValid(ssurgo_soils) == FALSE) {
    print(soil_fnames[i-1])
    print(gIsValid(ssurgo_soils, reason = TRUE))
    ssurgo_soils <- gBuffer(ssurgo_soils, width = 0, byid = T)
    print(gIsValid(ssurgo_soils, reason = TRUE))
  }
  soils_watershed <- ssurgo_soils
  if ('NOTCOM' %in% ssurgo_soils$musym) {
    setwd(STATSGOdir)
    statsgo_soils <- shapefile('statsgo2_CA_TA.shp') #arg goes here
    names(statsgo_soils) <- names(ssurgo_soils)
    statsgo_soils <- crop(statsgo_soils, ssurgo_soils)
    features_nodata <- which(ssurgo_soils$musym == 'NOTCOM')
    no_data_extent <- ssurgo_soils[features_nodata, ]
    statsgo_soils <- crop(statsgo_soils, no_data_extent)
    ssurgo_soils <- ssurgo_soils[-features_nodata, ]
    soils_watershed <- bind(ssurgo_soils, statsgo_soils)
  }
setwd(soilcombined)
#write shapefile to disk for rasterizing in ArcMap
shapefile(soils_watershed, soil_fnames[i-1], overwrite=TRUE)
}


setwd(file.path(SSURGOdir, 'uf18'))
soils_watershed <- shapefile('uf18_mapunits.shp')
setwd('C:\\Users\\smdevine\\Desktop\\SpatialData\\soils_data\\pit soils processing')
list.files()
statgo_clip <- shapefile('STATSGO_clip.shp')
names(statgo_clip) <- names(soils_watershed)
shapefile(statgo_clip, 'STATSGO_clip.shp', overwrite=TRUE)
#can't resolve this error, so completed rasterize task in ArcMAP
soils_r <- rasterize(x=test, y=raster_object, field='mukey', fun=min, small=TRUE)
plot(watershed)
plot(soils_watershed, add=T)
gIsValid(soils_watershed, reason = T)
test <- gBuffer(soils_watershed, width = 0, byid = T)
gIsValid(test, reason = T)
test <- spTransform(test, crs(raster_object))

#step 5a - read in CA BCM data and write the set to a stack which will be saved so that we can skip this step the next time
setwd(CA_BCMdir)
CA_BCMdirs <- list.dirs(CA_BCMdir)
CA_BCM_data <- list()
for (i in 2:4) {
  setwd(CA_BCMdirs[i])
  fnames <- list.files()
  CA_BCM_data[i-1] <- raster(fnames[7])
}
for (i in 5:16) {
  setwd(CA_BCMdirs[i])
  fnames <- list.files()
  CA_BCM_data[i-1] <- raster(fnames[8])
}
CA_BCM_data <- brick(CA_BCM_data)
setwd(CA_BCMdir)
writeRaster(CA_BCM_data, 'CA_BCM_30yr_summaries.tif', format='GTiff', overwrite=TRUE)
