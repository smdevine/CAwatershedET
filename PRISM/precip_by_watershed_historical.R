#this R script is designed to process a set of monthly PRISM precipitation files relative to a watershed boundary (as defined by a shapefile), so as to produce a matrix of mean precipitation values by month (columns) and water year (rows) for that watershed
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(geosphere)
mainDir <- 'C:/Users/smdevine/Desktop/spatial/PRISM'
PRISM_fname <- 'PRISM_ppt_stable_4kmM3_198101_201509_bil' #have to change this for the modern set of files; 1981 Jan-Sep files moved to historical for WY summary purposes
setwd(mainDir)
results <- '/results/monthly_precip_corrected/1981_2015' #modify for modern data set
boundaries <- file.path(mainDir, 'watershed_boundaries_revised')
#create results directory (need if/else statement here)
if (file.exists(file.path(mainDir, results)) == FALSE) {
  dir.create(file.path(mainDir, results))
}
outpath <- file.path(mainDir, results)
PRISM <- file.path(mainDir, PRISM_fname)
#create table of codes to find PRISM files
startyear <- 1981
endyear <- 2015
rownum <- endyear - startyear
datecodes_his <- data.frame(matrix(ncol=13, nrow=rownum))
names(datecodes_his) <- c("WY", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

#write to table of codes
for (i in 1:nrow(datecodes_his)) {
  datecodes_his[i, 1] <- startyear+1
  datecodes_his[i, 2] <- paste(startyear, '10', sep="")
  datecodes_his[i, 3] <- paste(startyear, '11', sep="")
  datecodes_his[i, 4] <- paste(startyear, '12', sep="")
  datecodes_his[i, 5] <- paste(startyear+1, '01', sep="")
  datecodes_his[i, 6] <- paste(startyear+1, '02', sep="")
  datecodes_his[i, 7] <- paste(startyear+1, '03', sep="")
  datecodes_his[i, 8] <- paste(startyear+1, '04', sep="")
  datecodes_his[i, 9] <- paste(startyear+1, '05', sep="")
  datecodes_his[i, 10] <- paste(startyear+1, '06', sep="")
  datecodes_his[i, 11] <- paste(startyear+1, '07', sep="")
  datecodes_his[i, 12] <- paste(startyear+1, '08', sep="")
  datecodes_his[i, 13] <- paste(startyear+1, '09', sep="")
  startyear <- startyear + 1
}

#initialize data.frame of PRISM filenames (water year precip totals)
filenames_PRISM <- datecodes_his
#loop through files, extract data by month, and write ouput to results folder if so desired
for (i in 1:nrow(datecodes_his)) {
  for (j in 2:ncol(datecodes_his)) {
    filenames_PRISM[i, j] <- paste("PRISM_ppt_stable_4kmM3_", datecodes_his[i, j], '_bil.bil',  sep="") #paste 'PRISM_ppt_stable_4kmM3_' needs to be checked depending upon PRISM filename for historial vs. modern periods
  }
}

#run code from here down
#read in shapefile (gUnaryUnion was tested to ensure it does gives same total geometry)
HUC_precip <- function(shapefile, HUC_name, HUC_alias) {
  setwd(boundaries)
  temp_name <- gUnaryUnion(shapefile(shapefile))
  setwd(PRISM)
  PRISM_monthly <- filenames_PRISM
  for (i in 1:nrow(filenames_PRISM)) {
    for (j in 2:ncol(filenames_PRISM)) {
      temp_raster <- crop(raster(filenames_PRISM[i, j]), temp_name) #note that the returned raster has an extent defined as a rectangle based on the min and max coordinates of the polygon.  this seems to speed up the extraction process
      tot_precip_mo <- extract(temp_raster, temp_name, fun=mean) #this is necessary to calculate the stats based on the actual geometry of the watershed polygon
      PRISM_monthly[i, j] <- tot_precip_mo
    }
  }
  print(paste(areaPolygon(temp_name)/4046.86, HUC_alias)) #print out watershed acres 
  names(PRISM_monthly) <- c("water year", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  setwd(outpath)
  write.csv(PRISM_monthly, paste(HUC_name, HUC_alias, '_MO_precip.csv', sep=''), row.names=FALSE)
  return(PRISM_monthly)
}
#run function for each watershed
UF6_result <- HUC_precip('uf6.sacramento.shp', 'UF6', 'Upper_Sacramento_R') #does not include Goose Lake, assuming it's a closed basin
UF8_result <- HUC_precip('uf8.feather.shp', 'UF8', 'Upper_Feather_R')
UF9_result <- HUC_precip('uf9.yuba.shp', 'UF9',	'Upper_Yuba_R') #result deleted on 9/2/16 because of watershed geom error
UF11_result <- HUC_precip('uf11.american.shp', 'UF11', 'Upper_American_R')
UF13_result <- HUC_precip('uf13.cosumnes.shp',	'UF13',	'Upper_Cosumnes_R')
UF14_result <- HUC_precip('uf14.mokelumne.shp',	'UF14',	'Upper_Mokelumne_R')
UF16_result <- HUC_precip('uf16.stanislaus.shp', 'UF16',	'Upper_Stanislaus_R')
UF18_result <- HUC_precip('uf18.tuolumne.shp', 'UF18', 'Upper_Tuolomne_R')
UF19_result <- HUC_precip('uf19.merced.shp', 'UF19', 'Upper_Merced_R')
UF22_result <- HUC_precip('uf22.san.joaquin.shp', 'UF22', 'Upper_San Joaquin_R')

#analyses for new and corrected watersheds on 9.2.16
UF6_upperSac <- HUC_precip('uf.sacramento.headwaters.shp', 'SDT', 'Upper_Sacramento_R')
UF6_McCloud <- HUC_precip('uf.mccloud.shp', 'MSS', 'Upper_McCloud_R')
UF6_Pit <- HUC_precip('uf.pit.river.shp', 'PSH', 'Upper_Pit_R')
UF9_Yuba <- HUC_precip('uf9.yuba.shp', 'UF9', 'Upper_Yuba_R')
UF10_Bear <- HUC_precip('uf10.bear.shp', 'UF10', 'Upper_Bear_R')
UF15_Calaveras <- HUC_precip('uf15.calaveras.shp', 'UF15', 'Upper_Calaveras_R')
UF20_Chowchilla <- HUC_precip('uf20.chowchilla.shp', 'UF20', 'Upper_Chowchilla_R')
UF21_Fresno <- HUC_precip('uf21.fresno.shp', 'UF21', 'Upper_Fresno_R')
UF23_Kings <- HUC_precip('uf.kings.shp', 'KGF', 'Upper_Kings_R')
UF23_Kaweah <- HUC_precip('uf.kaweah.shp', 'KWT', 'Upper_Kaweah_R')
UF23_Tule <- HUC_precip('uf.tule.shp', 'SCC', 'Upper_Tule_R')
UF23_Kern <- HUC_precip('uf.kern.shp', 'KRI', 'Upper_Kern_R')