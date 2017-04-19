#working with the climo data from TopoWx
library(sp)
library(raster)
library(rgeos)
#attempt to run this through the Toshiba external hard drive crashed with Pit R watershed so switched to desktop 
mainDir <- 'E:/TopoWx'
homeDir <- 'C:\\Users\\smdevine\\Desktop\\Forest dieoff and hydrology\\TopoWx'
setwd(homeDir)
results <- 'results_TMIN'
if (file.exists(file.path(homeDir, results)) == FALSE) {
  dir.create(file.path(homeDir, results))
}
results <- file.path(homeDir, results)
TMAX_dir <- file.path(homeDir, 'TMAX')
TMIN_dir <- file.path(homeDir, 'TMIN')
boundaries <- file.path(homeDir, 'watershed_boundaries_revised')

#start function here
topo_wx <- function(data_dir, shapefile, HUC_name, HUC_alias) {
  startyear <- 1948
  endyear <- 2015
  rownum <- endyear - startyear + 1
  temp_summary <- data.frame(matrix(ncol=13, nrow=rownum))
  names(temp_summary) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  setwd(boundaries)
  temp_name <- gUnaryUnion(shapefile(shapefile))
  setwd(data_dir) #arg
  tmax <- list.files()
  for (j in 1:length(tmax)){
    year <- tmax[j]
    year <- substr(year, 6, 9)
    temp_summary[j, 1] <- year
    all_months <- brick(tmax[j])
    c <- crs(all_months)
    temp_name <- spTransform(temp_name, c)
    all_months <- crop(all_months, temp_name) #note that the returned raster has an extent defined as a rectangle based on the min and max coordinates of the polygon.  this seems to speed up the extraction process, because the raster being processed is smaller
    for (i in 1:12){
      month <- all_months[[i]]
      temp_monthly <- extract(month, temp_name, fun=mean) #this is necessary to calculate the stats based on the actual geometry of the watershed polygon
      temp_summary[j, i+1] <- temp_monthly
    }
  }
  setwd(results)
  write.csv(temp_summary, paste(HUC_name, HUC_alias, '_MO_tmin.csv', sep=''), row.names=FALSE)
}

#run function for each watershed and variable.  output data is still organized by calendar year
topo_wx(TMAX_dir, 'uf.sacramento.headwaters.shp', 'SDT', 'Upper_Sacramento_R_test')
topo_wx(TMAX_dir, 'uf.mccloud.shp', 'MSS', 'Upper_McCloud_R')
topo_wx(TMAX_dir, 'uf.pit.river.shp', 'PSH', 'Upper_Pit_R')
topo_wx(TMAX_dir, 'uf8.feather.shp', 'UF8', 'Upper_Feather_R')
topo_wx(TMAX_dir, 'uf9.yuba.shp', 'UF9', 'Upper_Yuba_R')
topo_wx(TMAX_dir, 'uf11.american.shp', 'UF11', 'Upper_American_R')
topo_wx(TMAX_dir, 'uf13.cosumnes.shp',	'UF13',	'Upper_Cosumnes_R')
topo_wx(TMAX_dir, 'uf14.mokelumne.shp',	'UF14',	'Upper_Mokelumne_R1of2')
topo_wx(TMAX_dir, 'uf16.stanislaus.shp', 'UF16',	'Upper_Stanislaus_R')
topo_wx(TMAX_dir, 'uf18.tuolumne.shp', 'UF18', 'Upper_Tuolomne_R')
topo_wx(TMAX_dir, 'uf19.merced.shp', 'UF19', 'Upper_Merced_R')
topo_wx(TMAX_dir, 'uf22.san.joaquin.shp', 'UF22', 'Upper_San Joaquin_R')
topo_wx(TMAX_dir, 'uf10.bear.shp', 'UF10', 'Upper_Bear_R')
topo_wx(TMAX_dir, 'uf15.calaveras.shp', 'UF15', 'Upper_Calaveras_R')
topo_wx(TMAX_dir, 'uf20.chowchilla.shp', 'UF20', 'Upper_Chowchilla_R')
topo_wx(TMAX_dir, 'uf21.fresno.shp', 'UF21', 'Upper_Fresno_R')
topo_wx(TMAX_dir, 'uf.kings.shp', 'KGF', 'Upper_Kings_R')
topo_wx(TMAX_dir, 'uf.kaweah.shp', 'KWT', 'Upper_Kaweah_R')
topo_wx(TMAX_dir, 'uf.tule.shp', 'SCC', 'Upper_Tule_R')
topo_wx(TMAX_dir, 'uf.kern.shp', 'KRI', 'Upper_Kern_R')

topo_wx(TMIN_dir, 'uf.sacramento.headwaters.shp', 'SDT', 'Upper_Sacramento_R')
topo_wx(TMIN_dir, 'uf.mccloud.shp', 'MSS', 'Upper_McCloud_R')
topo_wx(TMIN_dir, 'uf.pit.river.shp', 'PSH', 'Upper_Pit_R')
topo_wx(TMIN_dir, 'uf8.feather.shp', 'UF8', 'Upper_Feather_R')
topo_wx(TMIN_dir, 'uf9.yuba.shp', 'UF9', 'Upper_Yuba_R')
topo_wx(TMIN_dir, 'uf11.american.shp', 'UF11', 'Upper_American_R')
topo_wx(TMIN_dir, 'uf13.cosumnes.shp',	'UF13',	'Upper_Cosumnes_R')
topo_wx(TMIN_dir, 'uf14.mokelumne.shp',	'UF14',	'Upper_Mokelumne_R')
topo_wx(TMIN_dir, 'uf16.stanislaus.shp', 'UF16',	'Upper_Stanislaus_R')
topo_wx(TMIN_dir, 'uf18.tuolumne.shp', 'UF18', 'Upper_Tuolomne_R')
topo_wx(TMIN_dir, 'uf19.merced.shp', 'UF19', 'Upper_Merced_R')
topo_wx(TMIN_dir, 'uf22.san.joaquin.shp', 'UF22', 'Upper_San Joaquin_R')
topo_wx(TMIN_dir, 'uf10.bear.shp', 'UF10', 'Upper_Bear_R')
topo_wx(TMIN_dir, 'uf15.calaveras.shp', 'UF15', 'Upper_Calaveras_R')
topo_wx(TMIN_dir, 'uf20.chowchilla.shp', 'UF20', 'Upper_Chowchilla_R')
topo_wx(TMIN_dir, 'uf21.fresno.shp', 'UF21', 'Upper_Fresno_R')
topo_wx(TMIN_dir, 'uf.kings.shp', 'KGF', 'Upper_Kings_R')
topo_wx(TMIN_dir, 'uf.kaweah.shp', 'KWT', 'Upper_Kaweah_R')
topo_wx(TMIN_dir, 'uf.tule.shp', 'SCC', 'Upper_Tule_R')
topo_wx(TMIN_dir, 'uf.kern.shp', 'KRI', 'Upper_Kern_R')
