#script to extract STATSGO data of relevance to csv files for processing in statsgo2_analysis.R
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
STATSGOtab <- file.path(mainDir, 'soils_data/STATSGO2_CA/soils/wss_gsmsoil_CA_[2006-07-06]/wss_gsmsoil_CA_[2006-07-06]/tabular')
chorizon <- 'chorizon.txt'
chfrags <- 'chfrags.txt'
component <- 'comp.txt'
corestrictions <- 'crstrcts.txt'
muaggatt <- 'muaggatt.txt'
outpath <- file.path(mainDir, 'soils_data/STATSGO2_CA/summarized_data')
setwd(STATSGOtab)
#read in chorizon data and select appropriate columns and name them
chorizon_data <- read.csv(chorizon, header = FALSE, sep = "|", stringsAsFactors = FALSE)
chorizon_data <- chorizon_data[ ,c(1, 7, 10, 34, 61, 67, 73, 86, 170, 171)]
colnames(chorizon_data) <- c('hzname', 'hzdept_r', 'hzdepb_r', 'sandtotal_r', 'claytotal_r', 'om_r', 'dbthirdbar_r', 'awc_r', 'cokey', 'hzkey')
head(chorizon_data)
#read in cfrags data and select appropriate columns and name them
chfrags_data <- read.csv(chfrags, header = FALSE, sep = "|", stringsAsFactors = FALSE)
chfrags_data <- chfrags_data[ , c(2, 6, 11)]
colnames(chfrags_data) <- c('fragvol_r', 'fragsize_r', 'hzkey')
frag_vol_r_sum <- as.data.frame(tapply(chfrags_data$fragvol_r, chfrags_data$hzkey, sum)) #this has less rows than chorizon, so must assume that lack of data means lack of frags(except for restrictive horizons)
colnames(frag_vol_r_sum) <- 'fragvol_r_sum'
frag_vol_r_sum$hzkey <- rownames(frag_vol_r_sum)
chorizon_data <- merge(chorizon_data, frag_vol_r_sum, by='hzkey', all = TRUE)
#chorizon_data$frag_vol_r_sum[is.na(chorizon_data$frag_vol_r_sum)] <- 0
#setwd(outpath)
#write.csv(chorizon_data, 'STATSGO2_hz_data.csv', row.names=FALSE) #already wrote data to file
#read in component data and select appropriate columns and name them
component_data <- read.csv(component, header = FALSE, sep = "|", stringsAsFactors = FALSE)
component_data <- component_data[ , c( 108, 109, 4, 1, 2, 3, 6, 84, 86)]
colnames(component_data) <- c('mukey', 'cokey', 'compname', 'comppct_l', 'comppct_r', 'comppct_h', 'majcompflag', 'taxorder', 'taxgrtgroup')
corestrictions_data <- read.csv(corestrictions, header = FALSE, sep='|', stringsAsFactors = FALSE)
corestrictions_data <- corestrictions_data[ , c(1, 3, 4, 5, 12)]
colnames(corestrictions_data) <- c('reskind', 'resdept_l', 'resdept_r', 'resdept_h', 'cokey')
component_data <- merge(component_data, corestrictions_data, by='cokey', all = TRUE)
setwd(outpath)
write.csv(component_data, 'STATSGO2_comp_data.csv', row.names=FALSE)
muaggatt_data <- read.csv(muaggatt, header = FALSE, sep='|', stringsAsFactors = FALSE)
muaggatt_data <- muaggatt_data[ ,c(2, 15, 40)]
colnames(muaggatt_data) <- c('muname', 'aws0150wta', 'mukey')
setwd(outpath)
write.csv(muaggatt_data, 'STATSGO2_mu_data.csv', row.names=FALSE)
