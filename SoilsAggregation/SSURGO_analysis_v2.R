#script to aggregate soils data relevant to water storage for watershed ET analysis
#hard part was trying to create horizon nomenclature where there was not much to begin with in these old surveys
#soils data is read-in from CSV files created from SSURGO that were queried by Mike Walkinshaw.  queries were specific to my AOI
#it gets 700+ lines complicated, but there are notes 
dirnumber <- 20 #argument for manually running function
run_dup_ck <- 'yes' #argument for manually running function
library(plyr)
library(dplyr)
library(nnet)
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
SSURGOdir <- file.path(mainDir, 'soils_data/SSURGOrevised')
OSDdir <- file.path(mainDir, 'soils_data/master_soils_watershed_study')
results <- file.path(mainDir, 'soils_data/SSURGO_summary')
setwd(SSURGOdir)
ssurgo_dirs <- list.dirs(SSURGOdir)
sum_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {sum(x, na.rm = TRUE)}
}
range_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {range(x, na.rm = TRUE)}
}
# run_dup_ck should be 'yes' on initial runs and runs with watersheds with no known duplicate cokeys and 'no' on runs with watersheds with legitimate, duplicate cokeys
aggregate_SSURGO <- function(dirnumber, run_dup_ck) {
  watershed_name <- basename(ssurgo_dirs[dirnumber])
  if (file.exists(file.path(results, watershed_name)) == FALSE) {
    dir.create(file.path(results, watershed_name))
  }
  outpath <- file.path(results, watershed_name)
  ssurgo_dir <- ssurgo_dirs[dirnumber]
  ssurgo_tables <- list.files(ssurgo_dir, pattern = glob2rx('*.csv'))
#comp = 1, horizon = 2, and mu_area = 3, mu_table = 4
  setwd(ssurgo_dir)
  ssurgo_comp <- read.csv(ssurgo_tables[1], stringsAsFactors = FALSE,  na.strings=c(""," ", 'NA'))
  ssurgo_comp$majcompflag[ssurgo_comp$majcompflag=='No '] <- 'No' #strip out the space after 'No'
  ssurgo_horizon <-read.csv(ssurgo_tables[2], stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
  ssurgo_mu_area <- read.csv(ssurgo_tables[3], stringsAsFactors = FALSE)
  ssurgo_mu <- read.csv(ssurgo_tables[4], stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
  print(paste(nrow(ssurgo_mu_area), nrow(ssurgo_mu), 'should match for mu level data.'))
  ssurgo_mu <- merge(ssurgo_mu, ssurgo_mu_area, by='mukey')
#check for cokey duplication as a result of component restriction. extract unique elements
  if (run_dup_ck=='yes') {
    ssurgo_comp_d <- ssurgo_comp[!duplicated(ssurgo_comp$cokey),]
    stopifnot(identical(ssurgo_comp_d,  unique(ssurgo_comp)))
    ssurgo_comp_d <- NULL
  }
  txt1 <- paste(watershed_name, 'is the watershed being processed.')
  txt2 <- paste(length(unique(ssurgo_horizon$cokey)), 'unique cokeys in soil horizon data table.')
  txt3 <- paste(length(unique(ssurgo_comp$cokey)), 'unique cokeys in soil component data table.')
  txt4 <- paste(nrow(ssurgo_comp), 'rows in the component data table.')
  txt5 <- paste(paste(unique(ssurgo_comp$reskind), collapse=', '), 'are the unique kinds of restrictions.')
  txt6 <- paste(length(unique(ssurgo_comp$mukey)), 'unique mukeys in soil component data table')
  txt7 <- paste(length(unique(ssurgo_mu$mukey)), 'unique mukeys in soil mapunit data table')
#QC code to save csv file for inspecting soil horizons with NA for AWC
  i <- which(is.na(ssurgo_horizon$awc_r))
  ssurgo_horizon_NAdata <- ssurgo_horizon[i,]
  j <- ssurgo_horizon$cokey[i]
  k <- match(j, ssurgo_comp$cokey)
  l <- ssurgo_comp[k,]
  ssurgo_horizon_NAdata <- merge(ssurgo_horizon_NAdata, l, by='cokey')
  setwd(outpath)
  write.csv(ssurgo_horizon_NAdata, paste(watershed_name, '_horizons_NA_AWC.csv', sep = ''), row.names=FALSE)
  ssurgo_horizon$hzthickness <- ssurgo_horizon$hzdepb_r - ssurgo_horizon$hzdept_r
  ssurgo_horizon$hzawc_cm <- ssurgo_horizon$hzthickness*ssurgo_horizon$awc_r
  ssurgo_horizon$fragvol_temp <- ssurgo_horizon$hzthickness*ssurgo_horizon$fragvol_r_sum
  ssurgo_horizon$clay_content <- (ssurgo_horizon$claytotal_r/100)*ssurgo_horizon$dbthirdbar_r*((100-ssurgo_horizon$fragvol_r_sum)/100)*ssurgo_horizon$hzthickness*10
  ssurgo_horizon$om_content <- (ssurgo_horizon$om_r/100)*ssurgo_horizon$dbthirdbar_r*((100-ssurgo_horizon$fragvol_r_sum)/100)*ssurgo_horizon$hzthickness*10
  ssurgo_horizon$awc_qc <- ssurgo_horizon$hzawc_cm
  ssurgo_horizon$awc_qc[ssurgo_horizon$awc_qc > 0] <- 1 #this means that horizons with a reported '0' are not included as part of the depth calc
  comp_awc_thickness <- as.data.frame(tapply(ssurgo_horizon$awc_qc*ssurgo_horizon$hzthickness, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_awc_thickness) <- 'awc_soilthickness_cm'
  comp_awc_thickness$cokey <- rownames(comp_awc_thickness)
  ssurgo_horizon$clay_qc <- ssurgo_horizon$clay_content
  ssurgo_horizon$clay_qc[ssurgo_horizon$clay_qc > 0] <- 1 #this means that horizons with a reported '0' are not included as part of the depth calc
  comp_clay_thickness <- as.data.frame(tapply(ssurgo_horizon$clay_qc*ssurgo_horizon$hzthickness, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_clay_thickness) <- 'clay_thickness_calc'
  comp_clay_thickness$cokey <- rownames(comp_clay_thickness)
  ssurgo_horizon$om_qc <- ssurgo_horizon$om_content
  ssurgo_horizon$om_qc[ssurgo_horizon$om_qc > 0] <- 1 #this means that horizons with a reported '0' are not included as part of the depth calc
  comp_om_thickness <- as.data.frame(tapply(ssurgo_horizon$om_qc*ssurgo_horizon$hzthickness, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_om_thickness) <- 'om_thickness_calc'
  comp_om_thickness$cokey <- rownames(comp_om_thickness)
  ssurgo_horizon$frag_qc <- ssurgo_horizon$fragvol_r_sum
  ssurgo_horizon$frag_qc[ssurgo_horizon$frag_qc > 0] <- 1 #this means that horizons with a reported '0' are not included as part of the depth calc (not currently aggregating this data)
  comp_frag_thickness <- as.data.frame(tapply(ssurgo_horizon$frag_qc*ssurgo_horizon$hzthickness, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_frag_thickness) <- 'frag_thickness_calc'
  comp_frag_thickness$cokey <- rownames(comp_frag_thickness)
  comp_awc_H2Ocm <- as.data.frame(tapply(ssurgo_horizon$hzawc_cm, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_awc_H2Ocm) <- 'awc_H2Ocm'
  comp_awc_H2Ocm$cokey <- rownames(comp_awc_H2Ocm)
  comp_thickness <- as.data.frame(tapply(ssurgo_horizon$hzthickness, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_thickness) <- 'thickness_cm'
  comp_thickness$cokey <- rownames(comp_thickness)
  comp_fragvol_avg <- as.data.frame(tapply(ssurgo_horizon$fragvol_temp, ssurgo_horizon$cokey, sum_modified)/comp_frag_thickness$frag_thickness_calc) #correct comp_thickness for only those horizons with fragvol data THIS NEEDS REVIEW
  colnames(comp_fragvol_avg) <- 'fragvol_wtdavg'
  comp_fragvol_avg$cokey <- rownames(comp_fragvol_avg)
  comp_clay_kgm2 <- as.data.frame(tapply(ssurgo_horizon$clay_content, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_clay_kgm2) <- 'clay_kgm2'
  comp_clay_kgm2$cokey <- rownames(comp_clay_kgm2)
  comp_om_kgm2 <- as.data.frame(tapply(ssurgo_horizon$om_content, ssurgo_horizon$cokey, sum_modified))
  colnames(comp_om_kgm2) <- 'om_kgm2'
  comp_om_kgm2$cokey <- rownames(comp_om_kgm2)
  comp_bottom_hz <- as.data.frame(ssurgo_horizon %>% group_by(cokey) %>% slice(which.max(hzdept_r))) #select the deepest horizons by cokey
  comp_bottom_hz <- comp_bottom_hz[ , c('cokey', 'hzname', 'hzdept_r', 'hzdepb_r', 'awc_r')]
  colnames(comp_bottom_hz)[2:5] <- c('hzname_deepest', 'hzdept_r_deepest', 'hzdepb_r_deepest', 'awc_r_deepest')
  comp_hzsummary <- join_all(list(comp_thickness, comp_awc_H2Ocm, comp_awc_thickness, comp_fragvol_avg, comp_frag_thickness, comp_om_kgm2, comp_om_thickness, comp_clay_kgm2, comp_clay_thickness, comp_bottom_hz), by = 'cokey')
  rownames(comp_hzsummary) <- NULL
  setwd(outpath)
  write.csv(comp_hzsummary, paste(watershed_name, '_initial_hzlevel_aggregation.csv', sep = ''), row.names = FALSE) #this shows the intitial results of the horizon level aggregation to component level
  ssurgo_comp <- merge(ssurgo_comp, comp_hzsummary, by='cokey', all = TRUE)
  txt12 <- paste('There are', sum(is.na(ssurgo_comp$awc_H2Ocm)), 'cokeys with no AWC data before modification of the database.')
#restrictive layer work
#first delete duplicate cokeys that have multiple restrictions by logic of importance to this analysis.  conditions listed below may have to be increased depending upon what duplicates are encountered.
  if (run_dup_ck=='no') {
    d <- which(duplicated(ssurgo_comp$cokey))
    d2 <- which(duplicated(ssurgo_comp$cokey, fromLast = TRUE))
    d_all <- c(d, d2)
    ssurgo_comp_d <- ssurgo_comp[d, ]
    ssurgo_comp_d2 <- ssurgo_comp[d2, ]
    ssurgo_comp <- ssurgo_comp[-d_all, ]
    d_remove_i <- vector()
    d2_remove_i2 <- vector()
    for (i in 1:nrow(ssurgo_comp_d)) {
      if (ssurgo_comp_d$cokey[i] != ssurgo_comp_d2$cokey[i]) {
        stop("Cokeys do not match in the duplicate cokey removal section of the script.")
      }
      if (ssurgo_comp_d$reskind[i]=='Paralithic bedrock' & ssurgo_comp_d2$reskind[i]=='Lithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Paralithic bedrock restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Paralithic bedrock' & ssurgo_comp_d$reskind[i]=='Lithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Paralithic bedrock restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Duripan' & ssurgo_comp_d2$reskind[i]=='Lithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Duripan restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Duripan' & ssurgo_comp_d$reskind[i]=='Lithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Duripan restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Duripan' & ssurgo_comp_d2$reskind[i]=='Paralithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Duripan restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Duripan' & ssurgo_comp_d$reskind[i]=='Paralithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Duripan restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Duripan' & ssurgo_comp_d2$reskind[i]=='Duripan' & (ssurgo_comp_d$resdept_r[i] > ssurgo_comp_d2$resdept_r[i])) {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with deeper Duripan restrictionwhere there are two Duripan layers, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Duripan' & ssurgo_comp_d2$reskind[i]=='Duripan' & (ssurgo_comp_d$resdept_r[i] < ssurgo_comp_d2$resdept_r[i])) {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with deeper Duripan restriction where there are two Duripan layers, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Cemented horizon' & ssurgo_comp_d2$reskind[i]=='Lithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Cemented horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Cemented horizon' & ssurgo_comp_d$reskind[i]=='Lithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Cemented horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Fragipan' & ssurgo_comp_d2$reskind[i]=='Densic material') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Fragipan horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Fragipan' & ssurgo_comp_d$reskind[i]=='Densic material') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Fragipan horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Abrupt textural change' & ssurgo_comp_d2$reskind[i]=='Paralithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Abrupt textural change horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Abrupt textural change' & ssurgo_comp_d$reskind[i]=='Paralithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Abrupt textural change horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Abrupt textural change' & ssurgo_comp_d2$reskind[i]=='Duripan') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Abrupt textural change horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Abrupt textural change' & ssurgo_comp_d$reskind[i]=='Duripan') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Abrupt textural change horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Undefined' & ssurgo_comp_d2$reskind[i]=='Paralithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Undefined horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Undefined' & ssurgo_comp_d$reskind[i]=='Paralithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Undefined horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d$reskind[i]=='Undefined' & ssurgo_comp_d2$reskind[i]=='Lithic bedrock') {
        d_remove_i <- c(d_remove_i, i) #this will remove cokey with Undefined horizon restriction, but data still available in horizon table
        next
      }
      if (ssurgo_comp_d2$reskind[i]=='Undefined' & ssurgo_comp_d$reskind[i]=='Lithic bedrock') {
        d2_remove_i2 <- c(d2_remove_i2, i) #this will remove cokey with Undefined horizon restriction, but data still available in horizon table
        next
      }
    }
    ssurgo_comp_d <- ssurgo_comp_d[-d_remove_i,]
    ssurgo_comp_d2 <- ssurgo_comp_d2[-d2_remove_i2,]
    ssurgo_comp_d <- rbind(ssurgo_comp_d, ssurgo_comp_d2)
    ssurgo_comp <- rbind(ssurgo_comp, ssurgo_comp_d)
    ssurgo_comp_d <- ssurgo_comp[!duplicated(ssurgo_comp$cokey),]
    stopifnot(identical(ssurgo_comp_d, unique(ssurgo_comp)))
    ssurgo_comp_d <- NULL
    ssurgo_comp_d2 <- NULL
  }
#add area info to component table
  ssurgo_comp <- merge(ssurgo_comp, ssurgo_mu, by='mukey')
  ssurgo_comp['aws0150wta'] <- NULL #get rid of mu level aws info from merge operation in component table
  colnames(ssurgo_comp)[ncol(ssurgo_comp)] <- 'mu_hectares'
  ssurgo_comp$hectares <- ssurgo_comp$mu_hectares*(ssurgo_comp$comppct_r/100) #correct hectares based upon component percentage
  txt17 <- paste('Major comonents total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='Yes')]), digits = 0), 'hectares.') #add up major comp acreage
  txt18 <- paste('Minor components total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='No')]), digits = 0), 'hectares.') #add up minor comp acreage
#fill in restriciton information for minor components (this is more robust version to handle multiple possible restrictions)
  ssurgo_comp$reskind[which(is.na(ssurgo_comp$reskind) & ssurgo_comp$majcompflag=='Yes')] <- 'Zilch'
  ssurgo_comp$reskind[ssurgo_comp$compname=='Rock outcrop'] <- 'Lithic bedrock' #because some major components called 'Rock outcrop' don't have a listed lithic contact and the above script only fills in data for minor components
  ssurgo_comp$resdept_r[ssurgo_comp$compname=='Rock outcrop'] <- 0
  majcomps_reskinds <- ssurgo_comp[which(ssurgo_comp$majcompflag=='Yes'), ]
  reskind_area_by_compname <- as.data.frame(tapply(majcomps_reskinds$hectares, list(majcomps_reskinds$compname, majcomps_reskinds$reskind), sum))
  resdept_compcalc <- as.data.frame(tapply(majcomps_reskinds$resdept_r*majcomps_reskinds$hectares, list(majcomps_reskinds$compname, majcomps_reskinds$reskind), sum, na.rm=TRUE))
  resdept_comp_wtdavg <- resdept_compcalc/reskind_area_by_compname
  resdept_comp_wtdavg$Zilch[resdept_comp_wtdavg$Zilch==0] <- 9999 #use 9999 as a dummy depth for major components having no listed restrictions
  reskind_synthesis <- as.data.frame(matrix(nrow=length(reskind_area_by_compname), ncol=9))
  colnames(reskind_synthesis) <- c('compname', 'reskind1', 'reskind2', 'reskind3', 'reskind1_p', 'reskind2_p', 'resdept_reskind1', 'resdept_reskind2', 'resdept_reskind3')
  save_compnames <- vector()
  for (i in 1:nrow(reskind_area_by_compname)){
    reskind_synthesis[i, 1] <- rownames(reskind_area_by_compname)[i]
    a <- as.matrix(reskind_area_by_compname[i, ])
    a <- t(a)
    a <- a[which(a!=0), , drop=FALSE]
    b <- as.matrix(resdept_comp_wtdavg[i, ])
    b <- t(b)
    b <- b[which(!is.na(b)), , drop=FALSE]
    if (nrow(a)>3) {
      stop(print(paste("There are more than three kinds of restrictions for the", rownames(reskind_area_by_compname)[i], "component. Script most be edited to handle this at line 235.")))
    }
    if (nrow(a)==3) {
      print(paste('WARNING!', rownames(reskind_area_by_compname)[i], 'has conflicting restrictive layers:', paste(row.names(a), collapse = ', ')))
      #a <- a[which.is.max(a), ] #no longer breaking ties at random so as to capture distinct possibilities for minor components
      reskind_synthesis[i, 2] <- row.names(a)[1]
      reskind_synthesis[i, 3] <- row.names(a)[2]
      reskind_synthesis[i, 4] <- row.names(a)[3]
      reskind_synthesis[i, 5] <- a[1]/sum(a)
      reskind_synthesis[i, 6] <- a[2]/sum(a)
      reskind_synthesis[i, 7] <- b[1]
      reskind_synthesis[i, 8] <- b[2]
      reskind_synthesis[i, 9] <- b[3]
      save_compnames <- c(save_compnames, rownames(reskind_area_by_compname)[i])
      next
    }
    if (nrow(a)==2) {
      print(paste('WARNING!', rownames(reskind_area_by_compname)[i], 'has conflicting restrictive layers:', paste(row.names(a), collapse = ', ')))
      #a <- a[which.is.max(a), ] #no longer breaking ties at random so as to capture distinct possibilities for minor components
      reskind_synthesis[i, 2] <- row.names(a)[1]
      reskind_synthesis[i, 3] <- row.names(a)[2]
      reskind_synthesis[i, 4] <- NA
      reskind_synthesis[i, 5] <- a[1]/sum(a)
      reskind_synthesis[i, 6] <- NA
      reskind_synthesis[i, 7] <- b[1]
      reskind_synthesis[i, 8] <- b[2]
      reskind_synthesis[i, 9] <- NA
      save_compnames <- c(save_compnames, rownames(reskind_area_by_compname)[i])
      next
    }
    else {
      reskind_synthesis[i, 2] <- row.names(a)
      reskind_synthesis[i, 3] <- NA
      reskind_synthesis[i, 4] <- NA
      reskind_synthesis[i, 5] <- NA
      reskind_synthesis[i, 6] <- NA
      reskind_synthesis[i, 7] <- b[1]
      reskind_synthesis[i, 8] <- NA
      reskind_synthesis[i, 9] <- NA
      }
  }
  #adjust 'zilch' back to 'none' for the major component data
  ssurgo_comp$reskind[ssurgo_comp$reskind=='Zilch'] <- 'None'
  reskind_synthesis$reskind1[reskind_synthesis$reskind1=='Zilch'] <- 'None'
  reskind_synthesis$reskind2[reskind_synthesis$reskind2=='Zilch'] <- 'None'
  reskind_synthesis$reskind3[reskind_synthesis$reskind3=='Zilch'] <- 'None'
  reskind_synthesis$resdept_reskind1[reskind_synthesis$resdept_reskind1==9999] <- NA
  reskind_synthesis$resdept_reskind2[reskind_synthesis$resdept_reskind2==9999] <- NA
  reskind_synthesis$resdept_reskind3[reskind_synthesis$resdept_reskind3==9999] <- NA
#THIS IS AN EXPEDIENT SOLUTION TO ALLOCATE MAJOR COMPONENT DATA TO MINOR COMPONENTS.  THE ALLOCATION IS BASED UPON AREAL PROPORTION OF THE MAJOR COMPONENTS.  HOWEVER, I AM NOT USING MINOR COMPONENT AREAL DATA TO GUIDE THE ALLOCATION PROCESS, WHICH WOULD BE MORE IDEAL BUT IS PROGRAMMITACALLY VERY MUCH A CHALLENGE.
  define_reskind <- function(df) {
    i <- which(is.na(df$reskind) & df$majcompflag=='No')  #this means that major components will not have their reskinds adjusted by the most common reskind for a given component name; also any minor component that had an identified reskind will not be adjusted
    for (j in 1:length(i)){
      reskind_synthesis_row <- match(df$compname[i[j]], reskind_synthesis$compname)
      if (is.na(reskind_synthesis_row)) {
        next
      }
      if(!is.na(reskind_synthesis$reskind2_p[reskind_synthesis_row])) {
        reskind_pick <- sample(2:4, size=1, prob = c(reskind_synthesis$reskind1_p[reskind_synthesis_row], reskind_synthesis$reskind2_p[reskind_synthesis_row], (1-reskind_synthesis$reskind1_p[reskind_synthesis_row]-reskind_synthesis$reskind2_p[reskind_synthesis_row]))) #pick reskind1, reskind2, or reskind3 based on the areal proportions determined during the reskind_synthesis script above; this returns a 2, 3, or 4, referring to either column 2, 3, or 4 of reskind_synthesis
        df$reskind[i[j]] <- reskind_synthesis[reskind_pick][reskind_synthesis_row, ]#have to use comma here to reference the row, because column number indexing does not drop the 'data.frame' class when column is returned
        if (df$reskind[i[j]] != 'None') {
          df$resdept_r[i[j]] <- reskind_synthesis[ ,paste('resdept_reskind', as.character(reskind_pick-1), sep = '')][reskind_synthesis_row] #column indexing is by column names determined by reskind_pick
        }
        next
      }
      if (!is.na(reskind_synthesis$reskind1_p[reskind_synthesis_row])) {
        reskind_pick <- sample(2:3, size=1, prob = c(reskind_synthesis$reskind1_p[reskind_synthesis_row], (1-reskind_synthesis$reskind1_p[reskind_synthesis_row]))) #pick reskind1 or reskind2 based on the proportions determined during the reskind_synthesis script above; this returns a 2 or 3, referring to either column 2 or 3 of reskind_synthesis
        df$reskind[i[j]] <- reskind_synthesis[reskind_pick][reskind_synthesis_row, ]#have to use comma here to reference the row, because column number indexing does not drop the 'data.frame' class when column is returned
        if (df$reskind[i[j]] != 'None') {
          df$resdept_r[i[j]] <- reskind_synthesis[ ,paste('resdept_reskind', as.character(reskind_pick-1), sep = '')][reskind_synthesis_row]
        }
        next
      }
      df$reskind[i[j]] <- reskind_synthesis$reskind1[reskind_synthesis_row]
      if (df$reskind[i[j]] != 'None') {
        df$resdept_r[i[j]] <- reskind_synthesis$resdept_reskind1[reskind_synthesis_row]
      }
    }
    invisible(df)
  }
  ssurgo_comp <- define_reskind(ssurgo_comp)#define based on watershed data under consideration
  
#then do a cross-watershed reskind fix  
  setwd(OSDdir)
  reskind_synthesis <- read.csv('reskind_synthesis_all_watersheds.csv', stringsAsFactors = FALSE)
  ssurgo_comp <- define_reskind(ssurgo_comp) #define based on all watersheds data

#calculate percenatge of mapunit comps with restrictive layer
  ssurgo_comp$restr_dummy <- ssurgo_comp$resdept_r #copy depth to restrictive layer
  ssurgo_comp$restr_dummy[ssurgo_comp$restr_dummy >= 0] <- 1 #this creates a dummy variable to do matrix multiplication
  restr_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$restr_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is a component restriction
  colnames(restr_ppct_tot) <- 'restr_ppct_tot'
  restr_ppct_tot$mukey <- rownames(restr_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes

#fill in deepest horizon data with matching component name data and change 'H' horizon nomenclature to 'H'.  CHECK PRINT OUTPUT.
  no_restr_comps <- ssurgo_comp[-which(ssurgo_comp$reskind == 'Paralithic bedrock' | ssurgo_comp$reskind == 'Lithic bedrock' | ssurgo_comp$reskind == "Densic bedrock" | ssurgo_comp$reskind == "Densic material"), ] #this is to pull out those components without an identified densic, paralithic or lithic contact from this analysis
  hzbottom_by_compname <- tapply(as.factor(no_restr_comps$hzname_deepest), no_restr_comps$compname, summary)
  hzbottom_synthesis <- as.data.frame(matrix(nrow=length(hzbottom_by_compname), ncol=2))
  colnames(hzbottom_synthesis) <- c('compname', 'hzname_deepest_simplified')
  txt8 <- paste(paste(names(hzbottom_by_compname[[1]]), collapse = ', '), 'are the bottom horizon names for components without a restriction before modification.')
  hz_names <- names(hzbottom_by_compname[[1]])
  for (i in 1:length(hzbottom_by_compname)){
    hzbottom_synthesis[i, 1] <- names(hzbottom_by_compname[i])
    a <- as.matrix(hzbottom_by_compname[[i]])
    if ("NA's" %in% row.names(a)){
      a <- as.matrix(a[-which(row.names(a)=="NA's"), ])
    }
    if (sum(a) == 0){
      hzbottom_synthesis[i, 2] <- NA
      next
    }
    a <- as.matrix(a[which(a!=0), ])
    if (nrow(a)==1 & 'H' %in% strsplit(rownames(a)[[1]], split='')){
      hzbottom_synthesis[i, 2] <- 'H'
      next
    }
    for (m in 1:nrow(a)){
      row.names(a)[m] <- gsub('[0-9]+', '', row.names(a)[m]) #strip out numbers from the horizon names so we can see the first letter of each horizon name
      row.names(a)[m] <- gsub('[a-c, e-q, s-z]', '', row.names(a)[m]) #strip out suffix symbols except 'd' and 'r' for 'Cd' and 'Cr'
    }
    j <- as.numeric()
    for (k in 1:nrow(a)) {
      if (('A' %in% strsplit(rownames(a), split='')[[k]]) | ('B' %in% strsplit(rownames(a), split='')[[k]]) | ('C' %in% strsplit(rownames(a), split='')[[k]])) {
        j <- c(j, k)
      }
    }
    if (length(j) == 1) {
      a <- as.matrix(a[j, ])
      if (rownames(a) == 'Cr' | rownames(a) == 'Cd') {
        hzbottom_synthesis[i, 2] <- rownames(a)
        print(paste(names(hzbottom_by_compname[i]), 'has a Cr or Cd horizon & no listed restriction.'))
        next
      }
      hzbottom_synthesis[i, 2] <- strsplit(rownames(a), split='')[[1]][1]
      next
    }
    if (length(j) > 1) {
      b <- a
      for (z in 1:nrow(b)) {
        if (rownames(b)[z] == 'Cd' | rownames(b)[z] == 'Cr') {
          print(paste(names(hzbottom_by_compname[i]), 'has a Cr or Cd horizon & no listed restriction.'))
          next #keep 'Cd' and 'Cr' horizon names
        }
        rownames(b)[z] <- strsplit(rownames(b)[z], split = '')[[1]][1] #otherwise, simplify horizon name to first letter
      }
      hz_nombres <- unique(rownames(b))
      if (length(hz_nombres)==1) {
        hzbottom_synthesis[i, 2] <- hz_nombres
        next
      }
      a <- as.matrix(a[j, ])
      hz_most_common <- a[which.is.max(a),] #this function breaks ties at random compared to which.max which takes the first of ties
      if (hz_most_common == 'Cd' | hz_most_common == 'Cr') {
        hzbottom_synthesis[i, 2] <- names(hz_most_common) #keep 'Cd' and 'Cr' horizon names
        next
      }
      hzbottom_synthesis[i, 2] <- strsplit(names(hz_most_common), split='')[[1]][1] #otherwise, simplify horizon name to first letter of the most common horizon name
      next
    }
  }
  no_restr_comps <- merge(x=no_restr_comps, y=hzbottom_synthesis, by='compname', all=TRUE)
  ssurgo_comp <- ssurgo_comp[which(ssurgo_comp$reskind == 'Paralithic bedrock' | ssurgo_comp$reskind == 'Lithic bedrock' | ssurgo_comp$reskind == "Densic bedrock" | ssurgo_comp$reskind == "Densic material"), ] #extract rows with these types of restrictions
  #this assumes that the SSURGO horizon table listed the component restrictions as a horizon. can double check that no R horizons have an AWC value
  ssurgo_comp$hzname_deepest_simplified[ssurgo_comp$reskind=='Lithic bedrock'] <- 'R'
  ssurgo_comp$hzname_deepest_simplified[ssurgo_comp$reskind=='Paralithic bedrock'] <- 'Cr'
  ssurgo_comp$hzname_deepest_simplified[ssurgo_comp$reskind=='Densic bedrock' | ssurgo_comp$reskind=='Densic material'] <- 'Cd'
  ssurgo_comp <- rbind(ssurgo_comp, no_restr_comps, stringsAsFactors=FALSE) #combine all data back together
  
  #insert additional bottom name corrections here: (1) first, from cross watershed component level info (2) then, from summarized OSD database.  Both of these are read in from csv files produced from 'master_soil_comp_processing.R'  What if a component name has more than one possibility?
  #setwd(OSDdir)
  # cross_watershed_hznames <- read.csv('deephznames_by_compname.csv', stringsAsFactors = FALSE)
  # i <- which(is.na(ssurgo_comp$hzname_deepest_simplified))
  # for (j in 1:length(i)){
  #   ssurgo_comp$hzname_deepest_simplified[i[j]] <- cross_watershed_hznames$hzname_deepest_simplified[match(ssurgo_comp$compname[i[j]], cross_watershed_hznames$compname)]
  # }
# insert OSD horizon name and depth info
  setwd(OSDdir)
  osd_hznames <- read.csv('comps_osd_bottom_hz_edited.csv', stringsAsFactors = FALSE)
  i <- which(is.na(ssurgo_comp$hzname_deepest_simplified))
  ssurgo_comp$hzname_deepest_osd <- NA
  ssurgo_comp$hzdept_r_deepest_osd <- NA
  ssurgo_comp$hzdepb_r_deepest_osd <- NA
  new_restrictions <- vector()
  for (j in 1:length(i)){
    look_up_index <- match(tolower(ssurgo_comp$compname[i[j]]), tolower(osd_hznames$compname))
    if (is.na(look_up_index)) {
      next
    }
    if(is.na(osd_hznames$hzname_simplified[look_up_index])) {
      next
    }
    if (osd_hznames$hzname_simplified[look_up_index]=='A'| osd_hznames$hzname_simplified[look_up_index]=='B'| osd_hznames$hzname_simplified[look_up_index]=='C'| osd_hznames$hzname_simplified[look_up_index]=='O') { # if the simplified horizon name is A, B, C, or O,' then do the following--
      ssurgo_comp$hzname_deepest_simplified[i[j]] <- osd_hznames$hzname_simplified[look_up_index]
      ssurgo_comp$hzdept_r_deepest_osd[i[j]] <- osd_hznames$hzdept_r[look_up_index]
      ssurgo_comp$hzdepb_r_deepest_osd[i[j]] <- osd_hznames$hzdepb_r[look_up_index]
      ssurgo_comp$hzname_deepest_osd[i[j]] <- osd_hznames$hzname[look_up_index]
      next
    }
    if (osd_hznames$hzname_simplified[look_up_index]=='R' | osd_hznames$hzname_simplified[look_up_index]=='Cr' | osd_hznames$hzname_simplified[look_up_index]=='Cd') { # if the simplified horizon name is R, Cr, or Cd and if the reskind is NA or not 'None,' then do the following--
      ssurgo_comp$hzname_deepest_simplified[i[j]] <- osd_hznames$hzname_simplified[look_up_index]
      ssurgo_comp$hzdept_r_deepest_osd[i[j]] <- osd_hznames$hzdept_r[look_up_index]
      ssurgo_comp$hzdepb_r_deepest_osd[i[j]] <- osd_hznames$hzdepb_r[look_up_index]
      ssurgo_comp$hzname_deepest_osd[i[j]] <- osd_hznames$hzname[look_up_index]
      new_restrictions <- c(new_restrictions, i[j])
      next
    }
    print(paste('WARNING, cokey', ssurgo_comp$cokey[i[j]], 'from', watershed_name, 'did not fall into a horizon naming category.  See line 430.'))
    next
  }

# #correct depth to paralithic, lithic, or densic layer for those without ID'd component restrictions that are minor components, because I assume that major components would have had a listed restriction.  These is essentially to finish populating any deep horizon data from the cross-watershed and OSD fix above
  #first the fix for fills from cross-watershed data
# ssurgo_comp$resdept_r[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='Cr' & ssurgo_comp$majcompflag=='No')] <- ssurgo_comp$hzdept_r_deepest[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='Cr' & ssurgo_comp$majcompflag=='No')]
# ssurgo_comp$resdept_r[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='R' & ssurgo_comp$majcompflag=='No')] <- ssurgo_comp$hzdept_r_deepest[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='R' & ssurgo_comp$majcompflag=='No')]
#   ssurgo_comp$resdept_r[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='Cd' & ssurgo_comp$majcompflag=='No')] <- ssurgo_comp$hzdept_r_deepest[which(is.na(ssurgo_comp$reskind) & is.na(ssurgo_comp$hzname_deepest_osd) & ssurgo_comp$hzname_deepest_simplified=='Cd' & ssurgo_comp$majcompflag=='No')] 
  
#then fill in the component restriction information for those minor components missing it (only minor components would still have restriction == NA)
  altered_reskinds <- ssurgo_comp[new_restrictions, ]
  txt15 <- paste('There are', length(unique(ssurgo_comp$compname[which(is.na(ssurgo_comp$hzname_deepest_simplified))])), 'unique component names without a designated deep horizon with no identified lithic, paralithic, or densic contact.', '\n', 'See _comps_hzname_deepest_isNA.csv for specific component names.  This affects a total of', round(sum(ssurgo_comp$hectares[which(is.na(ssurgo_comp$hzname_deepest_simplified))]), digits = 0), 'hectares.')
  txt16 <- paste('These are the unique', paste(unique(ssurgo_comp$hzname_deepest_simplified), collapse=', '), 'simplified horizon names after processing.')
  comps_hzname_deepest_isNA <- as.data.frame(unique(ssurgo_comp$compname[which(is.na(ssurgo_comp$hzname_deepest_simplified))]), stringsAsFactors=FALSE)
  colnames(comps_hzname_deepest_isNA) <- 'soil component name'
  setwd(outpath)
  write.csv(comps_hzname_deepest_isNA, paste(watershed_name, '_comps_hzname_deepest_isNA.csv', sep = ''), row.names=FALSE)
  write.csv(altered_reskinds, paste(watershed_name, '_comps_hzname_isR_Cr_Cd.csv', sep = ''), row.names = FALSE)
  #this is slightly more conservative than what I do in Statsgo (see above csv file for details)
  ssurgo_comp$reskind[which(is.na(ssurgo_comp$reskind) & ssurgo_comp$hzname_deepest_simplified=='Cr')] <- 'Paralithic bedrock'
  ssurgo_comp$reskind[which(is.na(ssurgo_comp$reskind) & ssurgo_comp$hzname_deepest_simplified=='R')] <- 'Lithic bedrock'
  ssurgo_comp$reskind[which(is.na(ssurgo_comp$reskind) & ssurgo_comp$hzname_deepest_simplified=='Cd')] <- 'Densic material'

#area weighted function
  area_weighted_average <- function(df, varname) {
    has_data <- df[which(!is.na(df[[varname]])), ]
    data_area <- as.data.frame(tapply(has_data$hectares, list(has_data$compname, has_data$reskind), sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$hectares, list(has_data$compname, has_data$reskind), sum))
    data_by_compname <- data_calc_term/data_area
    #replace NA AWC values with component name average AWC values
    i <- which(is.na(df[[varname]]))
    for (j in 1:length(i)){
      look_up_index <- match(df$compname[i[j]], rownames(data_by_compname))
      if (is.na(look_up_index)) {
        next
      }
      compname_reskind <- df$reskind[i[j]] #find name of the compname_reskind
      if (is.na(compname_reskind)) {
        next(print(paste(rownames(data_by_compname)[look_up_index], 'is missing reskind data to determine which', varname, 'to paste.')))
      }
      df[[varname]][i[j]] <- data_by_compname[[compname_reskind]][look_up_index]
    }
    invisible(df)
  }
#correct depth to top and bottom of deepest horizon, for comparison to depth of top of restrictive horizon
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'hzdept_r_deepest')
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'hzdepb_r_deepest')

#correct bottom horizon awc data; had to run a modified version of the area weighted function
  ssurgo_comp$awc_r_deepest[ssurgo_comp$hzname_deepest_simplified=='R' & is.na(ssurgo_comp$awc_r_deepest)] <- 0 #in case a horizon is mistakely labeled R with awc value; this can be caught later for editing of the underlying database
  ssurgo_comp$awc_r_deepest[which(ssurgo_comp$compname=='Rock outcrop' | ssurgo_comp$compname=='ROCK OUTCROP')] <- 0 #may need to add other components here
  has_data <- ssurgo_comp[which(!is.na(ssurgo_comp$awc_r_deepest)), ]
  data_area <- as.data.frame(tapply(has_data$hectares, list(has_data$compname, has_data$reskind), sum))
  data_calc_term <- as.data.frame(tapply(has_data$awc_r_deepest*has_data$hectares, list(has_data$compname, has_data$reskind), sum))
  data_by_compname <- data_calc_term/data_area
  i <- which(is.na(ssurgo_comp$awc_r_deepest) & (is.na(ssurgo_comp$hzname_deepest_simplified) | ssurgo_comp$hzname_deepest_simplified=='A' | ssurgo_comp$hzname_deepest_simplified=='B' | ssurgo_comp$hzname_deepest_simplified=='C')) #this is the modified portion of the 'area weighted average' function.  Replace 'deepest horizon' NA values with component name average values, as long they're not Cr, Cd, or R, though I could consider populating Cr deep horizon data if a component name has that data
  for (j in 1:length(i)){
    look_up_index <- match(ssurgo_comp$compname[i[j]], rownames(data_by_compname))
    if (is.na(look_up_index)) {
      next
    }
    compname_reskind <- ssurgo_comp$reskind[i[j]] #find name of the compname_reskind
    if (is.na(compname_reskind)) {
      print(paste(rownames(data_by_compname)[look_up_index], 'is missing reskind data to determine which deephor AWC value to paste.'))
      next
    }
    ssurgo_comp$awc_r_deepest[i[j]] <- data_by_compname[[compname_reskind]][look_up_index]
  }
#read in all_watersheds awc summary by component name to fill in remaining NAs
  # setwd(OSDdir)
  # cross_watershed_awc <- read.csv('all_watersheds_awc_by_compname.csv', stringsAsFactors = FALSE)
  # i <- which(is.na(ssurgo_comp$awc_r_deepest) & (is.na(ssurgo_comp$hzname_deepest_simplified) | ssurgo_comp$hzname_deepest_simplified != 'R' | ssurgo_comp$hzname_deepest_simplified!= 'Cr' | ssurgo_comp$hzname_deepest_simplified!= 'Cd')) # because a NA for hz_name_simplied does not mean it is not an R horizon by 'R' programming logic
  # for (j in 1:length(i)){
  #   ssurgo_comp$awc_r_deepest[i[j]] <- cross_watershed_awc$mean_awc_r_deepest[match(ssurgo_comp$compname[i[j]], cross_watershed_awc$compname)]
  # }
  ssurgo_comp$awc_deepest_dummy <- ssurgo_comp$awc_r_deepest #copy awc data 
  ssurgo_comp$awc_deepest_dummy[ssurgo_comp$awc_deepest_dummy >= 0] <- 1 #this creates a dummy variable to do matrix multiplication, such that NAs in the awc_deepest_dummy column will mean that the component % will not be added to the total component %
  awc_deephor_pct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$awc_deepest_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is awc data
  colnames(awc_deephor_pct_tot) <- 'awc_deephor_pct_tot'
  awc_deephor_pct_tot$mukey <- rownames(awc_deephor_pct_tot) #this will get merged back to mu summary for calc purposes

#awc work
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'awc_H2Ocm') #see area weighted average function for details
  ssurgo_comp$awc_H2Ocm[which(ssurgo_comp$compname=='Rock outcrop' | ssurgo_comp$compname=='ROCK OUTCROP')] <- 0
  ssurgo_comp$awc_H2Ocm[which(is.na(ssurgo_comp$awc_H2Ocm) & ssurgo_comp$reskind=='Lithic bedrock' & ssurgo_comp$resdept_r==0)] <- 0 #some lava flows, etc. have this information but 
#now fill in remaining NAs with all_watershed synthesis data, now written as function
  all_watershed_synthesis <- function(df, varname, fname) {
    setwd(OSDdir)
    data_by_compname <- read.csv(fname, stringsAsFactors = FALSE)
    colnames(data_by_compname) <- c('Abrupt textural change', 'Cemented horizon', 'Densic bedrock', 'Densic material', 'Duripan', 'Lithic bedrock', 'None', 'Paralithic bedrock', 'Strongly contrasting textural stratification', 'compname')
    i <- which(is.na(ssurgo_comp[[varname]]))
    for (j in 1:length(i)){
      look_up_index <- match(df$compname[i[j]], data_by_compname$compname)
      if (is.na(look_up_index)) {
        next
      }
      compname_reskind <- df$reskind[i[j]] #find name of the compname_reskind
      if (is.na(compname_reskind)) {
        print(paste(data_by_compname$compname[look_up_index], 'is missing reskind data to determine which', varname, 'to choose.'))
        next
      }
      df[[varname]][i[j]] <- data_by_compname[[compname_reskind]][look_up_index]
    }
    invisible(df)
  }
  ssurgo_comp <- all_watershed_synthesis(ssurgo_comp, 'awc_H2Ocm', 'all_watersheds_awc_by_compname.csv') #this csv file was produced outside of this script by 'master_soil_comp_processing.R'
  ssurgo_comp$awc_dummy <- ssurgo_comp$awc_H2Ocm #copy awc data 
  ssurgo_comp$awc_dummy[ssurgo_comp$awc_dummy >= 0] <- 1 #this creates a dummy variable to do matrix multiplication, such that NAs in the awc_qc column will mean that the component % will not be added to the total component %
  awc_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$awc_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is awc data
  colnames(awc_ppct_tot) <- 'awc_ppct_tot'
  awc_ppct_tot$mukey <- rownames(awc_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes
  txt13 <- paste('There are ', sum(is.na(ssurgo_comp$awc_H2Ocm)), ' cokeys with no AWC data after modification of the database.  See ..._compnames_AWS_isNA.csv for specific component names.', '\n', length(unique(ssurgo_comp$compname[is.na(ssurgo_comp$awc_H2Ocm)])), 'of these represent unique component names, affecting', length(unique(ssurgo_comp$mukey[is.na(ssurgo_comp$awc_H2Ocm)])), 'unique map units.  It is a total of', round(sum(ssurgo_comp$hectares[which(is.na(ssurgo_comp$awc_H2Ocm))]), digits = 0), 'hectares.')
  comps_awc_is_na <- as.data.frame(unique(ssurgo_comp$compname[is.na(ssurgo_comp$awc_H2Ocm)]))
  names(comps_awc_is_na) <- 'soil component name'
  setwd(outpath)
  write.csv(comps_awc_is_na, paste(watershed_name, '_compnames_AWS_isNA.csv', sep = ''), row.names = FALSE)

#awc thickness correction work
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'awc_soilthickness_cm') #see area weighted average function for details
  ssurgo_comp$awc_soilthickness_cm[ssurgo_comp$compname=='Rock outcrop'] <- 0
  ssurgo_comp$awc_soilthickness_cm[which(is.na(ssurgo_comp$awc_soilthickness_cm) & ssurgo_comp$reskind=='Lithic bedrock' & ssurgo_comp$resdept_r==0)] <- 0
#now fill-in remaning NAs with all_watershed synthesis created in 'master_soil_comp_processing.R'
  ssurgo_comp <- all_watershed_synthesis(ssurgo_comp, 'awc_soilthickness_cm', 'all_watersheds_awc_thickness_by_compname.csv') #this csv file was produced outside of this script by 'master_soil_comp_processing.R'
  ssurgo_comp$awc_soilthickness_dummy <- ssurgo_comp$awc_soilthickness_cm #copy awc data 
  ssurgo_comp$awc_soilthickness_dummy[ssurgo_comp$awc_soilthickness_dummy >= 0] <- 1 #this creates a dummy variable to do matrix multiplication, such that NAs in the awc_qc column will mean that the component % will not be added to the total component %
  awc_soilthickness_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$awc_soilthickness_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is awc data
  colnames(awc_soilthickness_ppct_tot) <- 'awc_soilthickness_ppct_tot' #this should be equivalent to awc_ppct_totot
  awc_soilthickness_ppct_tot$mukey <- rownames(awc_soilthickness_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes

#OM variable work
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'om_kgm2') #see area weighted average function for details
  ssurgo_comp$om_kgm2[which(ssurgo_comp$compname=='Rock outcrop' | ssurgo_comp$compname=='ROCK OUTCROP')] <- 0
  ssurgo_comp$om_kgm2[which(is.na(ssurgo_comp$om_kgm2) & ssurgo_comp$reskind=='Lithic bedrock' & ssurgo_comp$resdept_r==0)] <- 0
  ssurgo_comp$om_dummy <- ssurgo_comp$om_kgm2 #copy depth to restrictive layer
  ssurgo_comp$om_dummy[ssurgo_comp$om_dummy >= 0] <- 1 #this creates a dummy variable to do matrix multiplication
  om_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$om_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is a component restriction
  colnames(om_ppct_tot) <- 'om_ppct_tot'
  om_ppct_tot$mukey <- rownames(om_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes

#clay variable work
  ssurgo_comp <- area_weighted_average(ssurgo_comp, 'clay_kgm2') #see area weighted average function for details
  ssurgo_comp$clay_kgm2[which(ssurgo_comp$compname=='Rock outcrop' | ssurgo_comp$compname=='ROCK OUTCROP')] <- 0
  ssurgo_comp$clay_kgm2[which(is.na(ssurgo_comp$clay_kgm2) & ssurgo_comp$reskind=='Lithic bedrock' & ssurgo_comp$resdept_r==0)] <- 0
  ssurgo_comp$clay_dummy <- ssurgo_comp$clay_kgm2 #copy clay content to new column
  ssurgo_comp$clay_dummy[ssurgo_comp$clay_dummy >= 0] <- 1 #this creates a dummy variable out of this new column to do matrix multiplication
  clay_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$clay_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is clay content data for a particular component
  colnames(clay_ppct_tot) <- 'clay_ppct_tot'
  clay_ppct_tot$mukey <- rownames(clay_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes

#percent rock outcrop, still need to account for major soil components with no restriction data but with simplified deep horizon as 'R' from database correction work; perhaps if deep horizon AWC == 0, then assume it's R
  ssurgo_comp$rock_dummy <- NA #create dummy variable
  ssurgo_comp$rock_dummy[which(ssurgo_comp$compname=='Rock outcrop' | ssurgo_comp$compname=='ROCK OUTCROP')] <- 1 #this creates a dummy variable to do matrix multiplication during map unit aggregation work
  ssurgo_comp$lithic_dummy <- NA
  a <- which(ssurgo_comp$reskind == 'Lithic bedrock' | (ssurgo_comp$hzname_deepest_simplified=='R' & ssurgo_comp$awc_r_deepest==0)) #latter catch may longer be necessary; should have been called as 'Lithic bedrock'
  ssurgo_comp$lithic_dummy[a] <- 1
  ssurgo_comp$paralithic_dummy <- NA
  b <- which(ssurgo_comp$reskind == 'Paralithic bedrock' | ssurgo_comp$reskind == 'Densic bedrock' | ssurgo_comp$reskind == 'Densic material')
  ssurgo_comp$paralithic_dummy[b] <- 1
  ssurgo_comp$Ahorizon_dummy <- NA
  c <- which(ssurgo_comp$hzname_deepest_simplified=='A')
  ssurgo_comp$Ahorizon_dummy[c] <- 1 #this is because all Components with no restrictions had their deepest horizon simplified to 'A horizon' if it was some kind of A horizon
  ssurgo_comp$Bhorizon_dummy <- NA
  d <- which(ssurgo_comp$hzname_deepest_simplified=='B')
  ssurgo_comp$Bhorizon_dummy[d] <- 1 #this is because all Components with no restrictions had their deepest horizon simplified to 'B horizon' if it was some kind of dominant B horizon
  ssurgo_comp$Chorizon_dummy <- NA
  e <- which((ssurgo_comp$hzname_deepest_simplified=='C' | ssurgo_comp$hzname_deepest_simplified=='Cr' | ssurgo_comp$hzname_deepest_simplified=='Cd')  & (is.na(ssurgo_comp$reskind) | ssurgo_comp$reskind != 'Paralithic bedrock' & ssurgo_comp$reskind != 'Densic bedrock' & ssurgo_comp$reskind != 'Densic material')) #this is because some general taxonomic major components did not have a restrictive horizon but the only available data found a deep horizon name of e.g. Cr when in fact that component name does not always have a Cr
  ssurgo_comp$Chorizon_dummy[e] <- 1
  ssurgo_comp$NAhorizon_dummy <- NA
  f <- which(is.na(ssurgo_comp$hzname_deepest_simplified))
  ssurgo_comp$NAhorizon_dummy[f] <- 1
  txt9 <- paste(sum(ssurgo_comp$lithic_dummy, ssurgo_comp$paralithic_dummy, ssurgo_comp$Chorizon_dummy, ssurgo_comp$NAhorizon_dummy, ssurgo_comp$Ahorizon_dummy, ssurgo_comp$Bhorizon_dummy, na.rm = TRUE), ' should equal ', nrow(ssurgo_comp), '.  If not, see code lines 359-372 in regards to deepest horizon names by component.', sep = '')

#component percentage totals for various calcs by contact type or lack thereof
  lithic_ppct_tot<- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$lithic_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is clay content data for a particular component
  colnames(lithic_ppct_tot) <- 'lithic_ppct_tot'
  lithic_ppct_tot$mukey <- rownames(lithic_ppct_tot) #this will get merged back to ssurgo_comp for calc purposes
  paralithic_ppct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$paralithic_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE))
  colnames(paralithic_ppct_tot) <- 'paralithic_ppct_tot'
  paralithic_ppct_tot$mukey <- rownames(paralithic_ppct_tot)
  Chorizon_ppct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$Chorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE))
  colnames(Chorizon_ppct_tot) <- 'Chorizon_ppct_tot'
  Chorizon_ppct_tot$mukey <- rownames(Chorizon_ppct_tot)
  NAhorizon_ppct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$NAhorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE))
  colnames(NAhorizon_ppct_tot) <- 'NAhorizon_ppct_tot'
  NAhorizon_ppct_tot$mukey <- rownames(NAhorizon_ppct_tot)
  Ahorizon_ppct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$Ahorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE))
  colnames(Ahorizon_ppct_tot) <- 'Ahorizon_ppct_tot'
  Ahorizon_ppct_tot$mukey <- rownames(Ahorizon_ppct_tot)
  Bhorizon_ppct_tot <- as.data.frame(tapply(ssurgo_comp$comppct_r*ssurgo_comp$Bhorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE))
  colnames(Bhorizon_ppct_tot) <- 'Bhorizon_ppct_tot'
  Bhorizon_ppct_tot$mukey <- rownames(Bhorizon_ppct_tot)

#qc check on soil thickness for AWC calculations (currently ignores soils with duripans or abrupt textural changes)
  ssurgo_comp$AWC_soil_thickness_qc <- NA
  a <- which(ssurgo_comp$reskind=='Lithic bedrock' | ssurgo_comp$reskind=='Paralithic bedrock')
  ssurgo_comp$AWC_soil_thickness_qc[a] <- ssurgo_comp$resdept_r[a] - ssurgo_comp$awc_soilthickness_cm[a] #this calculates the difference between the depth to the restrictive layer and the soil thickness used in the awc calculation where any horizons below the depth to the restriction were not used, even if they had a reported 0 for awc_r
  b <- which(is.na(ssurgo_comp$reskind))
  ssurgo_comp$AWC_soil_thickness_qc[b] <- ssurgo_comp$awc_soilthickness_cm[b] - ssurgo_comp$hzdepb_r_deepest[b]
  qc_value <- 20 #(depth discrepancy in cm)
  qc_results <- ssurgo_comp[which(abs(ssurgo_comp$AWC_soil_thickness_qc) > qc_value),]
  txt10 <- paste('There are', length(unique(qc_results$compname)), 'unique components out of', length(unique(ssurgo_comp$compname)), 'total unique components with a', qc_value, 'cm discrepancy between either: (1) in the case of soils with a restriction,', '\n', 'the depth to a restriction and the soil thickness used in the AWC calculation or, (2) in the case of soils with no restriction, the total profile', '\n', 'thickness and the depth used in the AWC calculation.  These discrepancies have multiple error sources.')
  ssurgo_comp$AWC_qc_flag <- NA
  c <- which(abs(ssurgo_comp$AWC_soil_thickness_qc) > qc_value)
  ssurgo_comp$AWC_qc_flag[c] <- 1 #could be replaced by some kind of index to indicate severity of discrepancy
  setwd(outpath)
  write.csv(qc_results, paste(watershed_name, '_components_with_soil_thickness_discrepancies.csv', sep = ''), row.names = FALSE)

#join ssurgo_comp data.frame with newly calculated 
  ssurgo_comp <- join_all(list(ssurgo_comp, restr_ppct_tot, awc_ppct_tot, om_ppct_tot, clay_ppct_tot, lithic_ppct_tot, paralithic_ppct_tot, Ahorizon_ppct_tot, Bhorizon_ppct_tot, Chorizon_ppct_tot, NAhorizon_ppct_tot, awc_soilthickness_ppct_tot, awc_deephor_pct_tot), by='mukey') #merge these component % totals with the original soil comp dataframe
  
##rewrite this section with function so that dummy controls whether or not division is performed
  ssurgo_comp$restr_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$restr_dummy*ssurgo_comp$restr_ppct_tot)
  ssurgo_comp$awc_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$awc_dummy*ssurgo_comp$awc_ppct_tot)
  ssurgo_comp$awc_deephor_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$awc_deepest_dummy*ssurgo_comp$awc_deephor_pct_tot)
  ssurgo_comp$awc_soilthickness_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$awc_soilthickness_dummy*ssurgo_comp$awc_soilthickness_ppct_tot) #should be equivalent to awc_wta_coeff
  ssurgo_comp$om_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$om_dummy*ssurgo_comp$om_ppct_tot)
  ssurgo_comp$clay_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$clay_dummy*ssurgo_comp$clay_ppct_tot)
  #ssurgo_comp$frags_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$frags_dummy*ssurgo_comp$frags_ppct_tot)
  ssurgo_comp$lithic_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$lithic_dummy*ssurgo_comp$lithic_ppct_tot)
  ssurgo_comp$paralithic_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$paralithic_dummy*ssurgo_comp$paralithic_ppct_tot)
  ssurgo_comp$Ahorizon_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$Ahorizon_dummy*ssurgo_comp$Ahorizon_ppct_tot)
  ssurgo_comp$Bhorizon_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$Bhorizon_dummy*ssurgo_comp$Bhorizon_ppct_tot)
  ssurgo_comp$Chorizon_wta_coeff <- ssurgo_comp$comppct_r/(ssurgo_comp$Chorizon_dummy*ssurgo_comp$Chorizon_ppct_tot)
  ssurgo_comp$NAhorizon_wta_coeff <-ssurgo_comp$comppct_r/(ssurgo_comp$NAhorizon_dummy*ssurgo_comp$NAhorizon_ppct_tot)
  setwd(outpath)
  write.csv(ssurgo_comp, paste(watershed_name, '_final_comp_aggregation.csv', sep=''), row.names = FALSE)
  
#get all component names and percentages by map unit
  mukeys <- unique(ssurgo_comp$mukey)
  comps_by_mukey <- list()
  compnames_pct <- ssurgo_comp[ , c('mukey', 'compname', 'comppct_r')]
  for (j in 1:length(mukeys)) {
    comps_by_mukey[[j]] <- compnames_pct[compnames_pct$mukey==mukeys[j],]
  }
  names(comps_by_mukey) <- mukeys
  compname_summary <- as.data.frame(matrix(ncol=2, nrow=length(mukeys)))
  colnames(compname_summary) <- c('mukey', 'component_summary')
  for (j in 1:length(comps_by_mukey)) {
    a <- comps_by_mukey[[j]] #test inner loop
    a <- a[order(-a$comppct_r),]
    b <- vector()
    for (i in 1:nrow(a)){
      c <- paste(a[i, 2], '(', a[i, 3], '%)', sep = '')
      if (i==1) {
        b <- c
        next
      }
      b <- paste(b, ', ', c, sep = '')
    }
    compname_summary[j, 1] <- a[1, 1]
    compname_summary[j, 2] <- b
  }
#calculate mu aggregated data for all components in a map unit and also by restrictive feature for awc
  mu_summary <- as.data.frame(tapply(ssurgo_comp$restr_wta_coeff*ssurgo_comp$resdept_r, ssurgo_comp$mukey, sum, na.rm=TRUE)) #calculate the wtd avg depth to a restrictive layer (needs to be checked); does not factor soils that don't have a depth to a restrictive layer (could do solum depth instead, if I clean up the NA's and H's for soils without contacts)
  colnames(mu_summary) <- 'res_dep_wtdavg'
  mu_summary$mukey <- rownames(mu_summary)
  mu_summary$awc_H2Ocm_wtdavg <- tapply(ssurgo_comp$awc_wta_coeff*ssurgo_comp$awc_H2Ocm, ssurgo_comp$mukey, sum_modified)
  mu_summary$awc_soilthickness_wtdavg <- tapply(ssurgo_comp$awc_soilthickness_wta_coeff*ssurgo_comp$awc_soilthickness_cm, ssurgo_comp$mukey, sum_modified)
  mu_summary$awc_deephor_wtdavg <- tapply(ssurgo_comp$awc_r_deepest*ssurgo_comp$awc_deephor_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$om_kgm2_wtdavg <- tapply(ssurgo_comp$om_wta_coeff*ssurgo_comp$om_kgm2, ssurgo_comp$mukey, sum_modified)
  mu_summary$clay_kgm2_wtdavg <- tapply(ssurgo_comp$clay_wta_coeff*ssurgo_comp$clay_kgm2, ssurgo_comp$mukey, sum_modified)
  mu_summary$rockoutcrop_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$rock_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$lithic_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$lithic_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$paralithic_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$paralithic_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$Ahorizon_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$Ahorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$Bhorizon_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$Bhorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$Chorizon_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$Chorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$NAhorizon_pct <- tapply(ssurgo_comp$comppct_r*ssurgo_comp$NAhorizon_dummy, ssurgo_comp$mukey, sum, na.rm=TRUE)
  mu_summary$lithic_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$lithic_dummy*ssurgo_comp$lithic_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$paralithic_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$paralithic_dummy*ssurgo_comp$paralithic_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$Ahorizon_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$Ahorizon_dummy*ssurgo_comp$Ahorizon_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$Bhorizon_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$Bhorizon_dummy*ssurgo_comp$Bhorizon_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$Chorizon_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$Chorizon_dummy*ssurgo_comp$Chorizon_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$NAhorizon_AWC_soildepth <- tapply(ssurgo_comp$awc_soilthickness_cm*ssurgo_comp$NAhorizon_dummy*ssurgo_comp$NAhorizon_wta_coeff, ssurgo_comp$mukey, sum_modified)
  mu_summary$QCflag_awc_calc_ppct_comps <- tapply(ssurgo_comp$awc_wta_coeff*ssurgo_comp$AWC_qc_flag*100, ssurgo_comp$mukey, sum, na.rm=TRUE) #this determines the percentage of a mapunit's components that have possible QC issues with the AWC calculation
  rownames(mu_summary) <- NULL
  mu_summary <- join_all(list(mu_summary, awc_ppct_tot, om_ppct_tot, clay_ppct_tot, awc_soilthickness_ppct_tot, awc_deephor_pct_tot, compname_summary), by='mukey')
  mu_summary2 <- as.data.frame(ssurgo_comp %>% group_by(mukey) %>% mutate(the_rank = rank(-comppct_r, ties.method='random')) %>% filter(the_rank==1) %>% select(-the_rank)) #this is how to select the dominant component in each map unit with ties broken randomly
  mu_summary2 <- mu_summary2[ , c("mukey", "cokey", "compname", "comppct_r", "taxorder", "taxgrtgroup", "reskind", "resdept_r", "thickness_cm", "hzname_deepest_simplified", "awc_H2Ocm", "awc_r_deepest")] #reduce the dims to these variables
  colnames(mu_summary2)[2:12] <- c('cokey_dom', 'compname_dom', 'comp_ppct_dom', 'taxorder_dom', 'taxgrtgroup_dom', 'reskind_dom', 'resdept_dom', 'thickness_cm_dom', 'hzname_deepest_simplified_dom', 'awc_H2Ocm_dom', 'awc_r_deepest_dom')  #rename the columns to show that these are variables with respect to the dominant component in each map unit
  mu_summary <- join_all(list(mu_summary, mu_summary2, ssurgo_mu), by='mukey')
  #mu_summary$awc_H2Ocm_wtdavg[mu_summary$awc_ppct_tot==0] <- NA
  #mu_summary$clay_kgm2_wtdavg[mu_summary$clay_ppct_tot==0] <- NA
  #mu_summary$om_kgm2_wtdavg[mu_summary$om_ppct_tot==0] <- NA
  txt14 <- paste('There are', sum(is.na(mu_summary$awc_H2Ocm_wtdavg)), "map units with NA for AWS from this script's aggregation work and", sum(is.na(mu_summary$aws0150wta)), 'map units with NA in the SSURGO pre-aggregated 0-150 cm data.')
  setwd(outpath)
  write.csv(mu_summary, paste(watershed_name, '_mu_summary.csv', sep = ''), row.names=FALSE)
  lm.aws.comparison <- lm(aws0150wta ~ awc_H2Ocm_wtdavg, data=mu_summary)
  jpeg(file = paste(watershed_name, '_aws_comparison.jpeg', sep = ''))
  par(mar= c(4.5, 5, 4, 1.5) + 0.1)
  plot(mu_summary$awc_H2Ocm_wtdavg, mu_summary$aws0150wta, type='p', main=paste('Available water storage (AWS) comparison for', watershed_name), xlab='modified SSURGO AWS est. (cm H2O)', ylab="SSURGO est. 0-150 cm AWS (cm H2O)", font.main=1)
  abline(0, 1, col=2)
  dev.off()
  txt11 <- capture.output(summary(lm.aws.comparison))
  fileConn <- file(paste(watershed_name, "_analysis_metadata.txt"))
  writeLines(c(txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt12, txt13, txt15, txt16, txt8, txt9, txt10, txt11, txt14, txt17, txt18), fileConn, sep = '\n')
  close(fileConn)
}
#loop through all the SSURGO data
for (i in 3:22) { # length(ssurgo_dirs); don't run this on directory 2; separate script required for statsgo2 data
  catch <- try(aggregate_SSURGO(i, 'yes'))
  if (class(catch) == 'try-error') {
    aggregate_SSURGO(i, 'no')
    next
  }
}


#additional test code can be found in SSURGO_analysis.R (i.e. version 1)