#analysis of station temperature data from major California water supply watersheds
#input data are organized by watershed.  outputs organized by variable and timeperiod and include both csv files and shapefiles summarizing the results by station for different time periods
#intensive QC checks included
#metadata still needs to be updated via detailed notes from notebook
#next step is to re-run analysis with data from all of California and include precipitation and wind and humidity where available
#needs to be user defined
mainDir <- 'C:\\Users\\smdevine\\Desktop\\Forest dieoff and hydrology\\long.term.temperatures' 
results <- 'results_by_watershed_max_min'
input_data <- 'input_data_max_min/round3' #input_data_max_min has data from all watersheds but is organized in various directories as additional watersheds were added to analysis.
if (file.exists(file.path(mainDir, results)) == FALSE) {
  dir.create(file.path(mainDir, results))
}
inpath <- file.path(mainDir, input_data)
outpath <- file.path(mainDir, results)

# read in historical climo data (daily)
setwd(inpath)
filenames_temps <- list.files()
temps_dfs <- lapply(filenames_temps, read.csv)
names(temps_dfs) <- filenames_temps

#create list of dataframes for the watershed dataset.  Each item in the list represents one station's data and these will be saved to individual csv files in new results file
data_by_station <- function(watershed, file_index) {
  if (file.exists(file.path(outpath, watershed)) == FALSE) { #WATERSHED NAME NEEDS TO BE MODIFIED
    dir.create(file.path(outpath, watershed)) #WATERSHED NAME NEEDS TO BE MODIFIED
  }
  save_path <- file.path(outpath, watershed) #WATERSHED NAME NEEDS TO BE MODIFIED
  setwd(save_path)
  station_list <- as.vector(unique(temps_dfs[[file_index]]$STATION)) #WATERSHED DATAFRAME NEEDS TO BE MANUALLY ID'd
  for (i in 1:length(station_list)) {
    indices <- which(temps_dfs[[file_index]]$STATION == station_list[i]) #AND HERE
    temps_subset <- temps_dfs[[file_index]][indices,] #AND HERE
    write.csv(temps_subset, paste(as.character(temps_subset$STATION_NAME[1]), '_data.csv', sep = ""), row.names=FALSE)
  }
}
#run function to split each watershed's dataset into individual station files (THIS DOES NOT NEED TO BE RERUN FOR THE PHASE 3 ANALYSIS)
data_by_station('Mill_BigChico', 4)

# read in climo data now separated by station for each watershed to summarize data into 12 monthly periods.
summarize_by_month <- function(watershed_name) {
  save_path <- file.path('C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/results_by_watershed_max_min_phase3/', watershed_name)
  setwd(save_path)
  filenames_temps <- list.files()
  watershed_dfs <- lapply(filenames_temps, read.csv, na.strings=c(""," ","unknown")) #read in files and add NAs where blanks exist
  names(watershed_dfs) <- filenames_temps
#describe a function for counting NAs within each month
  count_NAs <- function(x) {
    sum(is.na(x))
  }
#run a loop to iterate through each station's data
  stn_location_qc <- data.frame(matrix(ncol=20))
  colnames(stn_location_qc)[1:20] <- c('STATION', 'STATION_NAME', 'ELEVATION', 'LATITUDE', 'LONGITUDE', 'START_YR', 'END_YR', 'TMAX_COVERAGE', 'TMAX_QC_FLAGS', 'TMAX_M_FLAGS', 'TMIN_COVERAGE', 'TMIN_QC_FLAGS', 'TMIN_M_FLAGS', 'AVG_OBS', 'ELEV_CHANGES', 'LAT_CHANGES', 'LON_CHANGES', 'ELEV_RANGE', 'LAT_RANGE', 'LON_RANGE')
  i = 0
  for (j in 1:length(watershed_dfs)) {
    df <- watershed_dfs[[j]]
    df$TMAX[df$TMAX>50] <- NA #this converts unrealistic high temps to NA
    df$TMAX[df$TMAX==-9999] <- NA #convert -9999 to NA
    df$TMIN[df$TMIN==-9999] <- NA
    df$Quality.Flag[is.na(df$TMAX)] <- NA #remove TMAX quality flags where temp is now NA
    df$Quality.Flag.1[is.na(df$TMIN)] <- NA #same as above but for TMIN quality flags
    qc_flags_tmax <- length(na.omit(df$Quality.Flag))
    qc_flags_tmin <- length(na.omit(df$Quality.Flag.1))
    m_flags_tmax <- length(na.omit(df$Measurement.Flag))
    m_flags_tmin <- length(na.omit(df$Measurement.Flag.1))
    df$TMAX[which(!is.na(df$Quality.Flag))] <- NA #convert TMAX records to NA where a quality flag exists
    df$TMIN[which(!is.na(df$Quality.Flag.1))] <- NA #same as above but for TMIN
    if (length(na.omit(df$TMAX)) < 365*5) { #this skips stations with less than 5 years of TMAX data;
      next
    }
    i <- i + 1 #this is for the purpose of the metadata data.frame so as to not populate empty rows since some datasets may be skipped
    stn_location_qc[i,1] <- as.character(df$STATION[1])
    stn_location_qc[i,2] <- as.character(df$STATION_NAME[1])
    stn_location_qc[i,3] <- mean(df$ELEVATION, na.rm = TRUE)
    stn_location_qc[i,4] <- mean(df$LATITUDE, na.rm = TRUE)
    stn_location_qc[i,5] <- mean(df$LONGITUDE, na.rm = TRUE)
    stn_location_qc[i, 15] <- paste(unique(df$ELEVATION), collapse = ' ')
    stn_location_qc[i, 16] <- paste(unique(df$LATITUDE), collapse = ' ')
    stn_location_qc[i, 17] <- paste(unique(df$LONGITUDE), collapse = ' ')
    stn_location_qc[i, 18] <- max(df$ELEVATION, na.rm = TRUE) - min(df$ELEVATION, na.rm = TRUE)
    stn_location_qc[i, 19] <- max(df$LATITUDE, na.rm = TRUE) - min(df$LATITUDE, na.rm = TRUE)
    stn_location_qc[i, 20] <- max(df$LONGITUDE, na.rm = TRUE) - min(df$LONGITUDE, na.rm = TRUE)
#convert the date column to a date object
    df$DATE <- as.Date(as.character(df$DATE), "%Y%m%d") 
#insert missing dates and add them as rows
    date_sequence <- as.data.frame(seq(from=df$DATE[1], to=tail(df$DATE, 1), by='day'))
    colnames(date_sequence) <- 'DATE'
    df <- merge(df, date_sequence, by='DATE', all = TRUE)
#create a new column for year
    df$year <- as.integer(format(df$DATE, "%Y"))
#create a new column for month
    df$month <- as.integer(format(df$DATE, "%m"))
    df$day <- as.integer(format(df$DATE, "%d"))
    start_yr <- df$year[1]
    stn_location_qc[i,6] <- start_yr
    end_yr <- tail(df$year, 1)
    stn_location_qc[i,7] <- end_yr 
    tmax_records <- length(na.omit(df$TMAX))
    tmin_records <- length(na.omit(df$TMIN))
    temp_records <- nrow(df)
    tmax_coverage <- round(tmax_records/temp_records, digits = 3)
    stn_location_qc[i,8] <- tmax_coverage
    stn_location_qc[i,9] <- qc_flags_tmax
    stn_location_qc[i,10] <- m_flags_tmax
    tmin_coverage <- round(tmin_records/temp_records, digits = 3)
    stn_location_qc[i, 11] <- tmin_coverage
    stn_location_qc[i, 12] <- qc_flags_tmin
    stn_location_qc[i, 13] <- m_flags_tmin
    stn_location_qc[i, 14] <- round(mean(tmax_records, tmin_records), digits = 0)
    #stn_location_qc[i, 21] <- i
    df$diurnal_temp_range <- df$TMAX - df$TMIN #calc the daily diurnal temp range
    df$TAVG <- (df$TMAX + df$TMIN)/2 #calc the daily avg temp as mean of tmax and tmin
#QC work for monthly averages
    QC_matrix_tmax <- as.data.frame(tapply(df$TMAX, list(df$year, df$month), count_NAs)) #sum up the NAs in the temp column for each month
    QC_matrix_tmax[QC_matrix_tmax > 5] <- NA #if there are >5 NAs for a month, tag the month with a NA
    QC_matrix_tmax[QC_matrix_tmax <= 5] <- 1 #if there are 5 or less, tag the month with a 1 for matrix multiplication purposes
    QC_matrix_tmin <- as.data.frame(tapply(df$TMIN, list(df$year, df$month), count_NAs)) #sum up the NAs in the temp column for each month
    QC_matrix_tmin[QC_matrix_tmin > 5] <- NA #if there are >5 NAs for a month, tag the month with a NA
    QC_matrix_tmin[QC_matrix_tmin <= 5] <- 1 #if there are 5 or less, tag the month with a 1 for matrix multiplication purposes
    QC_matrix_diurnal <- as.data.frame(tapply(df$diurnal_temp_range, list(df$year, df$month), count_NAs)) #sum up the NAs in the temp column for each month
    QC_matrix_diurnal[QC_matrix_diurnal > 5] <- NA #if there are >5 NAs for a month, tag the month with a NA
    QC_matrix_diurnal[QC_matrix_diurnal <= 5] <- 1 #if there are 5 or less, tag the month with a 1 for matrix multiplication purposes
    QC_matrix_tavg <- as.data.frame(tapply(df$TAVG, list(df$year, df$month), count_NAs)) #sum up the NAs in the temp column for each month
    QC_matrix_tavg[QC_matrix_tavg > 5] <- NA #if there are >5 NAs for a month, tag the month with a NA
    QC_matrix_tavg[QC_matrix_tavg <= 5] <- 1 #if there are 5 or less, tag the month with a 1 for matrix multiplication purposes
    QC_matrix_tmax_annual <- as.data.frame(tapply(df$TMAX, df$year, count_NAs)) #sum up the NAs in the temp column for each year
    QC_matrix_tmax_annual[QC_matrix_tmax_annual > 60] <- NA #if there are >30 NAs for a year, tag the year with a NA
    QC_matrix_tmax_annual[QC_matrix_tmax_annual <= 60] <- 1 #if there are 30 or less, tag the year with a 1 for matrix multiplication purposes
    QC_matrix_tmin_annual <- as.data.frame(tapply(df$TMIN, df$year, count_NAs)) #sum up the NAs in the temp column for each year
    QC_matrix_tmin_annual[QC_matrix_tmin_annual > 60] <- NA #if there are >30 NAs for a year, tag the year with a NA
    QC_matrix_tmin_annual[QC_matrix_tmin_annual <= 60] <- 1 #if there are 30 or less, tag the year with a 1 for matrix multiplication purposes
    QC_matrix_diurnal_annual <- as.data.frame(tapply(df$diurnal_temp_range, df$year, count_NAs)) #sum up the NAs in the temp column for each year
    QC_matrix_diurnal_annual[QC_matrix_diurnal_annual > 60] <- NA #if there are >30 NAs for a year, tag the year with a NA
    QC_matrix_diurnal_annual[QC_matrix_diurnal_annual <= 60] <- 1 #if there are 30 or less, tag the year with a 1 for matrix multiplication purposes
    QC_matrix_tavg_annual <- as.data.frame(tapply(df$TAVG, df$year, count_NAs)) #sum up the NAs in the temp column for each year
    QC_matrix_tavg_annual[QC_matrix_tavg_annual > 60] <- NA #if there are >30 NAs for a year, tag the year with a NA
    QC_matrix_tavg_annual[QC_matrix_tavg_annual <= 60] <- 1 #if there are 30 or less, tag the year with a 1 for matrix multiplication purposes
#calculate monthly, summer, and annual means for each variable
    mean_tmax <- as.data.frame(tapply(df$TMAX, list(df$year, df$month), mean, na.rm=TRUE)) #calculate the avg tmax for each month regardless of NA observations
    mean_tmax <- QC_matrix_tmax*mean_tmax #then convert months with >= 5 missing days to NA
    colnames(mean_tmax)[1:12] <- c('jan_avgTMAX', 'feb_avgTMAX', 'mar_avgTMAX', 'apr_avgTMAX', 'may_avgTMAX', 'jun_avgTMAX', 'jul_avgTMAX', 'aug_avgTMAX', 'sep_avgTMAX', 'oct_avgTMAX', 'nov_avgTMAX', 'dec_avgTMAX')
    mean_tmax$year <- rownames(mean_tmax)
    mean_tmax_annual <- as.data.frame(tapply(df$TMAX, df$year, mean, na.rm=TRUE)) #calculate mean tmax by year
    mean_tmax_annual <- QC_matrix_tmax_annual*mean_tmax_annual #convert annual means to NA where QC pass failed
    colnames(mean_tmax_annual) <- 'annual_avgTMAX'
    mean_tmax_annual$year <- rownames(mean_tmax_annual)
    #mean_tmax_summer <- as.data.frame(apply(mean_tmax[7:9], 1, mean)) #Jul-Sep tmax avg
    #colnames(mean_tmax_summer) <- 'Jul_Sep_avgTMAX'
    #mean_tmax_summer$year <- rownames(mean_tmax_summer)
    mean_tmin <- as.data.frame(tapply(df$TMIN, list(df$year, df$month), mean, na.rm=TRUE))
    mean_tmin <- QC_matrix_tmin*mean_tmin
    colnames(mean_tmin)[1:12] <- c('jan_avgTMIN', 'feb_avgTMIN', 'mar_avgTMIN', 'apr_avgTMIN', 'may_avgTMIN', 'jun_avgTMIN', 'jul_avgTMIN', 'aug_avgTMIN', 'sep_avgTMIN', 'oct_avgTMIN', 'nov_avgTMIN', 'dec_avgTMIN')
    mean_tmin$year <- rownames(mean_tmin)
    mean_tmin_annual <- as.data.frame(tapply(df$TMIN, df$year, mean, na.rm=TRUE)) #calculate mean tmin by year
    mean_tmin_annual <- QC_matrix_tmin_annual*mean_tmin_annual
    colnames(mean_tmin_annual) <- 'annual_avgTMIN'
    mean_tmin_annual$year <- rownames(mean_tmin_annual)
    #mean_tmin_summer <- as.data.frame(apply(mean_tmin[7:9], 1, mean)) #Jul-Sep tmin avg
    #colnames(mean_tmin_summer) <-'Jul_Sep_avgTMIN'
    #mean_tmin_summer$year <- rownames(mean_tmin_summer)
    mean_diurnal <- as.data.frame(tapply(df$diurnal_temp_range, list(df$year, df$month), mean, na.rm=TRUE))
    mean_diurnal <- QC_matrix_diurnal*mean_diurnal
    colnames(mean_diurnal)[1:12] <- c('jan_avgDIURNAL', 'feb_avgDIURNAL', 'mar_avgDIURNAL', 'apr_avgDIURNAL', 'may_avgDIURNAL', 'jun_avgDIURNAL', 'jul_avgDIURNAL', 'aug_avgDIURNAL', 'sep_avgDIURNAL', 'oct_avgDIURNAL', 'nov_avgDIURNAL', 'dec_avgDIURNAL')
    mean_diurnal$year <- rownames(mean_diurnal)
    mean_diurnal_annual <- as.data.frame(tapply(df$diurnal_temp_range, df$year, mean, na.rm=TRUE)) #calculate mean diurnal temp by year
    mean_diurnal_annual <- QC_matrix_diurnal_annual*mean_diurnal_annual
    colnames(mean_diurnal_annual) <- 'annual_avgDIURNAL'
    mean_diurnal_annual$year <- rownames(mean_diurnal_annual)
    #mean_diurnal_summer <- as.data.frame(apply(mean_diurnal[7:9], 1, mean)) #Jul-Sep mean diurnal temp
    #colnames(mean_diurnal_summer) <-'Jul_Sep_avgDIURNAL'
    #mean_diurnal_summer$year <- rownames(mean_diurnal_summer)
    mean_tavg <- as.data.frame(tapply(df$TAVG, list(df$year, df$month), mean, na.rm=TRUE))
    mean_tavg <- QC_matrix_tavg*mean_tavg
    colnames(mean_tavg)[1:12] <- c('jan_TAVG', 'feb_TAVG', 'mar_TAVG', 'apr_TAVG', 'may_TAVG', 'jun_TAVG', 'jul_TAVG', 'aug_TAVG', 'sep_TAVG', 'oct_TAVG', 'nov_TAVG', 'dec_TAVG')
    mean_tavg$year <- rownames(mean_tavg)
    mean_tavg_annual <- as.data.frame(tapply(df$TAVG, df$year, mean, na.rm=TRUE)) #calculate mean tmin by year
    mean_tavg_annual <- QC_matrix_tavg_annual*mean_tavg_annual
    colnames(mean_tavg_annual) <- 'annual_TAVG'
    mean_tavg_annual$year <- rownames(mean_tavg_annual)
    #mean_tavg_summer <- as.data.frame(apply(mean_tavg[7:9], 1, mean)) #Jul-Sep tmin avg
    #colnames(mean_tavg_summer) <-'Jul_Sep_TAVG'
    #mean_tavg_summer$year <- rownames(mean_tavg_summer)
#save each of the three summary data.frames (tmax, tmin, and diurnal temp range) and the revised data as record of what was actually summarized
    if (file.exists(file.path(save_path, 'TMAX_monthly')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TMAX_monthly'))
    }
    save_pathTMAX <- file.path(save_path, 'TMAX_monthly') 
    setwd(save_pathTMAX)
    write.csv(mean_tmax, paste(as.character(df$STATION_NAME[1]), '_TMAX_monthly.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'TMIN_monthly')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TMIN_monthly'))
    }
    save_pathTMIN <- file.path(save_path, 'TMIN_monthly') 
    setwd(save_pathTMIN)
    write.csv(mean_tmin, paste(as.character(df$STATION_NAME[1]), '_TMIN_monthly.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'DIURNAL_monthly')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'DIURNAL_monthly'))
    }
    save_pathDIURNAL <- file.path(save_path, 'DIURNAL_monthly') 
    setwd(save_pathDIURNAL)
    write.csv(mean_diurnal, paste(as.character(df$STATION_NAME[1]), '_DIUR_monthly.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'TAVG_monthly')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TAVG_monthly'))
    }
    save_pathTAVG <- file.path(save_path, 'TAVG_monthly') 
    setwd(save_pathTAVG)
    write.csv(mean_tavg, paste(as.character(df$STATION_NAME[1]), '_TAVG_monthly.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'TMAX_annual')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TMAX_annual'))
    }
    setwd(file.path(save_path, 'TMAX_annual'))
    write.csv(mean_tmax_annual, paste(as.character(df$STATION_NAME[1]), '_TMAX_annual.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'TMIN_annual')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TMIN_annual'))
    }
    setwd(file.path(save_path, 'TMIN_annual'))
    write.csv(mean_tmin_annual, paste(as.character(df$STATION_NAME[1]), '_TMIN_annual.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'DIURNAL_annual')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'DIURNAL_annual'))
    }
    setwd(file.path(save_path, 'DIURNAL_annual'))
    write.csv(mean_diurnal_annual, paste(as.character(df$STATION_NAME[1]), '_DIUR_annual.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'TAVG_annual')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'TAVG_annual'))
    }
    setwd(file.path(save_path, 'TAVG_annual'))
    write.csv(mean_tavg_annual, paste(as.character(df$STATION_NAME[1]), '_TAVG_annual.csv', sep = ""), row.names=FALSE)
    # if (file.exists(file.path(save_path, 'TMAX_summer')) == FALSE) { # save_path is defined above
    #   dir.create(file.path(save_path, 'TMAX_summer'))
    # }
    # setwd(file.path(save_path, 'TMAX_summer'))
    # write.csv(mean_tmax_summer, paste(as.character(df$STATION_NAME[1]), '_TMAX_summer.csv', sep = ""), row.names=FALSE)
    # if (file.exists(file.path(save_path, 'TMIN_summer')) == FALSE) { # save_path is defined above
    #   dir.create(file.path(save_path, 'TMIN_summer'))
    # }
    # setwd(file.path(save_path, 'TMIN_summer'))
    # write.csv(mean_tmin_summer, paste(as.character(df$STATION_NAME[1]), '_TMIN_summer.csv', sep = ""), row.names=FALSE)
    # if (file.exists(file.path(save_path, 'DIURNAL_summer')) == FALSE) { # save_path is defined above
    #   dir.create(file.path(save_path, 'DIURNAL_summer'))
    # }
    # setwd(file.path(save_path, 'DIURNAL_summer'))
    # write.csv(mean_diurnal_summer, paste(as.character(df$STATION_NAME[1]), '_DIUR_summer.csv', sep = ""), row.names=FALSE)
    # if (file.exists(file.path(save_path, 'TAVG_summer')) == FALSE) { # save_path is defined above
    #   dir.create(file.path(save_path, 'TAVG_summer'))
    # }
    # setwd(file.path(save_path, 'TAVG_summer'))
    # write.csv(mean_tavg_summer, paste(as.character(df$STATION_NAME[1]), '_TAVG_summer.csv', sep = ""), row.names=FALSE)
    if (file.exists(file.path(save_path, 'data_revised')) == FALSE) { # save_path is defined above
      dir.create(file.path(save_path, 'data_revised'))
    }
    save_path_data_revised <- file.path(save_path, 'data_revised') 
    setwd(save_path_data_revised)
    write.csv(df, paste(as.character(df$STATION_NAME[1]), '_data_revised.csv', sep = ""), row.names=FALSE)
  }
    if (file.exists(file.path(save_path, 'metadata')) == FALSE) { # save_path is defined above
    dir.create(file.path(save_path, 'metadata'))
  }
  setwd(file.path(save_path, 'metadata'))
  write.csv(stn_location_qc, paste(watershed_name, '_metadata.csv', sep=""), row.names = FALSE)
}

#run the function (defined above to summarize data for each station by month)
summarize_by_month('Yuba') #summer calcs are commented out for now

#print available directories (to see what folders to access for function above)
analysisDir <- 'C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/results_by_watershed_max_min_phase3' #this is also where all the results were saved
print(list.dirs(path = analysisDir))

#then, analysis by month across years, for each of the variables of concern: tmax, tmin, & diurnal temp range
library(Kendall)

temp_analysis <- function(watershed) {
#this is the location of the summarized input files for each station
  analysisDir <- 'C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/results_by_watershed_max_min_phase3' 
#establish directories to save analysis results
  results_final <- 'C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/ANALYSIS_RESULTS'
  if (dir.exists(file.path(results_final, 'DIURNAL')) == FALSE) {
    dir.create(file.path(results_final, 'DIURNAL'))
  }
  resultsDiurn <- file.path(results_final, 'DIURNAL')
  if (dir.exists(file.path(results_final, 'TAVG')) == FALSE) {
    dir.create(file.path(results_final, 'TAVG'))
  }
  resultsTAVG <- file.path(results_final, 'TAVG')
  if (dir.exists(file.path(results_final, 'TMIN')) == FALSE) {
    dir.create(file.path(results_final, 'TMIN'))
  }
  resultsTMIN <- file.path(results_final, 'TMIN')
  if (dir.exists(file.path(results_final, 'TMAX')) == FALSE) {
    dir.create(file.path(results_final, 'TMAX'))
  }
  resultsTMAX <- file.path(results_final,'TMAX')
#establish watershed input files directory
  watershedDir <- file.path(analysisDir, watershed)
  setwd(watershedDir)
#create subdirectories if necessary
  if (dir.exists(file.path(resultsDiurn, 'monthly')) == FALSE) {
    dir.create(file.path(resultsDiurn, 'monthly'))
  }
  if (dir.exists(file.path(resultsDiurn, 'annual')) == FALSE) {
    dir.create(file.path(resultsDiurn, 'annual'))
  }
  # if (dir.exists(file.path(resultsDiurn, 'summer')) == FALSE) {
  #   dir.create(file.path(resultsDiurn, 'summer'))
  # }
  if (dir.exists(file.path(resultsTAVG, 'monthly')) == FALSE) {
    dir.create(file.path(resultsTAVG, 'monthly'))
  }
  if (dir.exists(file.path(resultsTAVG, 'annual')) == FALSE) {
    dir.create(file.path(resultsTAVG, 'annual'))
  }
  # if (dir.exists(file.path(resultsTAVG, 'summer')) == FALSE) {
  #   dir.create(file.path(resultsTAVG, 'summer'))
  # }
  if (dir.exists(file.path(resultsTMAX, 'monthly')) == FALSE) {
    dir.create(file.path(resultsTMAX, 'monthly'))
  }
  if (dir.exists(file.path(resultsTMAX, 'annual')) == FALSE) {
    dir.create(file.path(resultsTMAX, 'annual'))
  }
  # if (dir.exists(file.path(resultsTMAX, 'summer')) == FALSE) {
  #   dir.create(file.path(resultsTMAX, 'summer'))
  # }
  if (dir.exists(file.path(resultsTMIN, 'monthly')) == FALSE) {
    dir.create(file.path(resultsTMIN, 'monthly'))
  }
  if (dir.exists(file.path(resultsTMIN, 'annual')) == FALSE) {
    dir.create(file.path(resultsTMIN, 'annual'))
  }
  # if (dir.exists(file.path(resultsTMIN, 'summer')) == FALSE) {
  #   dir.create(file.path(resultsTMIN, 'summer'))
  # }
#ID the subdirectories
  summary_dirs <- list.dirs(path=watershedDir)
#read in diurnal annual temp summaries by watershed and station into  list
  diurnal_yr_fnames <- list.files(path=summary_dirs[3])
  setwd(summary_dirs[3])
  diurnal_yr_dfs <- lapply(diurnal_yr_fnames, read.csv)
  names(diurnal_yr_dfs) <- diurnal_yr_fnames
#read in diurnal monthly temp summaries by watershed station into  list
  diurnal_mo_fnames <- list.files(path=summary_dirs[4])
  setwd(summary_dirs[4])
  diurnal_mo_dfs <- lapply(diurnal_mo_fnames, read.csv)
  names(diurnal_mo_dfs) <- diurnal_mo_fnames
#read in diurnal summer temp summaries by station into  list
  # diurnal_su_fnames <- list.files(path=summary_dirs[5])
  # setwd(summary_dirs[5])
  # diurnal_su_dfs <- lapply(diurnal_su_fnames, read.csv)
  # names(diurnal_su_dfs) <- diurnal_su_fnames
#read in metadata file for merging with each analysis result
  metadata_fname <- list.files(path = summary_dirs[5])
  setwd(summary_dirs[5])
  metadata_df <- read.csv(metadata_fname, stringsAsFactors = FALSE)
#read in annual tavg summaries by station into list
  tavg_yr_fnames <- list.files(path=summary_dirs[6])
  setwd(summary_dirs[6])
  tavg_yr_dfs <- lapply(tavg_yr_fnames, read.csv)
  names(tavg_yr_dfs) <- tavg_yr_fnames
#read in monthly tavg summaries by station into list
  tavg_mo_fnames <- list.files(path=summary_dirs[7])
  setwd(summary_dirs[7])
  tavg_mo_dfs <- lapply(tavg_mo_fnames, read.csv)
  names(tavg_mo_dfs) <- tavg_mo_fnames
#read in summer tavg summaries by station into list
  # tavg_su_fnames <- list.files(path=summary_dirs[9])
  # setwd(summary_dirs[9])
  # tavg_su_dfs <- lapply(tavg_su_fnames, read.csv)
  # names(tavg_su_dfs) <- tavg_su_fnames
#read in annual tmax summaries by station into  list
  tmax_yr_fnames <- list.files(path=summary_dirs[8])
  setwd(summary_dirs[8])
  tmax_yr_dfs <- lapply(tmax_yr_fnames, read.csv)
  names(tmax_yr_dfs) <- tmax_yr_fnames
#read in monthly tmax summaries by station into  list
  tmax_mo_fnames <- list.files(path=summary_dirs[9])
  setwd(summary_dirs[9])
  tmax_mo_dfs <- lapply(tmax_mo_fnames, read.csv)
  names(tmax_mo_dfs) <- tmax_mo_fnames
#read in summer tmax summaries by station into  list
  # tmax_su_fnames <- list.files(path=summary_dirs[12])
  # setwd(summary_dirs[12])
  # tmax_su_dfs <- lapply(tmax_su_fnames, read.csv)
  # names(tmax_su_dfs) <- tmax_su_fnames
#read in annual tmin summaries by station into  list
  tmin_yr_fnames <- list.files(path=summary_dirs[10])
  setwd(summary_dirs[10])
  tmin_yr_dfs <- lapply(tmin_yr_fnames, read.csv)
  names(tmin_yr_dfs) <- tmin_yr_fnames
#read in monthly tmin summaries by station into  list
  tmin_mo_fnames <- list.files(path=summary_dirs[11])
  setwd(summary_dirs[11])
  tmin_mo_dfs <- lapply(tmin_mo_fnames, read.csv)
  names(tmin_mo_dfs) <- tmin_mo_fnames
#read in annual tmin summaries by station into  list
  # tmin_su_fnames <- list.files(path=summary_dirs[15])
  # setwd(summary_dirs[15])
  # tmin_su_dfs <- lapply(tmin_su_fnames, read.csv)
  # names(tmin_su_dfs) <- tmin_su_fnames
#put each of these lists in a mega-list (SUMMER HAS BEEN REMOVED)
  all_vars_list <- list(diurnal_yr_dfs, diurnal_mo_dfs, tavg_yr_dfs, tavg_mo_dfs, tmax_yr_dfs, tmax_mo_dfs, tmin_yr_dfs, tmin_mo_dfs)
  names(all_vars_list) <- c('Diur_T_Ann', 'Diur_T_Mo', 'TAVG_Ann', 'TAVG_Mo', 'TMAX_Ann', 'TMAX_Mo', 'TMIN_Ann', 'TMIN_Mo') #Ann=annual, Mo=monthly, Su=summer(Jul-Sep)
  yr_vars_indices <- c(1, 3, 5, 7)
  mo_vars_indices <- c(2, 4, 6, 8)
  diur_vars_indices <- 1:2
  tavg_vars_indices <- 3:4
  tmax_vars_indices <- 5:6
  tmin_vars_indices <- 7:8
  analysis_starts <- c(1906, 1926, 1946, 1976, 1996, 1926, 1976, 1926)
  analysis_stops <- c(1925, 1945, 1975, 1995, 2014, 1975, 2014, 2014)
  analysis_periods <- cbind(analysis_starts, analysis_stops)
  rownames(analysis_periods) <- c('1905_1925', '1926_1945', '1946_1975', '1976_1995', '1996_2014', '1926_1975', '1976_2014', '1926_2014')
  sig_level <- 0.05
  coverage_threshold <- 0.75
  count_obs <- function(x) {
    length(na.omit(x))
  }
  for (k in 1:length(all_vars_list)) {
    df_all <- all_vars_list[[k]]
    if (k %in% mo_vars_indices) {
      for (m in 1:nrow(analysis_periods)) {
        watershed_results <- data.frame(matrix(ncol=41)) # number of rows must be flexible because many stations' observation periods do not include the analysis period of concern
        colnames(watershed_results) <- c("STATION_NAME", "watershed", "start year", "end year", "data coverage", 'jan_tau', 'jan_pval', 'jan slope', 'feb_tau', 'feb_pval', 'feb_slope', 'mar_tau', 'mar_pval', 'mar_slope', 'apr_tau', 'apr_pval', 'apr_slope', 'may_tau', 'may_pval', 'may_slope', 'jun_tau', 'jun_pval', 'jun_slope', 'jul_tau', 'jul_pval', 'jul_slope', 'aug_tau', 'aug_pval', 'aug_slope', 'sep_tau', 'sep_pval', 'sep_slope', 'oct_tau', 'oct_pval', 'oct_slope', 'nov_tau', 'nov_pval', 'nov_slope', 'dec_tau', 'dec_pval', 'dec_slope')
        c = 0 #counter for adding results to data.frame 'watershed_results'; one for each analysis period x watershed x variable
        start_yrs <- analysis_periods[m,1]
        end_yrs <- analysis_periods[m,2]
        analysis_yrs <- start_yrs:end_yrs
        for (i in 1:length(df_all)) {
          df_stn <- df_all[[i]]
          df_indices <- which(df_stn$year %in% analysis_yrs)
          if (length(df_indices) == 0){
            next
          }
          df_stn <- df_stn[df_indices,]
          if (sum(apply(df_stn[1:12], 2, count_obs)) < (end_yrs-start_yrs+1)*12*coverage_threshold) {
            next
          }
          c = c + 1
          stn_name <- names(df_all)[i]
          stn_name_end <- nchar(stn_name) - 17
          stn_name <- substr(stn_name, 1, stn_name_end)
          watershed_results[c,1] <- stn_name
          watershed_results[c,2] <- watershed
          year_start <- df_stn$year[1]
          watershed_results[c,3] <- year_start
          years <- nrow(df_stn)
          year_end <- df_stn$year[years]
          watershed_results[c,4] <- year_end
          years_data_total <- 0
            for (j in 1:12) {
              years_data_total <- years_data_total + length(na.omit(df_stn[,j]))
              mk_test <- MannKendall(df_stn[,j])
              tau <- mk_test$tau[1]
              watershed_results[c, 3*j+3] <- tau
              pval <- mk_test$sl[1]
              watershed_results[c, 3*j+4] <- pval
              if (pval < sig_level) {
                lm_calc <- lm(df_stn[,j] ~ df_stn$year, data = df_stn, na.action = na.exclude)
                slope <- lm_calc$coefficients[[2]]
                watershed_results[c, 3*j+5] <- slope
              }
              else if (pval >= sig_level){
                watershed_results[c, 3*j+5] <- NA
              }
            }
          avg_yrs_data <- years_data_total/12
          watershed_results[c,5] <- avg_yrs_data/(end_yrs-start_yrs+1)
          #watershed_results[c,42] <- i
          }
        watershed_results <- merge(x=watershed_results, y=metadata_df, by='STATION_NAME', all.x=TRUE)
        if (k %in% diur_vars_indices) { #1
          if (dir.exists(file.path(resultsDiurn, 'monthly', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsDiurn, 'monthly', rownames(analysis_periods)[m]))
          }
        setwd(file.path(resultsDiurn, 'monthly', rownames(analysis_periods)[m]))
        write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tavg_vars_indices) { #2
          if (dir.exists(file.path(resultsTAVG, 'monthly', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTAVG, 'monthly', rownames(analysis_periods)[m]))
          }
        setwd(file.path(resultsTAVG, 'monthly', rownames(analysis_periods)[m]))
        write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tmax_vars_indices) { #3
          if (dir.exists(file.path(resultsTMAX, 'monthly', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTMAX, 'monthly', rownames(analysis_periods)[m]))
          }
        setwd(file.path(resultsTMAX, 'monthly', rownames(analysis_periods)[m]))
        write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tmin_vars_indices) { #4
          if (dir.exists(file.path(resultsTMIN, 'monthly', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTMIN, 'monthly', rownames(analysis_periods)[m]))
          }
        setwd(file.path(resultsTMIN, 'monthly', rownames(analysis_periods)[m]))
        write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
      }
    } #ends section for monthly datasets. starts section for summer datasets.
    # if (k %in% su_vars_indices) {
    #   for (m in 1:nrow(analysis_periods)) {
    #     watershed_results <- data.frame(matrix(ncol=8)) # number of rows must be flexible because many stations' observation periods do not include the analysis period of concern
    #     colnames(watershed_results) <- c("STATION_NAME", 'watershed', "start year", "end year", "data coverage", 'summer tau', 'summer pval', 'summer slope')
    #     c = 0 #counter for adding results to data.frame 'watershed_results'; one for each analysis period x watershed x variable
    #     start_yrs <- analysis_periods[m,1]
    #     end_yrs <- analysis_periods[m,2]
    #     analysis_yrs <- start_yrs:end_yrs
    #     for (i in 1:length(df_all)) {
    #       df_stn <- df_all[[i]]
    #       df_indices <- which(df_stn$year %in% analysis_yrs)
    #       if (length(df_indices) == 0){
    #         next
    #       }
    #       df_stn <- df_stn[df_indices,]
    #       if (count_obs(df_stn[,1]) < (end_yrs-start_yrs+1)*coverage_threshold) {
    #         next
    #       }
    #       c = c + 1
    #       stn_name <- names(df_all)[i]
    #       stn_name_end <- nchar(stn_name) - 16
    #       stn_name <- substr(stn_name, 1, stn_name_end)
    #       print(class(stn_name))
    #       watershed_results[c,1] <- stn_name
    #       watershed_results[c,2] <- watershed
    #       year_start <- df_stn$year[1]
    #       watershed_results[c,3] <- year_start
    #       years <- nrow(df_stn)
    #       year_end <- df_stn$year[years]
    #       watershed_results[c,4] <- year_end
    #       watershed_results[c,5] <- length(na.omit(df_stn[,1]))/years
    #       mk_test <- MannKendall(df_stn[,1])
    #       tau <- mk_test$tau[1]
    #       watershed_results[c,6] <- tau
    #       pval <- mk_test$sl[1]
    #       watershed_results[c,7] <- pval
    #       if (pval < sig_level) {
    #         lm_calc <- lm(df_stn[,1] ~ df_stn$year, data = df_stn, na.action = na.exclude)
    #         slope <- lm_calc$coefficients[[2]]
    #         watershed_results[c,8] <- slope #record the linear model slope if p-value significant
    #       }
    #       else if (pval >= sig_level){
    #         watershed_results[c,8] <- NA #if p-value is non-sig
    #       }
    #       #watershed_results[c,9] <- i #to add code to data.frame
    #     }
    #     watershed_results <- merge(x=watershed_results, y=metadata_df, by='STATION_NAME', all.x=TRUE)
    #     if (k %in% diur_vars_indices) { #1
    #       if (dir.exists(file.path(resultsDiurn, 'summer', rownames(analysis_periods)[m])) == FALSE) {
    #         dir.create(file.path(resultsDiurn, 'summer', rownames(analysis_periods)[m]))
    #       }
    #       setwd(file.path(resultsDiurn, 'summer', rownames(analysis_periods)[m]))
    #       write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
    #     }
    #     if (k %in% tavg_vars_indices) { #2
    #       if (dir.exists(file.path(resultsTAVG, 'summer', rownames(analysis_periods)[m])) == FALSE) {
    #         dir.create(file.path(resultsTAVG, 'summer', rownames(analysis_periods)[m]))
    #       }
    #       setwd(file.path(resultsTAVG, 'summer', rownames(analysis_periods)[m]))
    #       write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
    #     }
    #     if (k %in% tmax_vars_indices) { #3
    #       if (dir.exists(file.path(resultsTMAX, 'summer', rownames(analysis_periods)[m])) == FALSE) {
    #         dir.create(file.path(resultsTMAX, 'summer', rownames(analysis_periods)[m]))
    #       }
    #       setwd(file.path(resultsTMAX, 'summer', rownames(analysis_periods)[m]))
    #       write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
    #     }
    #     if (k %in% tmin_vars_indices) { #4
    #       if (dir.exists(file.path(resultsTMIN, 'summer', rownames(analysis_periods)[m])) == FALSE) {
    #         dir.create(file.path(resultsTMIN, 'summer', rownames(analysis_periods)[m]))
    #       }
    #       setwd(file.path(resultsTMIN, 'summer', rownames(analysis_periods)[m]))
    #       write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
    #     }
    #   }
    # } #this ends the summer section and begins the annual section.
    if (k %in% yr_vars_indices) {
      for (m in 1:nrow(analysis_periods)) {
        watershed_results <- data.frame(matrix(ncol=8)) # number of rows must be flexible because many stations' observation periods do not include the analysis period of concern
        colnames(watershed_results) <- c("STATION_NAME", 'watershed', "start year", "end year", "data coverage", 'annual tau', 'annual pval', 'annual slope')
        c = 0 #counter for adding results to data.frame 'watershed_results'; one for each analysis period x watershed x variable
        start_yrs <- analysis_periods[m,1]
        end_yrs <- analysis_periods[m,2]
        analysis_yrs <- start_yrs:end_yrs
        for (i in 1:length(df_all)) {
          df_stn <- df_all[[i]]
          df_indices <- which(df_stn$year %in% analysis_yrs)
          if (length(df_indices) == 0){
            next
          }
          df_stn <- df_stn[df_indices,]
          if (count_obs(df_stn[,1]) < (end_yrs-start_yrs+1)*coverage_threshold) {
            next
          }
          c = c + 1
          stn_name <- names(df_all)[i]
          stn_name_end <- nchar(stn_name) - 16
          stn_name <- substr(stn_name, 1, stn_name_end)
          watershed_results[c,1] <- stn_name
          watershed_results[c,2] <- watershed
          year_start <- df_stn$year[1]
          watershed_results[c,3] <- year_start
          years <- nrow(df_stn)
          year_end <- df_stn$year[years]
          watershed_results[c,4] <- year_end
          watershed_results[c,5] <- length(na.omit(df_stn[,1]))/years
          mk_test <- MannKendall(df_stn[,1])
          tau <- mk_test$tau[1]
          watershed_results[c,6] <- tau
          pval <- mk_test$sl[1]
          watershed_results[c,7] <- pval
          if (pval < sig_level) {
            lm_calc <- lm(df_stn[,1] ~ df_stn$year, data = df_stn, na.action = na.exclude)
            slope <- lm_calc$coefficients[[2]]
            watershed_results[c,8] <- slope #record the linear model slope if p-value significant
          }
          else if (pval >= sig_level){
            watershed_results[c,8] <- NA #if p-value is non-sig
          }
          #watershed_results[c,9] <- i #to add code to data.frame
        }
        watershed_results <- merge(x=watershed_results, y=metadata_df, by='STATION_NAME', all.x=TRUE)
        if (k %in% diur_vars_indices) { #1
          if (dir.exists(file.path(resultsDiurn, 'annual', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsDiurn, 'annual', rownames(analysis_periods)[m]))
          }
          setwd(file.path(resultsDiurn, 'annual', rownames(analysis_periods)[m]))
          write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tavg_vars_indices) { #2
          if (dir.exists(file.path(resultsTAVG, 'annual', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTAVG, 'annual', rownames(analysis_periods)[m]))
          }
          setwd(file.path(resultsTAVG, 'annual', rownames(analysis_periods)[m]))
          write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tmax_vars_indices) { #3
          if (dir.exists(file.path(resultsTMAX, 'annual', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTMAX, 'annual', rownames(analysis_periods)[m]))
          }
          setwd(file.path(resultsTMAX, 'annual', rownames(analysis_periods)[m]))
          write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
        if (k %in% tmin_vars_indices) { #4
          if (dir.exists(file.path(resultsTMIN, 'annual', rownames(analysis_periods)[m])) == FALSE) {
            dir.create(file.path(resultsTMIN, 'annual', rownames(analysis_periods)[m]))
          }
          setwd(file.path(resultsTMIN, 'annual', rownames(analysis_periods)[m]))
          write.csv(watershed_results, paste(watershed, names(all_vars_list)[k], rownames(analysis_periods)[m], '_mk_analysis.csv', sep=''), row.names=FALSE)
        }
      }
    }
  }
}

#run the analysis function HERE
temp_analysis('Yuba')

#merge results files by variable x time period combination
map_analysis <- function() {
  library(sp)
  library(rgdal)
  library(raster)
  results <- 'C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/ANALYSIS_RESULTS'
  tmax_mo <- file.path(results, 'TMAX/monthly')
  tmax_yr <- file.path(results, 'TMAX/annual')
  tmin_mo <- file.path(results, 'TMIN/monthly')
  tmin_yr <- file.path(results, 'TMIN/annual')
  tavg_mo <- file.path(results, 'TAVG/monthly')
  tavg_yr <- file.path(results, 'TAVG/annual')
  diur_mo <- file.path(results, 'DIURNAL/monthly')
  diur_yr <- file.path(results, 'DIURNAL/annual')
  yr_vars_indices <- c(1, 3, 5, 7)
  mo_vars_indices <- c(2, 4, 6, 8)
  diur_vars_indices <- 1:2
  tavg_vars_indices <- 3:4
  tmax_vars_indices <- 5:6
  tmin_vars_indices <- 7:8
  var_by_period <- c(diur_yr, diur_mo, tavg_yr, tavg_mo, tmax_yr, tmax_mo, tmin_yr, tmin_mo)
  save_path <- file.path(results, 'all_watersheds')
  create_directories <- function(x) { 
    if (dir.exists(file.path(save_path, x, 'annual')) == FALSE) {
      dir.create(file.path(save_path, x, 'annual'), recursive = TRUE)
    }
    if (dir.exists(file.path(save_path, x, 'monthly')) == FALSE) {
      dir.create(file.path(save_path, x, 'monthly'), recursive = TRUE)
    }
  }
  create_directories('DIURNAL')
  create_directories('TMAX')
  create_directories('TMIN')
  create_directories('TAVG')
  final_save <- function(vars_indices, period_indices, folder1, folder2) {
    if (i %in% vars_indices){
      if (i %in% period_indices){
        setwd(file.path(save_path, folder1, folder2))
        write.csv(df_combined, paste(basename(var_subset_dirs[j]), 'mk_analysis.csv', sep = ''), row.names = FALSE)
        shapefile(df_shp, paste(basename(var_subset_dirs[j]), 'mk_analysis', sep=''))
      }
    }
  }
  for (i in 1:length(var_by_period)) {
    var_subset_dirs <- list.dirs(var_by_period[i])
    for (j in 2:length(var_subset_dirs)) {
      var_subset_files <- list.files(var_subset_dirs[j])
      setwd(var_subset_dirs[j])
      var_subset_dfs <- lapply(var_subset_files, read.csv, stringsAsFactors = FALSE)
      names(var_subset_dfs) <- var_subset_files
      df_combined <- as.data.frame(matrix(ncol = length(var_subset_dfs[[1]])))
      colnames(df_combined) <- names(var_subset_dfs[[1]])
      for (k in 1:length(var_subset_dfs)) {
        df <- var_subset_dfs[[k]]
        if (is.na(df$STATION_NAME[1])) { #skip the analysis results files that are empty
          next
        }
        df_combined <- rbind(df_combined, df)
      }
      df_combined <- df_combined[-1,] #delete the first row, which was a dummy row
      unique_stations <- !duplicated(df_combined$STATION) #some of the watersheds had duplicate stations.  this removes them
      df_combined <- df_combined[unique_stations, ]
      df_shp <- SpatialPointsDataFrame(coords = cbind(df_combined$LONGITUDE, df_combined$LATITUDE), data=df_combined, proj4string=CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))#no documentation from CDO on which datum their geographic coordinate system is relative to, but NAD83 is most commone from US Federal agencies
      final_save(diur_vars_indices, yr_vars_indices, 'DIURNAL', 'annual')
      final_save(diur_vars_indices, mo_vars_indices, 'DIURNAL', 'monthly')
      final_save(tavg_vars_indices, yr_vars_indices, 'TAVG', 'annual')
      final_save(tavg_vars_indices, mo_vars_indices, 'TAVG', 'monthly')
      final_save(tmax_vars_indices, yr_vars_indices, 'TMAX', 'annual')
      final_save(tmax_vars_indices, mo_vars_indices, 'TMAX', 'monthly')
      final_save(tmin_vars_indices, yr_vars_indices, 'TMIN', 'annual')
      final_save(tmin_vars_indices, mo_vars_indices, 'TMIN', 'monthly')
    }
  }
}
#run the function to synthesize the results
map_analysis()

#stn data check[manually link to data_QC directory in each watershed data folder]
setwd('C:/Users/smdevine/Desktop/Forest dieoff and hydrology/long.term.temperatures/results_by_watershed_max_min/San_Joaquin/data_revised')
data_fnames <- list.files()
data_dfs <- lapply(data_fnames, read.csv)
names(data_dfs) <- data_fnames
for (i in 1:length(data_dfs)) {
  print(names(data_dfs)[i])
  print(paste(max(data_dfs[[i]]$TMAX, na.rm = TRUE), 'is the max temp.', sep = ' '))
  print(paste(min(data_dfs[[i]]$TMIN, na.rm = TRUE), 'is the min temp.', sep = ' '))
  print(paste(max(data_dfs[[i]]$diurnal_temp_range, na.rm = TRUE), 'is the max diurnal temp range.', sep = ' '))
}





#test code
for (i in 1:length(df_all)) {
  df_stn <- df_all[[1]]
  stn_name <- names(df_all)[1]
  stn_name_end <- nchar(stn_name) - 17
  stn_name <- substr(stn_name, 1, stn_name_end)
  year_start <- df_stn$year[1]
  years <- nrow(df_stn)
  year_end <- df_stn$year[years]
  for (j in 1:18) {
    lm.fit <- lm(df_stn[,j] ~ df_stn$year, data=df_stn)
    colname <- colnames(df_stn)[j]
    colname_end <- nchar(colname)-3
    colname <- substr(colname, 2, colname_end)
    print(paste(stn_name, colname))
    print(summary(lm.fit))
    plot(df_stn$year, df_stn[,j], type='b', xlab="Year", ylab=paste('avg max temp', colname), main=stn_name, font.main=2, font.lab=1)
    abline(abline(lm.fit, lty="dashed"))
    MannKendall(df_stn[,j])
  }
}


