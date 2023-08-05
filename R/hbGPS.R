hbGPS = function(gps_file = NULL,
                 outputDir = NULL,
                 idloc = 1,
                 maxBreakLengthSeconds = 120,
                 minTripDur = 60,
                 mintripDist_m = 100,
                 threshold_snr = 225,
                 threshold_snr_ratio = 50,
                 tz = "",
                 time_format = "%d/%m/%Y %H:%M:%SO",
                 GGIRpath = NULL,
                 verbose = TRUE,
                 outputFormat = "default",
                 AccThresholds = NULL) {
  
  outputFolder = paste0(outputDir, "/hbGPSoutput")
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  # Log input parameters:
  param_log = data.frame(gps_file = gps_file,
                         outputDir = outputDir,
                         idloc = idloc,
                         maxBreakLengthSeconds = maxBreakLengthSeconds,
                         minTripDur = minTripDur,
                         mintripDist_m = mintripDist_m,
                         threshold_snr = threshold_snr,
                         threshold_snr_ratio = threshold_snr_ratio,
                         tz = tz,
                         time_format = time_format,
                         GGIRpath = GGIRpath)
  
  sink(paste0(outputDir, "/parameter_log.txt"))
  for (i in 1:ncol(param_log)) {
    txt = paste0(colnames(param_log)[i], " =  ", param_log[1,i], "\n")
    cat(txt)
  }
  sink()
  
  
  if (dir.exists(gps_file)) {
    gps_file = dir(gps_file, full.names = TRUE)
  }
  
  for (filei in gps_file) {
    if (verbose == TRUE) cat(paste0("\n", basename(filei)))
    # Load GPS file csv
    out = load_and_tidy_up_GPS(gps_file = filei,
                               idloc = idloc,
                               tz = tz,
                               time_format = time_format)
    D = out$df
    ID = out$ID
    rm(out)
    
    #====================================
    # Merge with accelerometer output
    # Do this early on, such that there is room for using GGIR output in the GPS processing
    MD = mergeGGIR(GGIRpath = GGIRpath, GPSdf = D, ID = ID, verbose = verbose)
    D = MD$GPSdf
    log_acc = MD$log_acc
    rm(MD)
    # Prepare output filenames
    outfn = unlist(strsplit(basename(filei), split = "[.]csv"))
    pdffile = paste0(outputDir, "/", outfn, "_", ID, ".pdf")
    shpfile =  paste0(outputDir, "/", outfn, "_", ID, ".shp")
    
    # Derive variables for all time points, e.g. distance, speed, deltaTime, deltaElevation
    D = deriveVars(D)
    
    # Remove outliers / implausible values
    D = removeOutliers(D)
    
    # Initial state classification
    D = initialStateClassification(df = D,
                                   threshold_snr = threshold_snr,
                                   threshold_snr_ratio = threshold_snr_ratio)
    
    #======================================================
    # Reclassify based on state duration
    #For this we need continuous timeseries
    # duplicate rows to ensure regular time intervals
    # Note that these will be removed later, but are needed now to
    # extract segment lengths
    
    D = D[rep(1:nrow(D), D$deltaTime),] 
    D$duplicated = duplicated(D$time)
    
    # Set state of trip breaks to 3
    # trip breaks: any break in between trips shorter than X seconds will get state 3
    short_breaks = function(x, threshold = 60) {
      segments = rle(x)
      cent = 2:(length(segments$values) - 1)
      left = 1:(length(segments$values) - 2)
      right = 3:length(segments$values)
      br = which(
        segments$values[cent] == 2 &
          segments$length[cent] <= threshold &
          segments$values[left] > 2 & segments$values[right] > 2
      )
      if (length(br) > 0) {
        segments$values[br + 1] = 3 # note that these can also be brief indoor periods
      }
      x = rep(segments$values, times = segments$lengths)
    }
    maxBreakLengthSecond = 60
    D$state = short_breaks(x = D$state, threshold = maxBreakLengthSecond)
    
    D$state[which(D$indoor == TRUE & D$state != 3)] = 1
    
    # remove tagged data
    i99 = which(D$state == 99)
    if (length(i99) > 0) {
      D = D[-i99, ]
    }
    
    #================
    # Restore original resolution of data
    D = D[which(D$duplicated == FALSE), ]
    
    # Derive trips by looking at sequences of states
    # No state classification here or further down
    D = deriveTrips(df = D, tz = tz,
                    minTripDur = minTripDur,
                    mintripDist_m = mintripDist_m)
    
    # convert units back to degrees
    D$lat = units::set_units(D$lat, "degrees")
    D$lon = units::set_units(D$lon, "degrees")
    
    Ntrips = length(unique(D$trip)) - 1 
    
    # Add factor variable with statenames
    statenames = c("indoor/tunnel", # state 1
                   "stationary", # state 2
                   "trip break", # state 3
                   "1-7 km/h", "7-10 km/h", # state 4 5
                   "10-15 km/h", # state 6
                   "15-35 km/h", ">35 km/h") # state 7 8
    D$statenames = cut(D$state, breaks = seq(0.5, 8.5, 1), labels = statenames)
    
    
    if (verbose == TRUE) cat(paste0(" => ", Ntrips, " trips"))
    
    if (outputFormat == "PALMS") {
      if (length(grep(pattern = "GGIR", x = colnames(D), ignore.case = TRUE)) > 0) {
        # Rename existing columns
        names(D)[which(names(D) == "trip")] = "tripNumber"
        names(D)[which(names(D) == "mot")] = "tripMOT"
        names(D)[which(names(D) == "time")] = "dateTime"
        names(D)[which(names(D) == "GGIR_ACC")] = "activity"
        D$identifier = ID
        
        #-----------------------------------------------------------------
        # Add missing columns
        D$dow = as.POSIXlt(D$time)$wday
        
        D$fixTypeCode = -1 # unclear whether palmsplusr needs this
        D$tripType = 0 # default stationary (tripType 0 is also stationary)
        D$tripType[which(D$state == 3)] = 3 #pause point
        D$tripType[which(duplicated(D$tripNumber) == FALSE & D$tripNumber != 0)] = 1 #pause point
        D$tripType[which(rev(duplicated(rev(D$tripNumber)) == FALSE & rev(D$tripNumber) != 0))] = 4 #pause point
        # Note: palmsplusr does not seem to be needing trip midpoints (tripType = 2), so we skip those
        
        # Add accelerometer based columns
        nonwear = which(D$GGIR_invalidepoch == 1)
        if (length(nonwear) > 0) D$activity[nonwear] = -2
        D$activityIntensity = -2
        if (AccThresholds[1] != 0) AccThresholds = c(0, AccThresholds)
        for (AT in 1:length(AccThresholds)) {
          if (AT < length(AccThresholds) - 1) {
            conditionMet = which(D$activity >= AccThresholds[AT] &
                                   D$activity < AccThresholds[AT + 1])
          } else {
            conditionMet = which(D$activity >= AccThresholds[AT])
            
          }
          if (length(conditionMet) > 0) {
            D$activityIntensity[conditionMet] = AT - 1
          }
        }
        D$activityIntensity[nonwear] = -2
        D$GGIR_class_name = format(D$GGIR_class_id)
        D$activityBoutNumber = 0
        D$sedentaryBoutNumber = 0
        D$activityBoutNumber[grep(pattern = "_MVPA_bts_", x = D$GGIR_class_name, value = FALSE)] = 1
        D$sedentaryBoutNumber[grep(pattern = "_IN_", x = D$GGIR_class_name, value = FALSE)] = 1
        if (length(nonwear) > 0) {
          D$sedentaryBoutNumber[nonwear] = 0
          D$activityBoutNumber[nonwear] = 0
        }
        addBoutNumber = function(x) {
          dx = diff(x)
          s0 = which(dx == 1) + 1
          s1 = which(dx == -1)
          if (s1[1] < s0[1]) s1 = s1[-1]
          if (length(s0) > length(s1)) s0 = s0[1:length(s1)]
          cnt = 1
          for (si in 1:length(s0)) {
            x[s0[si]:s1[si]] = cnt
            cnt = cnt + 1
          }
          return(x)
        }
        D$activityBoutNumber = addBoutNumber(D$activityBoutNumber)
        D$sedentaryBoutNumber = addBoutNumber(D$sedentaryBoutNumber)
        # force time to character format to allign it with how PALMS stored timestamps
        D$dateTime = format(D$dateTime, format = "%d/%m/%Y  %H:%M:%S")
        D = D[, c("identifier", "dateTime", "dow", "lat", "lon", 
                  "fixTypeCode", "iov", "tripNumber", "tripType", "tripMOT", 
                  "activity", "activityIntensity", "activityBoutNumber",
                  "sedentaryBoutNumber")]
      } else {
        if (verbose == TRUE) cat(" => not exported because no matching GGIR data")
        next
      }
    }
    outputFileName = paste0(outputFolder, "/", unlist(strsplit(basename(filei), "[.]csv"))[1], ".csv")
    data.table::fwrite(D, file = outputFileName)
  }
  return(D)
}
