hbGPS = function(gps_file = NULL,
                 GGIRpath = NULL,
                 outputDir = NULL,
                 configFile = NULL,
                 verbose = TRUE,
                 return_object = FALSE, ...) {
  input = list(...)
  # Load arguments from configFile
  argNames = names(input)
  if (!is.null(configFile)) {
    config = data.table::fread(file = configFile, stringsAsFactors = FALSE, data.table = FALSE)
    if (nrow(config) > 1) {
      for (ci in 1:nrow(config)) {
        varName = as.character(config[ci, 1])
        # Only overwrite with config file value if it is not provided by user
        if (varName %in% c(argNames, "") == FALSE) {
          if (config$argument[ci] == "idloc") {
            idloc = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "maxBreakLengthSeconds") {
            maxBreakLengthSeconds = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "minTripDur") {
            minTripDur = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "minTripDist_m") {
            minTripDist_m = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "threshold_snr") {
            threshold_snr = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "threshold_snr_ratio") {
            threshold_snr_ratio = as.numeric(config$value[ci])
          } else if (config$argument[ci] == "tz") {
            tz = as.character(config$value[ci])
          } else if (config$argument[ci] == "time_format") {
            time_format = as.character(config$value[ci])
          } else if (config$argument[ci] == "outputFormat") {
            outputFormat = as.character(config$value[ci])
          } else if (config$argument[ci] == "AccThresholds") {
            if (grepl("c\\(", config$value[ci])) { # vector
              tmp = c(gsub(pattern = "c|\\(|\\)", x = config$value[ci], replacement = ""))
              tmp = unlist(strsplit(tmp, ","))
              suppressWarnings(try(expr = {isna = is.na(as.numeric(tmp[1]))}, silent = TRUE))
              # if (length(isna) == 0) isna = FALSE
              # if (isna == TRUE) {
              #   newValue = tmp # vector of characters
              # } else {
              newValue = as.numeric(tmp) # vector of numbers
              # }
              AccThresholds = newValue
            }
          }
        }
      }
    }
  }
  # Defaults if argument is not provided by user or configfile
  name = "idloc"
  if (name %in% argNames) {
    idloc = input[[name]]
  } else if (!exists(name)) {
    idloc = 2
  }
  name = "maxBreakLengthSeconds"
  if (name %in% argNames) {
    maxBreakLengthSeconds = input[[name]]
  } else if (!exists(name)) {
    maxBreakLengthSeconds = 120
  }
  
  name = "minTripDur"
  if (name %in% argNames) {
    minTripDur = input[[name]]
  } else if (!exists(name)) {
    minTripDur = 60
  }
  
  name = "minTripDist_m"
  if (name %in% argNames) {
    minTripDist_m = input[[name]]
  } else if (!exists(name)) {
    minTripDist_m = 100
  }
  
  name = "threshold_snr"
  if (name %in% argNames) {
    threshold_snr = input[[name]]
  } else if (!exists(name)) {
    threshold_snr = 225
  }
  
  name = "threshold_snr_ratio"
  if (name %in% argNames) {
    threshold_snr_ratio = input[[name]]
  } else if (!exists(name)) {
    threshold_snr_ratio = 50
  }
  
  name = "tz"
  if (name %in% argNames) {
    tz = input[[name]]
  } else if (!exists(name)) {
    tz = ""
  }
  name = "time_format"
  if (name %in% argNames) {
    time_format = input[[name]]
  } else if (!exists(name)) {
    time_format = "%d/%m/%Y %H:%M:%SO"
  }
  
  name = "outputFormat"
  if (name %in% argNames) {
    outputFormat = input[[name]]
  } else if (!exists(name)) {
    outputFormat = "default"
  }
  
  name = "AccThresholds"
  if (name %in% argNames) {
    AccThresholds = input[[name]]
  } else if (!exists(name)) {
    AccThresholds = NULL
  }
  #--------------------------------
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
                         minTripDist_m = minTripDist_m,
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
                    minTripDist_m = minTripDist_m)
    
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
    
    # print motivation for excluding files
    if (log_acc == 1) {
      if (verbose == TRUE) cat("\n  (X): GPS ID not found in ACC files")
    } else if (log_acc == 2) {
      if (verbose == TRUE) cat("\n  (X): ACC class dictionary not identify")
    } else if (log_acc == 3) {
      if (!file.exists(GGIR_ts_path) & verbose == TRUE) cat(paste0("\n  (X): path ", GGIR_ts_path, " does not exist"))
      if (!file.exists(GGIR_legend) & verbose == TRUE) cat(paste0("\n  (X): path ", GGIR_legend, " does not exist"))
    } else if (log_acc == 4) {
      if (verbose == TRUE) cat(paste0("\n  (X): ACC ID does not overlap with GPS data"))
    } else if (log_acc == 5) {
      if (verbose == TRUE) {
        cat(paste0("\n  (X): Less than 30% of matching ACC data is valid"))
      }
    } else if (log_acc == 0) {
      cat(paste0("\n  (V)"))
    }
    
    if (outputFormat == "PALMS") {
      out = imitatePALMSformat(D = D, ID = ID,
                               AccThresholds = AccThresholds,
                               verbose = verbose)
      D = out$D
      runNext = out$runNext
      if (runNext == TRUE) next
    }
    outputFileName = paste0(outputFolder, "/", unlist(strsplit(basename(filei), "[.]csv"))[1], ".csv")
    data.table::fwrite(D, file = outputFileName)
  }
  if (return_object == TRUE) {
    return(D)
  } else {
    return()
  }
}
