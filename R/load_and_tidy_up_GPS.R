load_and_tidy_up_GPS = function(gps_file, idloc = NULL, tz = "", time_format = "%d/%m/%Y %H:%M:%SO") {
  df = data.table::fread(file = gps_file, data.table = FALSE)
  if (all(c("V1", "V2", "V3", "V4") %in% colnames(df)[1:4])) {
    df = data.table::fread(file = gps_file, data.table = FALSE, header = TRUE)
  }
  if ("ptid" %in% colnames(df)) { # Specific for MSSE dataset
    UID = unique(df$ptid)
    ID = UID[7]
    df = df[which(df$ptid == ID),]
  } else {
    ID = 0
  }
  if (idloc == 2) {
    ID = unlist(strsplit(basename(gps_file), "_"))[1]
  } else if (idloc == 6) {
    ID = unlist(strsplit(basename(gps_file), "[.]"))[1]
  }
  
  #==================================0=========
  # Standardise format
  colnames(df) = tolower(colnames(df))
  
  # Time
  if (any(c("time", "date") %in% colnames(df))) {
    stop(paste0("GPS data is expected to not have a column named \"time\" ",
                "or \"date\". Please replace by \"local time\" and \"local date\" ",
                " \"utc time\" and \"utc date\" ,or \"datetime\"."))
  }
  # Identify whether UTC time needs to be converted to local time
  convert_UTC2local = FALSE
  if ("local time" %in% colnames(df) &&
      length(grep(pattern = "T| ", x = format(df$`local time`[1]))) > 0) {
    # local time is the full date time
    colnames(df) = gsub(pattern = "local time", replacement = "datetime", x = colnames(df))
  } else if (all(c("local time", "local date") %in% colnames(df))) {
    # local time and local date are stored separately
    colnames(df) = gsub(pattern = "local time", replacement = "time", x = colnames(df))
    colnames(df) = gsub(pattern = "local date", replacement = "date", x = colnames(df))
    df = df[, which(colnames(df) %in% c("utc time", "utc date") == FALSE)]
  } else if (all(c("utc time", "utc date") %in% colnames(df))) {
    # local time and ate are missing, but utc time and date are present
    convert_UTC2local = TRUE
    colnames(df) = gsub(pattern = "utc time", replacement = "time", x = colnames(df))
    colnames(df) = gsub(pattern = "utc date", replacement = "date", x = colnames(df))
  } else {
    # none of the above worked, give warning
    stop(paste0("GPS data is expected to either have column \"datetime\" (expressed in local timezone)",
                " or columns \"utc time\" and \"utc date\" ",
                "or columns \"local time\" and \"local date\""))
  }
  if ("datetime" %in% colnames(df)) {
    oldtime = df$datetime[pmin(10, nrow(df))]
    newTime = as.POSIXct(df$datetime, tz = tz, format = time_format,
                         origin = "1970-01-01")
  } else {
    oldtime = paste0(df$date[pmin(10, nrow(df))], " ", df$time[pmin(10, nrow(df))])
    if (convert_UTC2local == TRUE) {
      newTime = as.POSIXct(as.POSIXct(paste(df$date, df$time, sep = " "),
                                      tz = "UTC", format = time_format,
                                      origin = "1970-01-01"), tz = tz)
    } else {
      newTime = as.POSIXct(paste(df$date, df$time, sep = " "),
                           tz = tz, format = time_format,
                           origin = "1970-01-01")
    }
  }
  # Check that timestamp conversion worked
  if (any(is.na(newTime[1:pmin(100, length(newTime))])) == TRUE) {
    stop(paste0("Specified time_format is ", time_format, " we see ", oldtime))
  } else {
    df$time = newTime
  }
  # Order by timestamp
  df = df[order(df$time), ]
  colnames(df) = gsub(pattern = "height[(]m[)]", replacement = "height_m", x = colnames(df))
  colnames(df) = gsub(pattern = "latitude", replacement = "lat", x = colnames(df))
  colnames(df) = gsub(pattern = "longitude", replacement = "lon", x = colnames(df))
  colnames(df) = gsub(pattern = "nsat[(]used/view[)]", replacement = "nsat_uv", x = colnames(df))
  colnames(df) = gsub(pattern = "nsat [(]used/view[)]", replacement = "nsat_uv", x = colnames(df))
  colnames(df)[grep(pattern = "sat info ", x = colnames(df))] = "satinfo"
  # cat(paste0("\nLATITUDE ", mean(df$lat), " longitude ", mean(df$lon), "\n"))
  
  if ("satinfo" %in% colnames(df) == FALSE && all(c("sid", "snr") %in% colnames(df))) {
    # merge sid and snr to keep it consistent with expected satinfo column
    fuse_sid_snr = function(x) {
      sid  = unlist(strsplit(x[1], ";"))
      snr = unlist(strsplit(x[2], ";"))
      satinfo = paste0(paste0(sid, snr), collapse = ";")
      return(satinfo)
    }
    df$satinfo = apply(X = df[, c("sid", "snr")], MARGIN = 1, FUN = fuse_sid_snr)
  }
  if ("nsat_uv" %in% colnames(df) == FALSE && all(c("sid", "nsat") %in% colnames(df))) {
    df$nsat = suppressWarnings(as.numeric(df$nsat)) # force to number and set character values to missing
    create_nsatview = function(x) {
      nview  = length(unlist(strsplit(x[1], ";")))
      nsat = x[2]
      nsatview = paste0(paste0(nsat, "(", nview, ")"), collapse = ";")
      return(nsatview)
    }
    df$nsat_uv = apply(X = df[, c("sid", "nsat")], MARGIN = 1, FUN = create_nsatview)
  }

  # Remove duplicates
  Nduplicated = length(which(duplicated(df[, c("time", "lon", "lat")]) == TRUE))
  if (Nduplicated > 0) {
    if (Nduplicated > 10) {
      warning(paste0(Nduplicated, " duplicated data points found, check whether timestamps have seconds"))
    }
    df = df[!duplicated(df[, c("time", "lon", "lat")]),]
  }
  
  # Remove some columns that will not be used
  df = df[, which(colnames(df) %in% c("distance", "speed[(]km/h[)]",
                                   "dsta", "dage", "pdop", "vdop", "nsat", "sid", "valid",
                                   "hdop", "azimuth", "snr", "rcr", "ms") == FALSE)]
  df$lon = suppressWarnings(as.numeric(df$lon)) # force to number and set character values to missing
  df$lat = suppressWarnings(as.numeric(df$lat)) # force to number and set character values to missing
  df = df[which(is.na(df$lon) == FALSE & is.na(df$lat) == FALSE),]
  
  # Flip lon if coordinates are in the west
  if ("e/w" %in% colnames(df) & all(df$lon > 0)) {
    df$lon = df$lon * ifelse(df$`e/w` == "W", yes = -1, no = 1)
  }
  # Flip lat if coordinates are in the south
  if ("n/s" %in% colnames(df) & all(df$lat > 0)) {
    df$lat = df$lat * ifelse(df$`n/s` == "S", yes = -1, no = 1)
  }
  invisible(list(df = df, ID = ID))
}