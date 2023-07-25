load_and_tidy_up_GPS = function(gps_file, idloc = NULL, tz = "") {
  df = data.table::fread(file = gps_file, data.table = FALSE)
  if ("ptid" %in% colnames(df)) { # Specific for MSSE dataset
    UID = unique(df$ptid)
    ID = UID[7]
    df = df[which(df$ptid == ID),]
  } else {
    ID = 0
  }
  if (idloc == 2) {
    ID = unlist(strsplit(basename(gps_file), "_"))[1]
  }
  
  #===========================================
  # Standardise format
  colnames(df) = tolower(colnames(df))
  colnames(df) = gsub(pattern = "local date", replacement = "date", x = colnames(df))
  colnames(df) = gsub(pattern = "local time", replacement = "time", x = colnames(df))
  colnames(df) = gsub(pattern = "speed[(]km/h[)]", replacement = "speed_kmh", x = colnames(df))
  colnames(df) = gsub(pattern = "height[(]m[)]", replacement = "height_m", x = colnames(df))
  colnames(df) = gsub(pattern = "latitude", replacement = "lat", x = colnames(df))
  colnames(df) = gsub(pattern = "longitude", replacement = "lon", x = colnames(df))
  colnames(df) = gsub(pattern = "nsat [(]used/view[)]", replacement = "nsat_uv", x = colnames(df))
  colnames(df) = gsub(pattern = "sat info [(]sid-snr[)]", replacement = "satinfo", x = colnames(df))
  
  cat(paste0("\nLATITUDE ", mean(df$lat), " longitude ", mean(df$lon), "\n"))
  
  # Time
  if (length(grep(pattern = "sensecam", x = colnames(df))) > 0) {
    df$time = as.POSIXct(df$datetime, tz = tz)
  } else {
    df$time = as.POSIXct(paste(df$date, df$time, sep = " "), tz = tz)
  }
  # Order by timestamp
  df = df[order(df$time), ]
  
  # Remove duplicates
  Nduplicated = length(which(duplicated(df[, c("time", "lon", "lat")]) == TRUE))
  if (Nduplicated > 0) {
    warning(paste0(Nduplicated, " duplicated data points found, check whether timestamps have seconds"))
    df = df[!duplicated(df[, c("time", "lon", "lat")]),]
  }
  
  # Remove some columns that will not be used
  df = df[, which(colnames(df) %in% c("distance", "speed_kmh",
                                   "dsta", "dage", "pdop", "vdop", "nsat", "sid", "valid",
                                   "hdop", "azimuth", "snr", "rcr", "ms") == FALSE)]
  
  # Flip lon if coordinates are in the west
  if ("e/w" %in% colnames(df) & all(df$lon > 0)) {
    df$lon = df$lon * ifelse(df$`e/w` == "W", yes = -1, no = 1)
  }
  
  # Flip lat if coordinates are in the south
  if ("n/s" %in% colnames(df) & all(df$lat > 0)) {
    df$lat = df$lat * ifelse(df$`n/a` == "S", yes = -1, no = 1)
  }
  
  invisible(list(df = df, ID = ID))
}