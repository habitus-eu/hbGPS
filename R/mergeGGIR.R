mergeGGIR = function(GGIRpath, GPSdf, ID) {
  # GGIRpath: path to ms5.rawout folder produced by GGIR
  # GPSdf: dataframe create by rest of code
  
  # Identify matching GGIR output:
  items = dir(GGIRpath, full.names = TRUE)
  GGIR_legend = items[grep(pattern = "behavioralcodes", x = basename(items))]
  GGIR_legend = GGIR_legend[which.max(file.info(GGIR_legend)$ctime)]
  
  # select first time series in the folder, maybe later make this flexible to select specific time series
  GGIR_ts_paths = dir(items[grep(pattern = "behavioralcodes", x = basename(items), invert = TRUE)][1], full.names = TRUE)
  GGIR_ts_path = GGIR_ts_paths[grep(pattern = ID, x = GGIR_ts_paths, value = FALSE)]
  
  
  log_acc = 1
  if (!is.null(GGIR_ts_path) | !is.null(GGIR_legend)) {
    log_acc = 2
    if (file.exists(GGIR_ts_path) & file.exists(GGIR_legend)) {
      log_acc = 3
      G = data.table::fread(file = GGIR_ts_path, data.table = FALSE)
      Legend = data.table::fread(file = GGIR_legend, data.table = FALSE)
      GPSdf$timenum = as.numeric(GPSdf$time)
      rangeG = c(G$timenum[1], as.numeric(G$timenum[nrow(G)]))
      rangeD = c(GPSdf$timenum[1], GPSdf$timenum[nrow(GPSdf)])
      rangeStart = pmax(rangeG[1], rangeD[1])
      rangeEnd = pmin(rangeG[2], rangeD[2])
      # Check that time series overlap
      if (rangeEnd > rangeStart) {
        log_acc = 4
        # Only consider time range where GPS and GGIR time series are available
        GPSdf = GPSdf[which(GPSdf$timenum >= rangeStart & GPSdf$timenum <= rangeEnd),]
        G = G[which(G$timenum >= (rangeStart - 60) & G$timenum <= (rangeEnd + 60)),] # take wider GGIR range to ease interpolation
        
        # Only consider data if at least 30 percent of the recording has valid data
        if (length(which(G$invalidepoch == 0)) / nrow(G) > 0.3) {
          log_acc = 0
          # Interpolate GGIR output time series to match GPS time series
          # Note that when time resolution is different between GGIR output and GPS
          # this only interpolates time series to match resolution, which is not
          # the same as going back to original data and extracting same timestamps
          
          # Linearly interpolate acceleration
          GPSdf$GGIR_ACC = GGIRread::resample(raw = as.matrix(G$ACC), rawTime = G$timenum, time = GPSdf$time, stop = nrow(G), type = 1)
          # Nearest neigbour interpolate other GGIR output columns
          col2impute = c("SleepPeriodTime", "invalidepoch", "guider", "window", "class_id")
          GS = as.data.frame(GGIRread::resample(raw = as.matrix(G[, col2impute]),
                                                rawTime = G$timenum, time = GPSdf$time, stop = nrow(G), type = 2))
          colnames(GS) = paste0("GGIR_", col2impute)
          # turn class_id to factor to integrate the class labels
          GS$GGIR_class_id = cut(x = GS$GGIR_class_id, breaks = c(Legend$class_id - 0.1, 100), labels = Legend$class_name)
          GPSdf = cbind(GPSdf, GS)
        }
      }
    }
  }
  if (log_acc == 1) {
    warning("acc file path is NULL")
  } else if (log_acc == 2) {
    if (!file.exists(GGIR_ts_path)) warning(paste0(GGIR_ts_path, " does not exist"))
    if (!file.exists(GGIR_legend)) warning(paste0(GGIR_legend, " does not exist"))
  } else if (log_acc == 3) {
    warning("Acceleromter data does not overlap with GPS data")
  } else if (log_acc == 4) {
    warning(paste0("Acceleromter data does not come with more than 30% valid",
                   " data during overlapping interval"))
  } else if (log_acc == 0) {
    message("\nGGIR time series successfully merged with GPS time series")
  }
  invisible(list(GPSdf = GPSdf, log_acc = log_acc))
}