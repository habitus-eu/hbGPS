signalNoiseRatio = function(df) {
  getSNRratio = function(x) {
    x = as.numeric(unlist(strsplit(x, "[(]|[)]")))
    snr_ratio = (x[1] * 100) / x[2]
    return(snr_ratio)
  }
  
  getSNR = function(x) {
    x = unlist(strsplit(x, ";|-"))
    snr = sum(as.numeric(x[seq(2, length(x), by = 2)]))
    return(snr)
  }
  
  
  snr_available = FALSE
  
  if (all(c("nsatused", "nsatview") %in% colnames(df)) == TRUE) {
    df$snr_ratio = (df$nsatused * 100) / df$nsatview
    snr_available = TRUE
  } else {
    if ("nsat_uv" %in% colnames(df)) {
      df$snr_ratio = unlist(lapply(df[, "nsat_uv"], FUN = getSNRratio))
      snr_available = TRUE
    } else {
      warning("\nsnr_ratio could not be calculated")
    }
  }
  
  snr_ratio_available = FALSE
  
  if ("snrused" %in% colnames(df) == TRUE) {
    df$snr = df$snrused # !!! Check, is this correct? Has SNR already been calculated in the MSSE output file?
    snr_ratio_available = TRUE
  } else {
    if ("satinfo" %in% colnames(df)) {
      df$snr = unlist(lapply(df[, "satinfo"], FUN = getSNR))
      snr_ratio_available = TRUE
    } else {
      warning("\nsnr could not be calculated")
    }
  }
  invisible(list(df = df, snr_available = snr_available, snr_ratio_available = snr_ratio_available))
}