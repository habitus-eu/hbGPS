signalNoiseRatio = function(df) {
  getSNRratio = function(x) {
    x = as.numeric(unlist(strsplit(x, "[(]|[)]")))
    snr_ratio = (x[1] * 100) / x[2]
    return(snr_ratio)
  }
  
  getSNR = function(x) {
    # x is for example "#14-38;#06-34;#22-31;28-19;#03-37;20-00"
    # or "(14-38);(06-34);(22-31);(28-19);(03-37);(20-00)"
    # which gives Nsatillites of 6 and Ndashes of 6 below
    # this then gives snr_index of 2 below, which means that snr is 
    # the second value of each group.
    x = gsub(pattern = "[(]|[)]", replacement = "", x = x)
    Nsatillites = length(unlist(strsplit(x, ";")))
    Ndashes = length(unlist(strsplit(x, "-"))) - 1
    snr_index = (Ndashes / Nsatillites) + 1
    # now split up the x based on ; and -
    # in the example above this would be:
    # "#14 38 #06 34 #22 31 28 19 #03 37 20 00"
    x = unlist(strsplit(x, ";|-"))
    # take sum of every second value, which in this case is 159
    snr_indeces = seq(snr_index, length(x), by = snr_index)
    snr = sum(as.numeric(x[snr_indeces]))
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