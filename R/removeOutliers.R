removeOutliers = function(df) {
  # Remove missing values and outliers
  NAvalues = which(is.na(df$speed_kmh) == TRUE)
  NAvalues = NAvalues[-which(NAvalues == 1)]
  if (length(NAvalues) > 0) df = df[-NAvalues, ]
  
  NR = nrow(df)
  # Detect abnormal values (we call them Outliers here)
  
  # Speeds of 130 that are not preceded or followed by a speed of more than 30
  speedOutliers = which(df$speed_kmh[2:(NR - 1)] > 130 &
                          (df$speed_kmh[1:(NR - 2)] <= 30 | df$speed_kmh[3:NR] <= 30))
  
  # Elevation changes of more than 1000
  elevationOutliers =  which(abs(df$deltaElevation) > 1000)
  
  if (length(speedOutliers) > 0 | length(elevationOutliers) > 0) {
    Outliers = c(speedOutliers, elevationOutliers)
    # Also ignore  also the timepoints before and after because we
    # their values may also be affected
    Outliers = sort(unique(c(Outliers[which(Outliers > 1)] - 1, Outliers, Outliers + 1)))
    
    # Entirely remove rows, because these will just be gaps
    # in time where we have no knowledge of. We are not going to input these
    df = df[-Outliers, ]
    
    # Derive vars again because there are gaps in the data now
    # by which distance, delta time and speed are not longer valid
    df = deriveVars(df, convertUnit = FALSE)
  }
  NAvalues = which(is.na(df$speed_kmh) == TRUE)
  if (length(NAvalues) > 0) df = df[-NAvalues, ]
  return(df)
}