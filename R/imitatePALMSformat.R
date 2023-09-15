imitatePALMSformat = function(D, ID, AccThresholds, verbose = TRUE) {
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
    runNext = FALSE
  } else {
    # if (verbose == TRUE) cat(" => therefore not exported")
    runNext = TRUE
    D = NULL
  }
  invisible(list(D = D, runNext = runNext))
}