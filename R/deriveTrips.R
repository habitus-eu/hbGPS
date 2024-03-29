deriveTrips = function(df, tz, minTripDur, minTripDist_m) {
  # Derive segments
  Segs = deriveSegments(df)
  
  df$mot = 0
  df$iov = 0
  # Classify per segment the MOT and IOV
  for (i in 1:nrow(Segs)) {
    if (Segs$state[i] > 2) {
      # Mode of transport: vehicle (3), bycicle (2), walking (1)
      # Following speed thresholds are based on Carlson et al MSSE 2015
      # enhanced with OR statement in case majority of segment was classified as vehicle
      mot = ifelse(test = Segs$p90speed_kmh[i] >= 35 | Segs$vehicle[i] == TRUE, yes = 3, no =
                     ifelse(test = Segs$p90speed_kmh[i] >= 10, yes = 2, no = 1))
      # iov: Indoor (1) outdoor (2) vehicle (3)
      # Note that this copied mot classification for vehicle
      iov = ifelse(mot == 3, yes = 3, no =
                     ifelse(Segs$indoor[i] == FALSE, yes = 2, no = 1))
      df$mot[Segs$t0[i]:Segs$t1[i]] = mot
      df$iov[Segs$t0[i]:Segs$t1[i]] = iov
    }
  }
  
  # Derive trips from segments
  Segs$trip = 0 # default, not a trip
  Segs$trip[which(Segs$state > 2)] = 1 # trips (because 1 is indoor and 2 is stationary
  Segs$trip[which(diff(c(0, Segs$trip)) == 1)] = 2 #start of trips
  tripsi = which(Segs$trip > 0)
  Segs$trip[tripsi] = cumsum(Segs$trip[tripsi] - 1)
  dstate = diff(df$state)
  
  # Add trips back to time series:
  tripStart = which(Segs$trip[1:(length(dstate) - 1)] == 0 &
                      Segs$trip[2:length(dstate)] != 0) + 1
  if (Segs$trip[1] != 0) tripStart = c(1, tripStart)
  tripEnd = which(Segs$trip[1:(length(dstate) - 1)] != 0 &
                    Segs$trip[2:length(dstate)] == 0)
  if (Segs$trip[nrow(Segs)] != 0) tripEnd = c(tripEnd, nrow(Segs))
  df$tripMaxTimegGap = df$trip = df$tripDur = 0 #df$tripCumDist_m 
  df$tripDist_m = df$tripAveSpeed_kmh = 0
  df$tripElevation_m =  df$tripEleSpeed_kmh = 0
  df$N = df$tripAveIncl_deg =  df$tripAveAbsIncl_deg = df$tripEucDist_m = 0
  
  df$tripAveIncl_deg = 0 #df$tripmaxIncl_deg = 
  cnt = 1
  for (j in 1:length(tripStart)) {
    s0 = as.POSIXct(Segs$time_t0[tripStart[j]], tz = tz, origin = "1970-01-01")
    s1 = as.POSIXct(Segs$time_t1[tripEnd[j]], tz = tz, origin = "1970-01-01")
    tripInd = which(df$time >= s0 & df$time <= s1)
    # trip duration in minutes
    tripDur = diff(as.numeric(range(df$time[c(tripInd, tripInd[length(tripInd)] + 1)])))
    tripDist_m = sum(abs(df$distance_m[tripInd]))
    maxTimeGap = max(df$deltaTime[tripInd])
    tripIncl = mean(df$inclination_deg[tripInd])
    
    if (tripDur > minTripDur & tripDist_m > minTripDist_m) {
      df$trip[tripInd] = cnt #Segs$trip[tripStart[j]]
      N = length(tripInd)
      df$N[tripInd] = N #Segs$trip[tripStart[j]]
      df$tripDur[tripInd] = tripDur
      df$tripDist_m[tripInd] = tripDist_m
      df$tripEucDist_m[tripInd] = geodesicDistance(
        lat1 = df$lat[tripInd[1]],
        lat2 = df$lat[tripInd[N]],
        lon1 = df$lon[tripInd[1]],
        lon2 = df$lon[tripInd[N]]
      )
      df$tripDur[tripInd] = tripDur
      df$tripAveSpeed_kmh[tripInd] = (tripDist_m / tripDur) * 3.6
      df$tripDist_m[tripInd] = tripDist_m
      # df$tripCumDist_m[tripInd] = cumsum(abs(df$distance_m[tripInd])) # cumulative distance per trip
      tripElevantion = sum(abs(df$deltaElevation[tripInd])) # Total elevation change
      df$tripElevation_m[tripInd] = tripElevantion
      df$tripEleSpeed_kmh[tripInd] =  (tripElevantion / tripDur) * 3.6
      df$tripAveIncl_deg[tripInd] =  mean(df$inclination_deg[tripInd]) # trip average inclination_deg
      df$tripAveAbsIncl_deg[tripInd] =  mean(abs(df$inclination_deg[tripInd])) # trip average absolute inclination_deg
      df$tripMaxTimegGap[tripInd] =  maxTimeGap # cumulative distance per trip
      cnt = cnt + 1
    }
  }
  return(df)
}