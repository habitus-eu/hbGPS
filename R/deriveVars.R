deriveVars = function(D, convertUnit = TRUE) {
  D$deltaElevation = c(NA, D$height_m[2:nrow(D)] - D$height_m[1:(nrow(D) - 1)])
  
  # Distance
  D$lat = as.numeric(D$lat)
  D$lon = as.numeric(D$lon)
  
  if (convertUnit == TRUE) {
    D$lat = units::as_units(D$lat, "degrees")
    D$lat = units::set_units(D$lat, "radians")
    D$lon = units::as_units(D$lon, "degrees")
    D$lon = units::set_units(D$lon, "radians")
  }
  NR = nrow(D)
  lon1 = D$lon[1:(NR - 1)]
  lon2 = D$lon[2:NR]
  lat1 = D$lat[1:(NR - 1)]
  lat2 = D$lat[2:NR]
  distance = geodesicDistance(lat1, lat2, lon1, lon2)
  D$distance_m = c(NA, distance)
  # delta Time
  D$deltaTime = c(NA, diff(as.numeric(D$time)))
  # Speed
  D$speed_ms = D$distance_m / D$deltaTime #m/s
  D$speed_kmh = D$speed_ms * 3.6 #km/h
  # Elevation per distance 
  D$inclination_deg = 0 # default if both distance and deltaElevation are zero
  valid = which(is.na(D$deltaElevation) == FALSE & is.na(D$distance_m) == FALSE & (D$distance_m != 0 | D$deltaElevation != 0))
  D$inclination_deg[valid] = atan(D$deltaElevation[valid] / abs(D$distance_m[valid])) * (180 / pi)
  
  # Bearing and deltaBearing
  D$deltaBearing = D$bearing_deg = NA
  
  lon1_deg = units::set_units(lon1, "degrees")
  lon2_deg = units::set_units(lon2, "degrees")
  lat1_deg = units::set_units(lat1, "degrees")
  lat2_deg = units::set_units(lat2, "degrees")
  
  X = cos(lat2_deg) * sin(lon2_deg - lon1_deg)
  Y = cos(lat1_deg) * sin(lat2_deg) - sin(lat1_deg) * cos(lat2_deg) * cos(lon2_deg - lon1_deg)
  
  D$bearing_deg = c(0, atan2(X, Y) * (180 / pi))
  D$deltaBearing = c(0, abs(diff(D$bearing_deg)))
  above180 = which(D$deltaBearing > 180)
  if (length(above180) > 0) {
    D$deltaBearing[above180] = abs(D$deltaBearing[above180] - 360)
  }
  
  # # change in speed per second
  # D$acc = c(NA, diff(D$speed) / ((D$deltaTime[1:(NR - 1)] + D$deltaTime[2:(NR)]) / 2))
  return(D)
}