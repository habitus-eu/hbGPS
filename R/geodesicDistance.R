geodesicDistance = function(lat1, lat2, lon1, lon2, formula = "hf") {
  # geodesic distance between two points
  
  # lat and lon all assumed to be in radians
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  R = 6367000  # Earth average radius in meters
  
  # Code below is adapted from
  # https://www.r-bloggers.com/2010/11/great-circle-distance-calculations-in-r/
  # by Mario Pineda-Krch
  if (formula == "slc") {
    # Spherical Law of Cosines (slc)
    distance = R * acos(cos(lat1) * cos(lat2) * cos(dlon)) + (sin(lat1) * sin(lat2))
  } else if (formula == "hf") {
    # Haversine formula (hf)
    a = as.numeric(sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2)
    c = 2 * atan2(sqrt(a), sqrt(1 - a)) #note this is same as: c = 2 * asin(min(1, sqrt(a)))
    distance = R * c
  }
  return(distance)
}