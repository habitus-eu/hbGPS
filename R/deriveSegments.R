deriveSegments = function(D) {
  ch = which(diff(D$state) != 0)
  Nseg = length(ch) - 1
  Segs = data.frame(
    nr = numeric(Nseg),
    avespeed_kmh = numeric(Nseg),
    duration = numeric(Nseg),
    distance_m = numeric(Nseg),
    state = numeric(Nseg),
    t0 = numeric(Nseg),
    t1 = numeric(Nseg),
    snr = numeric(Nseg),
    snr_ratio = numeric(Nseg),
    lon_t0 = numeric(Nseg),
    lat_t0 = numeric(Nseg),
    lon_t1 = numeric(Nseg),
    lat_t1 = numeric(Nseg)
  )
  dosnr = ifelse("snr" %in% colnames(D), yes = TRUE, no = FALSE)
  
  for (i in 1:Nseg) {
    Segs$nr[i] = i
    t0 = ch[i] + 1
    t1 = ch[i + 1]
    Segs$avespeed_kmh[i] = mean(D$speed_kmh[t0:t1], na.rm = TRUE)
    Segs$duration[i] = sum(D$deltaTime[t0:t1], na.rm = TRUE) / 60 #as.numeric(difftime(time2 = D$time[t0], time1 = D$time[t1], units = "secs"))
    Segs$distance_m[i] = sum(D$distance_m[t0:t1], na.rm = TRUE)
    Segs$state[i] = mean(D$state[t0:t1], na.rm = TRUE)
    Segs$t0[i] = as.numeric(D$time[t0])
    Segs$t1[i] = as.numeric(D$time[t1])
    if (dosnr == TRUE) {
      Segs$snr[i] = mean(D$snr[t0:t1], na.rm = TRUE)
      Segs$snr_ratio[i] = mean(D$snr_ratio[t0:t1], na.rm = TRUE)
    }
    Segs$lon_t0[i] = D$lon[t0]
    Segs$lat_t0[i] =  D$lat[t0]
    Segs$lon_t1[i] = D$lon[t1]
    Segs$lat_t1[i] =  D$lat[t1]
  }
  return(Segs)
}