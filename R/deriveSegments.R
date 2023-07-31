deriveSegments = function(df) {
  ch = which(diff(df$state) != 0)
  Nseg = length(ch) - 1
  Segs = data.frame(
    nr = numeric(Nseg),
    avespeed_kmh = numeric(Nseg),
    p90speed_kmh = numeric(Nseg),
    indoor = numeric(Nseg),
    vehicle = numeric(Nseg),
    duration = numeric(Nseg),
    distance_m = numeric(Nseg),
    state = numeric(Nseg),
    time_t0 = numeric(Nseg),
    time_t1 = numeric(Nseg),
    t0 = numeric(Nseg),
    t1 = numeric(Nseg),
    snr = numeric(Nseg),
    snr_ratio = numeric(Nseg),
    lon_t0 = numeric(Nseg),
    lat_t0 = numeric(Nseg),
    lon_t1 = numeric(Nseg),
    lat_t1 = numeric(Nseg)
  )
  dosnr = ifelse("snr" %in% colnames(df), yes = TRUE, no = FALSE)
  
  for (i in 1:Nseg) {
    Segs$nr[i] = i
    t0 = ch[i] + 1
    t1 = ch[i + 1]
    Segs$avespeed_kmh[i] = mean(df$speed_kmh[t0:t1], na.rm = TRUE)
    # 90th percentile because Carlson et al MSSE 2015
    Segs$p90speed_kmh[i] = stats::quantile(x = df$speed_kmh[t0:t1], probs = c(0.9), na.rm = TRUE)
    Segs$indoor[i] = ifelse(test = length(which(df$indoor[t0:t1] == TRUE)) > (t1 - t0 + 1) / 2,
                            yes = TRUE,
                            no = FALSE)
    Segs$vehicle[i] = ifelse(test = length(which(df$vehicle[t0:t1] == TRUE)) > (t1 - t0 + 1) / 2,
                            yes = TRUE,
                            no = FALSE)
    Segs$duration[i] = sum(df$deltaTime[t0:t1], na.rm = TRUE) / 60 #as.numeric(difftime(time2 = df$time[t0], time1 = df$time[t1], units = "secs"))
    Segs$distance_m[i] = sum(df$distance_m[t0:t1], na.rm = TRUE)
    Segs$state[i] = mean(df$state[t0:t1], na.rm = TRUE)
    Segs$time_t0[i] = as.numeric(df$time[t0])
    Segs$time_t1[i] = as.numeric(df$time[t1])
    Segs$t0[i] = t0
    Segs$t1[i] = t1
    if (dosnr == TRUE) {
      Segs$snr[i] = mean(df$snr[t0:t1], na.rm = TRUE)
      Segs$snr_ratio[i] = mean(df$snr_ratio[t0:t1], na.rm = TRUE)
    }
    Segs$lon_t0[i] = df$lon[t0]
    Segs$lat_t0[i] =  df$lat[t0]
    Segs$lon_t1[i] = df$lon[t1]
    Segs$lat_t1[i] =  df$lat[t1]
  }
  return(Segs)
}