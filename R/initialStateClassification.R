initialStateClassification = function(df, threshold_snr, threshold_snr_ratio) {
  # If signal to noise ratio is available use it to detect indoor/outdoor
  # which we may then use to aid detecting states
  # Calculate SNR
  out = signalNoiseRatio(df)
  df = out$df
  snr_available = out$snr_available
  snr_ratio_available = out$snr_ratio_available
  
  # state 0: Initial indicate of state stationary but further down replaced by state 2
  # state 1: sequence of 3 steps with speed above 1
  # state 2: stationary, see state 0
  df$state = 0
  N = nrow(df)
  speeding = which(df$speed_kmh[1:(N - 2)] >= 1 &
          df$speed_kmh[2:(N - 1)] >= 1 &
          df$speed_kmh[3:N] >= 1)
  speeding = sort(unique(c(speeding, speeding + 1, speeding + 2)))
  df$state[speeding] = 1
  
  # Any point with a gap in time longer than 30 seconds relative
  # to previous point will  be set to state = 0
  # in order to prevent these from being part of trips
  gappedTrips = which(df$state == 1 & df$deltaTime > 30)
  if (length(gappedTrips) > 0) {
    df$state[gappedTrips] = 0
  }
  # Change state 0 to 2
  df$state[which(df$state == 0)] = 2
  df$indoor = FALSE
  df$vehicle = FALSE
  if (snr_ratio_available == TRUE & snr_available == TRUE) {
    left = 1:(nrow(df) - 1)
    right = 2:(nrow(df))

    INDOOR = c(df$snr[left] <= threshold_snr | df$snr_ratio[left] < threshold_snr_ratio)
    # speed above 20 in 2 successive steps
    # and change in bearing_deg between successive steps no larger than 90 degrees
    VEHICLE = c(df$speed_kmh[left] > 20 & df$speed_kmh[right] > 20 &
                  abs(df$deltaBearing[left]) < 90 &
                  abs(df$deltaBearing[right]) < 90)
    indoor = which(INDOOR == TRUE & VEHICLE == FALSE)
    vehicle = which(INDOOR == TRUE & VEHICLE == TRUE)
    if (length(indoor) > 0) {
      df$indoor[indoor] = TRUE
      df$state[indoor] = 2 # turn stationary
    }
    if (length(vehicle) > 0) {
      df$vehicle[vehicle] = TRUE
    }
  }
  # Add extra categories:
  # 1-7 walking or very slow driving vehicle
  # 7-10 walking or running or slow driving vehicle
  # 10-15 running or cycling or slow driving vehicle
  # 15-35 cycling or bus
  # Above 35 car

  df$state[which(df$speed_kmh >= 1 & df$speed_kmh < 7 & df$state == 1)] = 4
  df$state[which(df$speed_kmh >= 7 & df$speed_kmh < 10 & df$state == 1)] = 5
  df$state[which(df$speed_kmh >= 10 & df$speed_kmh < 15 & df$state == 1)] = 6
  df$state[which(df$speed_kmh >= 15 & df$speed_kmh < 35 & df$state == 1)] = 7
  df$state[which(df$speed_kmh >= 35 & df$state == 1)] = 8
  
  return(df)
}