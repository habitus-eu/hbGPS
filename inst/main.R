rm(list = ls())
graphics.off()

#=============================================
# USER INPUT NEEDED:

# gps_file = "/media/vincent/DATA/Habitus/gps/1.csv"
# tz = "Canada/Mountain" # <= ! adjust for each dataset
configFile = "D:/Code/HabitusGUI/inst/testfiles_hbgps/config_hbgps.csv"
testdata = "NBBB" # playce, BE
# testdata = "JosefFeb2024" # playce, BE
idloc = 6 #2
if (testdata == "playce") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/gps_playce/41023_B37-20160722.csv"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/GPS_R"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/output_acc_playce/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  tz = "Australia/Perth"
} else if (testdata == "BE") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/GPS"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/output_ACC/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  tz = "Europe/Brussels"
  
} else if (testdata == "DK") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata/GPS"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata/output_ACC/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  configFile = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata/config_hbGPS.csv"
  tz = "Europe/Brussels"
  
} else if (testdata == "Teun") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam/gps"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam/output_acc/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  configFile = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam/config_hbGPS.csv"
  tz = "Europe/Amsterdam"
} else if (testdata == "NBBB") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010/GPS"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010/output_Acc/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  configFile = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010/config_hbGPS.csv"
  tz = "Europe/Amsterdam"
  idloc = 2
} else if (testdata == "JosefFeb2024") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/JosefMarch2024/GPS"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/JosefMarch2024"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/JosefMarch2024/output_CSV_COUNTS//meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  tz = "Europe/Brussels"
} else if (testdata == "own") {
  gps_file = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/GPS"
  outputDir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata"
  GGIRpath = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/output_ACC/meta/ms5.outraw"
  time_format = "%Y/%m/%d %H:%M:%S"
  tz = "Europe/Brussels"
}


# gps_file = "/media/vincent/DATA/Habitus/GPS_R/sensecam cycling study for jasper/epoch-level from MSSE paper 2023.6.6.csv"


maxBreakLengthSeconds = 120 # seconds #120
minTripDur = 60 # seconds
threshold_snr = 225
threshold_snr_ratio = 50
mintripDist_m = 100 # meters
# SMOOTH_INDOOR = FALSE
write_shp = FALSE
do_pdf_plot = FALSE
do.mapview = TRUE
visual_inspect_ts = FALSE

folderWithFunctions = "D:/Code/hbGPS/R"


AccThresholds = c(100, 2500, 10000, 15000) * c(5/60) # assumes GGIR's default epoch length of 5 seconds
AccThresholds = round(AccThresholds, digits = 2)


# assumption is that GGIR has already been run
# Specify GGIR output folder




#===========================================
# NO USER INPUT NEEDED FROM HERE ONWARD

for (i in dir(folderWithFunctions, full.names = TRUE)) source(i)
timer0 = Sys.time()
D = hbGPS(gps_file = gps_file,
          GGIRpath = GGIRpath,
          outputDir = outputDir,
          idloc = idloc,
          # maxBreakLengthSeconds = maxBreakLengthSeconds,
          # minTripDur = minTripDur,
          # mintripDist_m = mintripDist_m,
          # threshold_snr = threshold_snr,
          # threshold_snr_ratio = threshold_snr_ratio,
          tz = tz,
          time_format = time_format,
          outputFormat = "PALMS",
          AccThresholds = AccThresholds,
          configFile = configFile)


timer1 = Sys.time()
cat(paste0("\n\nProcessing time: ", round(difftime(time1 = timer1, time2 = timer0, units = "secs"), digits = 2), " seconds\n"))
kkkk
# # SUMMARISE
# aggD = function(D) {
#   # # aggregate segments to summary
#   S1 = aggregate(x = D[, c("speed_kmh", "maxTimegGap", "tripDur")],
#                  by = list(D$trip), FUN = mean)
#   colnames(S1) = c("trip", "speed_kmh", "maxTimegGap_sec", "tripDur_min")
#   S1$tripDur_min = round(S1$tripDur_min / 60, digits = 2)
#   S1$speed_kmh = round(S1$speed_kmh, digits = 2)
#   S2 = aggregate(x = D$distance_m,
#                  by = list(D$trip), FUN = sum)
#   colnames(S2) = c("trip", "distance_km")  
#   S2$distance_km = round(S2$distance_km / 1000, digits = 2)
#   S = merge(S1, S2, by = "trip")
#   return(S)
# }
# print(aggD(D))

#====================================
# PLOT TO INSPECT DATA BEFORE IT IS TRANSFORMED TO SF FORMAT
if (do_pdf_plot == TRUE) {
  pdf_plot(df = D, tz, pdffile, statenames)
}

if (visual_inspect_ts == TRUE) {
  # D$recHour = floor((as.numeric(D$time) - as.numeric(D$time[1])) / 3600)
  D$istrip = 0
  D$istrip[which(D$trip != 0)] = 1
  graphics.off()
  
  tripEdge = which(abs(diff(D$istrip)) == 1) + 1
  CXL = 1.2
  cat("\nPrinting 10 examples of start/end times of trips:")
  for (is in sample(x = 1:length(tripEdge), replace = FALSE, size = 10)) {
    select = (tripEdge[is] - 5):(tripEdge[is] + 5)
    M = D[select, ]
    CX = 1
    CAX = 1
    CL = 1
    # x11()s
    par(mfrow = c(5, 1), mar = c(2, 4, 1, 1), bty = "l",
        cex.lab = CXL, cex = CX, 
        cex.axis = CAX, cex.lab = CL)
    
    plot(x = M$time, y = scale(M$lat),  type = "p", pch = 20,
         col = "red", ylim = c(-2, 2),
         xlab = "time", ylab = "scaled location")
    lines(x = M$time, scale(M$lon),  type = "p", pch = 20, col = "blue")
    abline(v = M$time[6], col = "purple", lwd = 2)
    
    indd = which(M$indoor == TRUE)
    plot(x = M$time[indd], M$istrip[indd], type = "p", pch = 20,
         xlab = "time", ylab = "trip", 
         ylim = c(-0.1, 1.1), xlim = range(M$time), col = "red")
    outd = which(M$indoor == FALSE)
    lines(x = M$time[outd], M$istrip[outd], type = "p", pch = 20)
    abline(v = M$time[6], col = "purple", lwd = 2)
    
    plot(x = M$time, M$speed_kmh,  type = "p", pch = 20,
         xlab = "time", ylab = "speed_kmh")
    abline(v = M$time[6], col = "purple", lwd = 2)
    abline(h = 1, col = "purple", lwd = 2)
    
    plot(x = M$time, M$deltaTime,  type = "p", pch = 20,
         xlab = "time", ylab = "deltaTime")
    abline(v = M$time[6], col = "purple", lwd = 2)
    
    plot(x = M$time, M$state,  type = "p", pch = 20,
         xlab = "time", ylab = "state")
    abline(v = M$time[6], col = "purple", lwd = 2)
    
    readline(prompt = "Press [enter] to continue")
    graphics.off()
  }
}



#====================================
# Turn D into simple features object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
df <- sf::st_as_sf(x = D,
                   coords = c("lon", "lat"),
                   crs = projcrs)



if ("sensecammode" %in% colnames(D)) {
  cat("\n\nRelative to sensecammode:\n")
  print(table(D$statenames, D$sensecammode))
  PercUnlabelled = round(length(which(D$sensecammode == "")) / length(D$sensecammode) * 100, digits = 2)
  cat(paste0("\n  unlabelled ", PercUnlabelled, "%"))
  cat("\n\nRelative to palmsmode:\n")
  print(table(D$statenames, D$palmsmode))
  PercUnlabelled = round(length(which(D$palmsmode == "")) / length(D$palmsmode) * 100, digits = 2)
  cat(paste0("\n  unlabelled ", PercUnlabelled, "%"))
}

if (do.mapview) {
  # Crop Jellybean Child Care Attadale:
  df1 = sf::st_crop(
    df,
    xmin = 115.795,
    xmax = 115.799,
    ymin = -32.035,
    ymax = -32.032)
  # Explore data visually
  library(mapview)
  df2 = df1[which(df1$date == unique(df1$date)[1]),]
  # # mapview(df2, zcol = "speed_kmh")
  mapview(df, zcol = "indoor")
  mapview(df1, zcol = "indoor")
  mapview(df2, zcol = "indoor")
}

# Number of frequency table of unique indoor points
# print(table(as.character(df1$geometry[which(df1$indoor == TRUE)])))

# Write to shape file
if (write_shp == TRUE) {
  library(sf)
  sf::write_sf(df, dsn = shpfile)
}