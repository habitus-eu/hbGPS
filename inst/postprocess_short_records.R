rm(list = ls())
library(GGIR)


# Specify thresholds and timezone identical to how it was used in GGIR call
desiredtz = ""
threshold.lig = round(100 * (5 / 60), digits = 2)
threshold.mod = round(2500 * (5 / 60), digits = 2)
threshold.vig = round(10000 * (5 / 60), digits = 2)
# Set to GGIR output folder
GGIR_output_folder = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Jae/output_acc"

acc.metric = "NeishabouriCount_x"

#============================================================
# No edits needed from here onward:
metadatadir = GGIR_output_folder
fnames.ms1 = dir(paste0(metadatadir, "/meta/basic"))
fnames.ms2 = dir(paste0(metadatadir, "/meta/ms2.out"))
fnames.ms3 = dir(paste0(metadatadir, "/meta/ms3.out"))
ms5.outraw = "/meta/ms5.outraw"
params_phyact = load_params("phyact")$params_phyact
params_phyact[["threshold.lig"]] = threshold.lig
params_phyact[["threshold.mod"]] = threshold.mod
params_phyact[["threshold.vig"]] = threshold.vig
params_247 = load_params("247")$params_247
params_general = load_params("general")$params_general
params_general[["acc.metric"]] = acc.metric
params_output = load_params("output")$params_output

for (i in 1:length(fnames.ms3)) {
  selp = which(fnames.ms1 == paste0("meta_", fnames.ms3[i]))
  load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
  selp = which(fnames.ms2 == fnames.ms3[i])
  load(paste0(metadatadir, "/meta/ms2.out/", fnames.ms2[selp]))
  load(paste0(metadatadir, "/meta/ms3.out/", fnames.ms3[i]))
  # Initialise ts object:
  ts = g.part5_initialise_ts(IMP, M, params_247, params_general)
  # Classify intensity levels:
  ts$sibdetection = 0
  ts$diur = 0
  out = identify_levels(
    ts = ts,
    TRLi = threshold.lig,
    TRMi = threshold.mod,
    TRVi = threshold.vig,
    ws3 = 10,
    params_phyact = params_phyact
  )
  LEVELS = out$LEVELS
  Lnames = out$Lnames
  outdir = paste0(metadatadir, ms5.outraw, "/", threshold.lig, "_", threshold.mod, "_", threshold.vig)
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  # Create legend file:
  legendfile = paste0(metadatadir, ms5.outraw, "/behavioralcodes", as.Date(Sys.time()), ".csv")
  if (file.exists(legendfile) == FALSE) {
    legendtable = data.frame(class_name = Lnames, class_id = 0:(length(Lnames) - 1), stringsAsFactors = FALSE)
    data.table::fwrite(legendtable, file = legendfile, row.names = FALSE,
                       sep = params_output[["sep_reports"]])
  }
  sibDef = "NA"
  rawlevels_fname =  paste0(metadatadir, ms5.outraw, "/", threshold.lig, "_", threshold.mod, "_", threshold.vig, "/",
                            gsub(pattern = "[.]|rdata|csv|cwa|gt3x|bin",
                                 replacement = "", x = tolower(fnames.ms3[i])),
                            "_", sibDef, ".", params_output[["save_ms5raw_format"]])
  ms5rawlevels = data.frame(date_time = ts$time, class_id = LEVELS,
                            stringsAsFactors = FALSE)
  if (length(grep(pattern = " ", x = ts$time[1])) == 0) {
    ts$timestamp = as.POSIXct(ts$time, tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
    ms5rawlevels$date_time = as.POSIXct(ms5rawlevels$date_time, 
                                        tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
  } else {
    ts$timestamp = ts$time
  }
  # Create numeric time to faciltiate merging
  ts$timenum = as.numeric(ts$timestamp)
  ms5rawlevels$timenum = as.numeric(ms5rawlevels$date_time)
  mdat = merge(ts, ms5rawlevels, by = "timenum")
  names(mdat)[which(names(mdat) == "nonwear")] = "invalidepoch"
  names(mdat)[which(names(mdat) == "diur")] = "SleepPeriodTime"
  mdat = mdat[,-which(names(mdat) == "date_time")]
  # Add invalid day indicator
  mdat$invalid_wakinghours = mdat$invalid_sleepperiod =  mdat$invalid_fullwindow = 100
  wakeup = which(diff(c(mdat$SleepPeriodTime,0)) == -1) + 1 # first epoch of each day
  # round acceleration values to 3 digits to reduce storage space
  mdat$ACC = round(mdat$ACC, digits = 3)
  # round light data to 0 digits to reduce storage space
  if ("lightpeak" %in% names(mdat)) mdat$lightpeak = round(mdat$lightpeak)
  mdat$guider = "unknown"
  mdat = mdat[,-which(names(mdat) %in% c("timestamp","time"))]
  # save to csv file
  data.table::fwrite(mdat, rawlevels_fname, row.names = F, sep = params_output[["sep_reports"]])
  rm(mdat, ts, ms5rawlevels)
}
