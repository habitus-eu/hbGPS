rm(list = ls())
library(GGIR)

# Specify path to folder create by GGIR:
GGIR_output_folder = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Jae/output_acc"

# Specify arguments identical to how they were set for GGIR:
desiredtz = "America/New_York"
threshold.lig = round(100 * (5 / 60), digits = 2)
threshold.mod = round(2500 * (5 / 60), digits = 2)
threshold.vig = round(10000 * (5 / 60), digits = 2)
acc.metric = "NeishabouriCount_x"
part5_agg2_60seconds = TRUE # Set to TRUE to aggregate data per 1 minute, alternatively set to FALSE

#============================================================
# No edits needed from here onward:
metadatadir = GGIR_output_folder
fnames.ms1 = dir(paste0(metadatadir, "/meta/basic"))
fnames.ms2 = dir(paste0(metadatadir, "/meta/ms2.out"))
ms5.outraw = "/meta/ms5.outraw"
params_phyact = load_params("phyact")$params_phyact
params_phyact[["threshold.lig"]] = threshold.lig
params_phyact[["threshold.mod"]] = threshold.mod
params_phyact[["threshold.vig"]] = threshold.vig
params_247 = load_params("247")$params_247
params_general = load_params("general")$params_general
params_general[["acc.metric"]] = acc.metric
params_general[["desiredtz"]] = desiredtz
params_general[["part5_agg2_60seconds"]] = part5_agg2_60seconds
params_output = load_params("output")$params_output
for (i in 1:length(fnames.ms2)) {
  selp = which(fnames.ms1 == paste0("meta_", fnames.ms2[i]))
  load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
  load(paste0(metadatadir, "/meta/ms2.out/", fnames.ms2[i]))
  # Initialise ts object:
  ts = g.part5_initialise_ts(IMP, M, params_247, params_general)
  # Classify intensity levels:
  ts$sibdetection = 0
  ts$diur = 0
  # Aggregate per minute:
  ws3new = params_general[["windowsizes"]][1]
  ts$time = iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])
  if (params_general[["part5_agg2_60seconds"]] == TRUE) { # Optionally aggregate to 1 minute epoch:
    ts$time_num = floor(as.numeric(ts$time) / 60) * 60
    # only include angle if angle is present
    ts = aggregate(ts[,c("ACC","sibdetection", "diur", "nonwear")],
                     by = list(ts$time_num), FUN = function(x) mean(x))
    ts$sibdetection = round(ts$sibdetection)
    ts$diur = round(ts$diur)
    ts$nonwear = round(ts$nonwear)
    names(ts)[1] = "time"
    # # convert back to iso8601 format
    ts$time = as.POSIXct(ts$time, origin = "1970-1-1", tz = params_general[["desiredtz"]])
    ws3new = 60 # change because below it is used to decide how many epochs are there in
  }
  # Classify levels
  out = identify_levels(
    ts = ts,
    TRLi = threshold.lig,
    TRMi = threshold.mod,
    TRVi = threshold.vig,
    ws3 = ws3new,
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
                                 replacement = "", x = tolower(fnames.ms2[i])),
                            "_", sibDef, ".", params_output[["save_ms5raw_format"]])
  ms5rawlevels = data.frame(date_time = ts$time, class_id = LEVELS,
                            stringsAsFactors = FALSE)
  if (length(grep(pattern = " ", x = ts$time[1])) == 0) {
    ts$timestamp = as.POSIXct(ts$time, tz = params_general[["desiredtz"]], format = "%Y-%m-%dT%H:%M:%S%z")
    ms5rawlevels$date_time = as.POSIXct(ms5rawlevels$date_time, 
                                        tz = params_general[["desiredtz"]], format = "%Y-%m-%dT%H:%M:%S%z")
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
