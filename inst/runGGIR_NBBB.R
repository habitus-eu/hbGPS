rm(list = ls())
dirR = dir("D:/Code/GGIR/R", full.names = TRUE)
dirR = dirR[grep(pattern = "sys", x = dirR, invert = TRUE)]
for (i in dirR) source(i)
# library(GGIR)


GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010/Acc",
     outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/NBBB2010",
     dataFormat = "actigraph_csv",
     mode = c(1:5),
     overwrite = TRUE,
     do.parallel = TRUE,
     do.report = c(2, 4, 5),
     windowsizes = c(2, 900, 3600),
     threshold.in = round(100 * (5/60), digits = 2),
     threshold.mod = round(2500 * (5/60), digits = 2),
     threshold.vig = round(10000 * (5/60), digits = 2),
     extEpochData_timeformat = "%m-%d-%Y %H:%M:%S",
     do.neishabouricounts = TRUE,
     acc.metric = "NeishabouriCount_x",
     HASPT.algo = "NotWorn",
     HASIB.algo = "NotWorn",
     idloc = 2,
     do.visual = TRUE,
     includedaycrit = 10,
     includenightcrit = 10,
     visualreport = FALSE,
     outliers.only = FALSE,
     save_ms5rawlevels = TRUE,
     ignorenonwear = FALSE,
     HASPT.ignore.invalid = FALSE,
     save_ms5raw_without_invalid = FALSE
)


