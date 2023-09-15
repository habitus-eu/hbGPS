rm(list = ls())
dirR = dir("D:/Code/GGIR/R", full.names = TRUE)
for (i in dirR) source(i)
# library(GGIR)



GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata/ACC",
     outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/DKtestdata",
     dataFormat = "actigraph_csv",
     mode = c(1:5),
     overwrite = TRUE,
     do.report = c(2, 4, 5),
     windowsizes = c(15, 900, 3600),
     threshold.in = round(100 * (5/60), digits = 2),
     threshold.mod = round(2500 * (5/60), digits = 2),
     threshold.vig = round(10000 * (5/60), digits = 2),
     extEpochData_timeformat = "%m/%d/%Y",
     do.neishabouricounts = TRUE,
     acc.metric = "NeishabouriCount_x",
     HASPT.algo = "NotWorn",
     HASIB.algo = "NotWorn",
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


kkk

GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/acc_playce",
     outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing",
     mode = c(5),
     overwrite = TRUE,
     do.report = c(),
     windowsizes = c(5, 900, 3600),
     includedaycrit = 10,
     includenightcrit = 10,
     part5_agg2_60seconds = TRUE,
     HASPT.algo = "NotWorn",
     HASIB.algo = "NotWorn",
     HASPT.ignore.invalid = FALSE,
     threshold.mod = c(100, 120),
     boutdur.in = c(25, 30),
     ignorenonwear = FALSE,
     save_ms5rawlevels = TRUE,
     save_ms5raw_without_invalid = FALSE
)
GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/gps_playce/41023_B37-20160722.csv",
     outputdir = "D:/Dropbox/Work/sharedfolder/DATA/BohmerRotterdam2023/actigraph",
     dataFormat = "actigraph_csv",
     mode = c(1:5),
     overwrite = FALSE,
     do.report = c(2),
     windowsizes = c(1, 900, 3600),
     threshold.in = round(100 * (5/60), digits = 2),
     threshold.mod = round(2500 * (5/60), digits = 2),
     threshold.vig = round(10000 * (5/60), digits = 2),
     extEpochData_timeformat = "%m/%d/%Y",
     do.neishabouricounts = TRUE,
     acc.metric = "NeishabouriCount_x",
     HASPT.algo = "NotWorn",
     HASIB.algo = "NotWorn",
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