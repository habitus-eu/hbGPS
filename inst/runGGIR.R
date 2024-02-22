rm(list = ls())
dirR = dir("D:/Code/GGIR/R", full.names = TRUE)
dirR = dirR[grep(pattern = "sysdata.rda", x = dirR, invert = TRUE, value = FALSE)]
for (i in dirR) source(i)

usecase = "playce"

if (usecase == "Jae") { # short recording 
        GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Jae/acc",
             outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Jae",
             dataFormat = "actigraph_csv",
             mode = c(1:3),
             idloc = 6,
             overwrite = TRUE,
             do.parallel = FALSE,
             do.imp = FALSE,
             do.report = c(),
             windowsizes = c(10, 60, 900),
             threshold.lig = round(100 * (5/60), digits = 2),
             threshold.mod = round(2500 * (5/60), digits = 2),
             threshold.vig = round(10000 * (5/60), digits = 2),
             extEpochData_timeformat = "%m/%d/%Y %H:%M:%S",
             do.neishabouricounts = TRUE,
             acc.metric = "NeishabouriCount_x",
             HASPT.algo = "NotWorn",
             HASIB.algo = "NotWorn",
             do.visual = TRUE,
             includedaycrit = 2,
             includenightcrit = 2,
             visualreport = FALSE,
             outliers.only = FALSE,
             save_ms5rawlevels = TRUE,
             ignorenonwear = FALSE,
             HASPT.ignore.invalid = FALSE,
             save_ms5raw_without_invalid = FALSE
        )
        
} else if (usecase == "Teun") {
        
        GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam/acc",
             outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/Teun/Driestam",
             dataFormat = "actigraph_csv",
             mode = c(1:3),
             idloc = 6,
             overwrite = FALSE,
             do.parallel = FALSE,
             do.imp = FALSE,
             do.report = c(2, 4, 5),
             windowsizes = c(10, 900, 3600),
             threshold.lig = round(100 * (5/60), digits = 2),
             threshold.mod = round(2500 * (5/60), digits = 2),
             threshold.vig = round(10000 * (5/60), digits = 2),
             extEpochData_timeformat = "%m/%d/%Y %H:%M:%S",
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
} else if (usecase == "DK") {
        # DK
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
} else if (usecase == "playce") {
        GGIR(datadir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/acc_playce",
             outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing",
             mode = c(1:5),
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
}