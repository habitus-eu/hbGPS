![GitHub Actions R-CMD-check](https://github.com/habitus-eu/hbGPS/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/habitus-eu/hbGPS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/habitus-eu/hbGPS)

# hbGPS

R package to process GPS data collected in human behaviour with option to merge 
time series derived with the GGIR R package for accelerometer data. The package 
offers a pipeline that does:
- Load GPS csv files (aim is to make this flexible to most common formats)
- Account for timezone in timestamp interpretation
- Signal to noise calculation in GPS data
- Outlier detection and removal for speed and elevation
- Distance and speed calculation
- Indoor/Outdoor detection
- Trip detection where it allows for breaks in trips
- Merging of GGIR time series output data, this allows for flexibility to work
with a wide variety of accelerometer brands and data formats
- User control over key parameters


## Installation

```
install.packages("remotes")
remotes::install_github("wadpac/GGIR")
remotes::install_github("habitus-eu/hbGPS")
```

## Usage

The code below shows an example of how use hbGPS in combination with R package GGIR.


### Process accelerometer data with GGIR

See GGIR documentation for additional details on how to use GGIR:
https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html


#### If you have Raw data


```
library(GGIR)
GGIR(datadir = "F:/path/to/your/data/folder",
     outputdir = "F:/path/to/your/output/folder",
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
```

#### If you have Count data

```
AccThresholds = c(100, 2500, 10000, 15000) * c(5/60) # assumes GGIR's default epoch length of 5 seconds
AccThresholds = round(AccThresholds, digits = 2)

library(GGIR)
GGIR(datadir = "F:/path/to/your/data/folder",
     outputdir = "F:/path/to/your/output/folder",
     dataFormat = "actigraph_csv",
     mode = 1:5,
     overwrite = FALSE,
     do.report = c(2),
     windowsizes = c(1, 900, 3600),
     threshold.in = AccThresholds[1],
     threshold.mod = AccThresholds[2],
     threshold.vig = AccThresholds[3],
     extEpochData_timeformat = "%m/%d/%Y",
     do.neishabouricounts = TRUE,
     acc.metric = "NeishabouriCount_x",
     HASPT.algo = "NotWorn",
     HASIB.algo = "NotWorn",
     boutdur.mvpa = 10, # note that this can be a vector
     boutdur.in = 30, # note that this can be a vector
     boutdur.lig = 10, # note that this can be a vector
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
```


### Process GPS data and integrate GGIR output


```
gps_file = "D:/path/to/gps/data/which/can/be/folder/withfiles/or/singlefile"
outputDir = "F:/path/to/your/output/folder"

# assumption is that GGIR has already been run specify GGIR output folder:
GGIRpath = "F:/path/to/your/GGIR/output/folder/meta/ms5.outraw"

hbGPS(gps_file = gps_file,
      outputDir = outputDir,
      idloc = 2,
      maxBreakLengthSeconds = 120,
      minTripDur = 60,
      mintripDist_m = 100,
      threshold_snr = 225,
      threshold_snr_ratio = 50,
      tz = "Australia/Perth", # timezone database name
      time_format = "%Y/%m/%d %H:%M:%S",
      GGIRpath = GGIRpath,
      outputFormat = "PALMS",
      AccThresholds = AccThresholds) # alternative is "default"
```
The code will store a csv file with a time series for each input gps file.