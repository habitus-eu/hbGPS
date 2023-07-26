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

