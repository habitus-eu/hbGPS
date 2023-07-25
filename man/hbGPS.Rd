\name{hbGPS}
\alias{hbGPS}
\title{
  Central pipeline to process GPS and optionally merge in GGIR output
}
\description{
  Central pipeline to process GPS and optionally merge in GGIR output
}
\usage{
  hbGPS(gps_file = NULL,
         outputDir = NULL,
         idloc = 1,
         maxBreakLengthSeconds = 120,
         minTripDur = 60,
         mintripDist_m = 100,
         threshold_snr = 225,
         threshold_snr_ratio = 50,
         tz = "",
         time_format =  "\%d/\%m/\%Y \%H:\%M:\%SO",
         GGIRpath = NULL)
}
\arguments{
  \item{gps_file}{
    Path to GPS file in .csv format
  }
  \item{outputDir}{
    Path to folder where output should be stored
  }
  \item{idloc}{
    Number to indicato id location in file name (same as GGIR)
  }
  \item{maxBreakLengthSeconds}{
    Numeric maximum of trips breaks in seconds.
  }
  \item{minTripDur}{
    Numeric minimum trip duration in seconds.
  }
  \item{mintripDist_m}{
    Numeric minimum trip distance in meters.
  }
  \item{threshold_snr}{
    Numeric threshold for snr (signal to noise ratio)
  }
  \item{threshold_snr_ratio}{
    Numeric threshold for snr-ratio (signal to noise ratio ratio)
  }
  \item{tz}{
    Character to specify timezone name according to timezone database.
  }
  \item{time_format}{
    Character to specify timestamp format.
  }
  \item{GGIRpath}{
    Path to GGIR time series ouput folder (ms5.outraw)
  }

}
\value{
  Data.frame with GPS time series and added classifications
}
