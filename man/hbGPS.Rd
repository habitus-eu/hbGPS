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
        GGIRpath = NULL,
        outputDir = NULL,
        configFile = NULL,
        verbose = TRUE,
        ...)
}
\arguments{
  \item{gps_file}{
    Path to GPS file in .csv format
  }
  \item{GGIRpath}{
    Path to GGIR time series ouput folder (ms5.outraw)
  }
  \item{outputDir}{
    Path to folder where output should be stored
  }
  \item{configFile}{
    Character path to csv file with columns argument, value and context
  }
  \item{verbose}{
    Booelean to indicate whether output to console should be verbose.
  }
  \item{...}{
    Any of the parameters used by hbGPS. These are documented in the
    details section. You can provide these objects as argument to function hbGPS.
  }
}
\details{
  The following optional arguments can be provided, which will overrule the argument
  values provided via config file:
  \describe{
    \item{idloc}{
      Numeric (default = 1) to indicato id location in file name (same as GGIR)
    }
    \item{maxBreakLengthSeconds}{
      Numeric (default = 120) maximum of trips breaks in seconds.
    }
    \item{minTripDur}{
      Numeric (default = 60) minimum trip duration in seconds.
    }
    \item{mintripDist_m}{
      Numeric (default = 100) minimum trip distance in meters.
    }
    \item{threshold_snr}{
      Numeric (default = 225) threshold for snr (signal to noise ratio)
    }
    \item{threshold_snr_ratio}{
      Numeric (default = 50) threshold for snr-ratio (signal to noise ratio ratio)
    }
    \item{tz}{
      Character (default = "") to specify timezone name according to timezone database.
    }
    \item{time_format}{
      Character (default = "\%d/\%m/\%Y \%H:\%M:\%SO") to specify timestamp format.
    }
    \item{outputFormat}{
      Character (default = "default") to specify format in which the output 
      should be stored, when set to "PALMS" it will attempt to immitate PALMS output.
    }
    \item{AccThresholds}{
      Numeric vector (default = NULL) with acceleration cut-points to define 
      intensity levels sedentary behaviour, light, moderate, vigorous, and extra vigorougs.
      When working with counts keep in mind that counts are epoch lenght specific.
    }
  }
}
\value{
  Data.frame with GPS time series and added classifications
}
