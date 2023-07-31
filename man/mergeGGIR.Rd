\name{mergeGGIR}
\alias{mergeGGIR}
\title{
  Merge GGIR data
}
\description{
  Merge GGIR data into GPS data, interpolate towards GPS timestamps if
  timestamps do not allign.
}
\usage{
  mergeGGIR(GGIRpath, GPSdf, ID, verbose)
}
\arguments{
  \item{GGIRpath}{
    Path to GGIR time series ouput folder (ms5.outraw)
  }
  \item{GPSdf}{
    Data.frame with GPS time series
  }
  \item{ID}{
    ID of participant to match
  }
  \item{verbose}{
    See \link{hbGPS}
  }
}
\value{
  List with a data.frame df that holds the GPS time series and
  GGIR time series merged into it and acc_log with code
  to clarify merge status.
}
