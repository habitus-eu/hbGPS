\name{deriveTrips}
\alias{deriveTrips}
\title{
  Derive trips from GPS time series data
}
\description{
  Derive trips from GPS time series data
}
\usage{
  deriveTrips(df, tz, minTripDur, mintripDist_m)
}
\arguments{
  \item{df}{
    Data.frame with GPS time series
  }
  \item{tz}{
    Character to specify timezone name according to timezone database.
  }
  \item{minTripDur}{
    Numeric minimum trip duration
  }
  \item{mintripDist_m}{
    Numeric minimum trip distance
  }
}
\value{
  Data.frame with GPS time series with added columns to indicate classified trips
  and their characteristics
}
