\name{deriveVars}
\alias{deriveVars}
\title{
  Derive trips from GPS time series data
}
\description{
  Derive variables from GPS time series data such as distance, delta time,
  delta elevation, and speed
}
\usage{
  deriveVars(D, convertUnit = TRUE)
}
\arguments{
  \item{D}{
    Data.frame with GPS time series
  }
  \item{convertUnit}{
    Boolean to indicate whether degrees should be converted to radians.
  }
}
\value{
  Data.frame with GPS time series with added columns to represent derived
  variables
}
