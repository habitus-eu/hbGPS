\name{geodesicDistance}
\alias{geodesicDistance}
\title{
  Calculate geodesic distance
}
\description{
  Calculates geodesic distance between two coordinates
}
\usage{
  geodesicDistance(lat1, lat2, lon1, lon2, formula = "hf")
}
\arguments{
  \item{lat1}{
    Numeric latitude for first data point in radians
  }
  \item{lat2}{
    Numeric latitude for second data point in radians
  }
  \item{lon1}{
    Numeric longitude for first data point in radians
  }
  \item{lon2}{
    Numeric longitude for second data point in radians
  }
  \item{formula}{
    Character to indicate formula to be used: "slc" for Spherical Law
    of Cosines or "hf" for Haversine formula.
  }
  
}
\examples{
  library(hbGPS)
  geodesicDistance(lat1 = 0, lat2 = 90, lon1 = 45, lon2 = 45, formula = "hf")
}
