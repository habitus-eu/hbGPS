\name{removeOutliers}
\alias{removeOutliers}
\title{
  Remove outliers from GPS time series data
}
\description{
  Remove outliers from GPS time series data, where outlier is defined based on
  speed changes and/or elevation changes
}
\usage{
  removeOutliers(df)
}
\arguments{
  \item{df}{
    Data.frame with GPS time series
  }
}
\value{
  Data.frame with GPS time series with outliers removed
}
