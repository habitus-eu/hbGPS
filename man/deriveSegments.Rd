\name{deriveSegments}
\alias{deriveSegments}
\title{
  Extract segments from time series defined as non-zero state 
}
\description{
  Extract segments from time series defined as non-zero state and
  derive segment summary
}
\usage{
  deriveSegments(df)
}
\arguments{
  \item{df}{
    Data.frame with all GPS data
  }
}
\value{
  Segs a data.frame the provides a summary per segment.
}