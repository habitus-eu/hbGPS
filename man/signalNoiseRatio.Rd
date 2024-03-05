\name{signalNoiseRatio}
\alias{signalNoiseRatio}
\title{
  Derive signal to noise ratios
}
\description{
  Derive signal to noise ratios from GPS data
}
\usage{
  signalNoiseRatio(df)
}
\arguments{
  \item{df}{
    Data,frane as genated by \link{load_and_tidy_up_GPS}
  }
}
\value{
  List with a data.frame df that holds the GPS time series,
  Boolean snr_available and snr_ratio_available to indicate whether
  the two signal to noise ratios are available.
}
