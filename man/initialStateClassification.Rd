\name{initialStateClassification}
\alias{initialStateClassification}
\title{
  Classify behavioural state GPS
}
\description{
  Initial classification of behavioural states in GPS data
}
\usage{
  initialStateClassification(df, threshold_snr, threshold_snr_ratio)
}
\arguments{
  \item{df}{
    Data.frame with GPS time series
  }
  \item{threshold_snr}{
    Numeric threshold for snr (signal to noise ratio)
  }
  \item{threshold_snr_ratio}{
    Numeric threshold for snr-ratio (signal to noise ratio ratio)
  }
}
\value{
  Data.frame with GPS time series with added column to represent initial
  state classification.
}

