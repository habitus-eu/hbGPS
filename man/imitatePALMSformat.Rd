\name{imitatePALMSformat}
\alias{imitatePALMSformat}
\title{
  Imitation PALMS output format
}
\description{
  Convert hbGPS time series object into an imitation of the PALMS output format.
}
\usage{
  imitatePALMSformat(D, ID, AccThresholds, verbose = TRUE)
}
\arguments{
  \item{D}{
    Data.frame with merged GPS and GGIR time series
  }
  \item{ID}{
    ID of participant to match
  }
  \item{AccThresholds}{
    See \link{hbGPS}
  }
  \item{verbose}{
    See \link{hbGPS}
  }
}
\value{
  Data.frame based on input data.frame D and reformated to match PALMS output format
}
