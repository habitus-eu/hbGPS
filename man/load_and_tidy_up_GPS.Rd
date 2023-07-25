\name{load_and_tidy_up_GPS}
\alias{load_and_tidy_up_GPS}
\title{
  Load and tidy up GPS data
}
\description{
  Load and tidy up GPS data
}
\usage{
  load_and_tidy_up_GPS(gps_file, idloc = NULL, tz = "")
}
\arguments{
  \item{gps_file}{
    Path to GPS file in .csv format
  }
  \item{idloc}{
    Number to indicato id location in file name (same as GGIR)
  }
  \item{tz}{
    Timezone
  }
}
\value{
  List with a data.frame df that holds the GPS time series
  and ID a scalar to hold the ID of the participant.
}
