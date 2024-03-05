\name{load_and_tidy_up_GPS}
\alias{load_and_tidy_up_GPS}
\title{
  Load and tidy up GPS data
}
\description{
  Load and tidy up GPS data
}
\usage{
  load_and_tidy_up_GPS(gps_file, idloc = NULL, tz = "",
  time_format = "\%d/\%m/\%Y \%H:\%M:\%SO")
}
\arguments{
  \item{gps_file}{
    Path to GPS file in .csv format, see details for format.
  }
  \item{idloc}{
    Number to indicato id location in file name (same as GGIR)
  }
  \item{tz}{
    Timezone
  }
  \item{time_format}{
    See \link{hbGPS}
  }
}
\details{
  The code ignores letter case when reading column names. The GPS csv file is 
  expected to have the following column names: latitude or lat; longitude or lon;
  nat(used/view) or nat (used/view) where the value format can be
  3 (2), 3(2), 3/2, 3 / 2; a column with at least the character string "sat info " 
  where the valueformat can be "(1-1-1-10);(1-1-1-10)" or "#1-1-1-10;#1-1-1-10" where
  the number values per substring does not have to be 4 but the last value is always assumed
  to reflect SNR; a column name height(m); a column named speed(km/h); 
  a column local data or just date, and; a column local time or just time.
}
\value{
  List with a data.frame df that holds the GPS time series
  and ID a scalar to hold the ID of the participant.
}
