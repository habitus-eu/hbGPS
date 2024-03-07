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
  expected to have the following column names: "latitude"; "longitude";
  a column name "height(m)"; a column named "speed(km/h)".
  
  Further, timestamps are expected to be available as either: "datetime" column in local timezone,
  "time" and "date" columns in local timezone, "local time" and "local date"
  column, or "utc time" and "utc date" column.
  
  Satellite information:
  A column with at least the character string "sat info " 
  where the valueformat can be "(1-1-1-10);(1-1-1-10)" or "#1-1-1-10;#1-1-1-10" where
  the number values per substring does not have to be 4 but the last value is always assumed
  to reflect SNR. If sat info is not available then the code assumes that there is a sid and snr,
  which when values are fused pairwise reflect the sat info value.
  
  Satellites in view and used is either expected to be provided with column nsat(used/view) or 
  nsat (used/view) where the value format can be
  used (view), used(view), used/view, or used / view. 
  If nsat(used/view) column is not available the code will look for column nsat and sid.
  It then extracts from the sid column the number of satilites in view.
}
\value{
  List with a data.frame df that holds the GPS time series
  and ID a scalar to hold the ID of the participant.
}
