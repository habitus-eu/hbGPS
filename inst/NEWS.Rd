\name{NEWS}
\title{News for Package \pkg{GGIRread}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 0.0.6 (release date:??-??-2024)}{
    \itemize{
      \item Simplify mergeGGIR to no longer expect window column, see #20
      \item Allow mergeGGIR to handle both full path to ms5.outraw folder or 
      only the GGIR output folder
    }
}
\section{Changes in version 0.0.5 (release date:05-03-2024)}{
    \itemize{
      \item More formats for GPS satillitate info now supported, see #12
      \item Add documentation for GPS csv file format
    }
}
\section{Changes in version 0.0.4 (release date:28-09-2023)}{
    \itemize{
      \item Handle absence of activity bouts
      \item Automatically append all files at the end of the process
    }
}
\section{Changes in version 0.0.3 (release date:15-09-2023)}{
    \itemize{
      \item Remove exclusion of recordings with less than 30 percent
      valid ACC data in mergeGGIR as it seems a too stringent and hard to justify criteria.
    }
}
\section{Changes in version 0.0.2 (release date:15-09-2023)}{
    \itemize{
      \item Improved handling of missing accelerometer files
    }
}
\section{Changes in version 0.0.1 (release date:08-09-2023)}{
    \itemize{
      \item First release
    }
}
