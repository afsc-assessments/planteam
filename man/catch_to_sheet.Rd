% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catch_to_sheet.R
\name{catch_to_sheet}
\alias{catch_to_sheet}
\title{Send AKRO catch data to the plan team google sheet}
\usage{
catch_to_sheet(year, data, area)
}
\arguments{
\item{year}{current year}

\item{data}{catch data file}

\item{area}{'goa' or 'bsai' - currently this only works for 'goa'}
}
\description{
Send AKRO catch data to the plan team google sheet
}
\examples{
/dontrun{
catch_to_sheet(year=2023, data=goa_nov_catch, area='goa')
}
}
