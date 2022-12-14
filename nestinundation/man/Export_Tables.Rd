% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InspectOutput.R
\name{Export_Tables}
\alias{Export_Tables}
\title{Export tables to Excel}
\usage{
Export_Tables(
  FILEPATH = getwd(),
  FILENAME = paste0(nestID, " Inundation Data.xlsx")
)
}
\arguments{
\item{FILEPATH}{Specify the file path (as a text string) where the Excel file should be placed. If not specified, this defaults to the current working directory.}

\item{FILENAME}{Specify the desired file name including the .xlsx file extension. If not specified, this defaults to "nestID Inundation Data.xlsx"}
}
\value{
Exports summary and per-event tables to separate Excel sheets
}
\description{
Prepares tables for export to Excel for additional reporting or data analysis. Overall inundation summary, individual inundation events, and overall temperature summary details are output to their own sheets. User must separately specify a write function to define the filepath and name.
}
\examples{
Export_Tables()
}
