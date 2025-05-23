% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_inspire_coordinates}
\alias{extract_inspire_coordinates}
\title{Extract INSPIRE Grid Coordinates}
\usage{
extract_inspire_coordinates(inspire_ids)
}
\arguments{
\item{inspire_ids}{A character vector of INSPIRE grid cell identifiers.}
}
\value{
A tibble with extracted X and Y coordinates.
}
\description{
This function extracts X and Y coordinates from INSPIRE grid cell IDs
for both 1km and 100m grid resolutions.
}
