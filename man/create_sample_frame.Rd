% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_sample_frame.R
\name{create_sample_frame}
\alias{create_sample_frame}
\title{Create a Sample Frame for Municipalities}
\usage{
create_sample_frame(
  .data,
  year,
  geo_unit = c("gkpol", "RegioStaR7", "RegioStaR17"),
  inhabitants_threshold = 50000,
  minimum_sample_points = 10
)
}
\arguments{
\item{.data}{A data frame containing georeferenced sample data with `AGS` identifiers.}

\item{year}{The year for which the sample frame is created (used to fetch municipality data).}

\item{geo_unit}{A character vector specifying the geographic unit to be used,
defaulting to `c("gkpol", "RegioStaR7", "RegioStaR17")`.}

\item{inhabitants_threshold}{The minimum population required for inclusion (default: 50,000).}

\item{minimum_sample_points}{The minimum required sample points per geographic unit (default: 10).}
}
\value{
A tibble with municipality-level sample information.
}
\description{
This function constructs a sample frame based on geographic units and population thresholds,
ensuring a minimum number of sampling points. The function enriches the dataset with
municipal information, aggregates data at various geographic levels, and ensures sampling
requirements are met before merging the results into a final frame.
}
\examples{
# Sample data for the year 2022 with a population threshold of 100,000
sampled_data <- function(data, year = 2022, inhabitants_threshold = 100000)

# Sample data using the "RegioStaR17" geographic unit
sampled_data <- function(data, year = 2022, geo_unit = "RegioStaR17")

}
