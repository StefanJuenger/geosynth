% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{spl_simple_join}
\alias{spl_simple_join}
\title{Perform a Spatial Join and Extract Attribute}
\usage{
spl_simple_join(data, shape_file, attribute, replace_na = NULL)
}
\arguments{
\item{data}{An `sf` object containing the spatial data to be joined.}

\item{shape_file}{An `sf` object representing the shape file with the desired attribute.}

\item{attribute}{The attribute (column) from `shape_file` to be extracted.}

\item{replace_na}{Optional value to replace `NA` values in the extracted attribute (default: `NULL`).}
}
\value{
A vector containing the extracted attribute values for each feature in `data`.
}
\description{
This function performs a spatial join between a dataset (`data`) and a shape file (`shape_file`),
extracting a specified attribute from the shape file based on spatial intersection.
}
