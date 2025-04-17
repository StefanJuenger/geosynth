
# geosynth

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

This R package provides tools to create synthetic versions of geospatial
survey data that mimic the spatial structure of the original dataset
without exposing confidential coordinates. It is designed to support
spatial linking workflows–such as joining survey data to contextual
geographic information–by ensuring the synthetic data produces results
that closely resemble the original data. This enables testing,
prototyping, and methodological development without accessing restricted
location data.

## Installation

You can install the development version of `geosynth` like so:

``` r
remotes::install_github("StefanJuenger/geosynth")
```

## In a nutshell

At the heart of the package are two functions:
`geosynth::create_sample_frame()` constructs a sample frame based on
municipalities and population thresholds while ensuring a minimum number
of sampling points; `geosynth::draw_sample()` selects a sample of
INSPIRE grid cells from a pre-defined sample frame, considering
population data for weighting with built-in census data. Additionally,
if one has access to the original geocoordinates and not only the
municipality information, `geosynth::shuffle_min_distance()` ensures
shuffling rows in the synthetic dataset so that each point is at least a
pre-defined threshold of `x` kilometers away from its corresponding
point in the original dataset.
