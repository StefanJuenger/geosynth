#' Create Census Inhabitants Dataset
#'
#' This (internal) function retrieves 1km grid population data, adds municipality codes (AGS)
#' for available years, and saves the final dataset as an RDS file.
#'
#' @return Saves an RDS file containing census inhabitants data with AGS codes.
#' @noRd
create_census_inhabitants <- function() {

  # Retrieve 1km grid attribute for inhabitants from z11 package
  census_inhabitants <- z22::z22_data("population", as = "sf")

  census_inhabitants <-
    census_inhabitants |>
    dplyr::mutate(
      inspid1km =
        z22::z22_inspire_generate(census_inhabitants, res = "1km")
    ) |>
    dplyr::select(inspid1km, inhabitants = cat_0)

  # Extract available years from municipality data files and add AGS codes
  census_inhabitants <-
    dplyr::bind_cols(
      census_inhabitants,
      c(as.character(2012:2024)) |>
        purrr::map(~{  # Iterate over each year
          year = .x

          # Load municipality shape data
          municipalities_shape <-
            glue::glue(
              "./data-raw/Gemeindegrenzen_{year}_mit_Einwohnerzahl.geojson"
            ) |>
            sf::st_read() |>
            sf::st_transform(3035) |>
            dplyr::rename_with(stringr::str_to_lower)

          # Perform a spatial join to add AGS (municipality codes)
          sf::st_join(
            census_inhabitants,
            municipalities_shape["ags"],
            join = st_nearest_feature
          ) |>
            dplyr::select(dplyr::starts_with("ags")) |>
            dplyr::rename_with(
              ~ paste0("ags_", year), dplyr::starts_with("ags")
              ) |>
            sf::st_drop_geometry()
        })
    )

  usethis::use_data(census_inhabitants, overwrite = TRUE)
}

#' Create Municipalities Inhabitants Dataset
#'
#' This (internal) function processes municipality population data from shapefiles, transforms
#' the spatial data, assigns administrative codes (AGS), classifies population groups,
#' merges with RegioStaR reference data, and saves the results as RDS files.
#'
#' @return Saves RDS files for each year containing processed municipality data.
#' @import readxl
#' @import sf
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import glue
#' @import readr
#' @noRd
create_municipalities_inhabitants <- function () {
  list.files("./data-raw/", pattern = "Einwohnerzahl") |> # Find relevant population data files
    purrr::walk(~{
      year <- stringr::str_extract(.x, "[0-9]{4}") # Extract year from filename

      # Load RegioStaR reference data based on the year
      if (year < 2015) {
        regiostar_data <-
          readxl::read_excel(
            "./data-raw/2024 RegioStaR-Referenzdateien_Mobilthek.xlsx",
            sheet = "ReferenzGebietsstand2015"
          ) |>
          dplyr::rename_with(stringr::str_to_lower)
      } else {
        regiostar_data <-
          readxl::read_excel(
            "./data-raw/2024 RegioStaR-Referenzdateien_Mobilthek.xlsx",
            sheet = glue::glue("ReferenzGebietsstand{year}")
          ) |>
          dplyr::rename_with(stringr::str_to_lower)
      }

      # Prepare RegioStaR data: Rename AGS column, ensure 8-digit codes, and select relevant columns
      regiostar_data <-
        regiostar_data |>
        dplyr::rename(ags = 1) |>
        dplyr::mutate(ags = stringr::str_pad(ags, 8, pad = "0")) |>
        dplyr::select(ags, dplyr::contains("regiostar"))

      # Load shapefile, transform spatial reference, rename columns, and process municipality data
      joined_data <-
        sf::read_sf(glue::glue("./data-raw/", .x)) |>
        sf::st_drop_geometry() |>
        # sf::st_transform(3035) |> # Transform to EPSG:3035 (European LAEA projection)
        dplyr::rename_all(toupper) |> # Convert column names to uppercase
        dplyr::transmute(
          lan = AGS |> stringr::str_sub(1, 2), # Extract state code
          ags = AGS |> stringr::str_sub(1, 8), # Ensure 8-digit municipality code
          inhabitants = EWZ,

          # Classify population into groups (gkpol) based on population size (inhabitants)
          gkpol = dplyr::case_when(
            inhabitants <= 1999 ~ 1,
            inhabitants > 1999 & inhabitants <= 4999 ~ 2,
            inhabitants > 4999 & inhabitants <= 19999 ~ 3,
            inhabitants > 19999 & inhabitants <= 49999 ~ 4,
            inhabitants > 49999 & inhabitants <= 99999 ~ 5,
            inhabitants > 99999 & inhabitants <= 499999 ~ 6,
            inhabitants > 499999 ~ 7,
            TRUE ~ NA
          ),
          inhabitants # Keep total population column
        ) |>
        dplyr::left_join(regiostar_data, by = "ags") |> # Merge with RegioStaR reference data
        dplyr::select(
          lan, ags, gkpol, regiostar7, regiostar17, inhabitants
        )

      file_name <- paste0("mun_", year)

      assign(file_name, joined_data)

      do.call(
        usethis::use_data,
        list(as.name(file_name), compress = "xz", overwrite = TRUE)
      )
    })
}

#' Create Fake Survey Coordinates
#'
#' This (internal) function simulates survey sampling locations by distributing sample points across municipalities
#' and 1km grid cells based on population data, while ensuring a minimum number of samples per area.
#'
#' @param no_sample_points Integer. Total number of sample points to distribute across municipalities. Default is 100.
#' @param sample_size Integer. Total number of survey samples to generate. Default is 3000.
#' @param min_sample Integer. Minimum number of samples per sampled municipality. Default is 5.
#' @param power Numeric. Exponent used in the power-law weighting of population size to control sample distribution. Default is 0.4.
#'
#' @return Saves a dataset named `fake_survey_coordinates` in the package's data directory using `usethis::use_data()`.
#'
#' @examples
#' create_fake_survey_coordinates()
#'
#' @keywords internal
create_fake_survey_coordinates <- function (
    no_sample_points = 100,
    sample_size = 3000,
    min_sample = 5,
    power = 0.4
) {
  # Load municipality shapefile for the year 2024
  municipality_shape <- load_mun_shape(2024)

  # Load population data from packaged qs file
  census_inhabitants <- load_census()

  # Summarize number of municipalities and inhabitants per state (LAN)
  n_sp <-
    municipality_shape |>
    sf::st_drop_geometry() |>
    dplyr::group_by(lan) |>
    dplyr::summarise(
      inhabitants = sum(inhabitants),
      no_mun = dplyr::n()
    ) |>
    dplyr::mutate(
      city_state = ifelse(no_mun <= 2, TRUE, FALSE),
      city_sample_points = ifelse(city_state, no_mun, 0)
    )

  # Calculate how many sample points remain to be distributed among non-city states
  remaining_sample_points <- no_sample_points - sum(n_sp$city_sample_points)

  # Distribute remaining sample points proportional to powered population
  n_sp <-
    n_sp |>
    dplyr::mutate(
      non_city_inhabitants = ifelse(!city_state, inhabitants^power, 0L),
      prop_weight = non_city_inhabitants / sum(non_city_inhabitants),
      proportional = round(prop_weight * remaining_sample_points)
    ) |>
    dplyr::mutate(samples = city_sample_points + proportional) |>
    dplyr::select(lan, inhabitants_sum = inhabitants, samples)

  # Function to sample municipalities within each LAN by population
  sample_mun <- function(x) {
    dplyr::sample_n(x, size = x$samples[1], weight = x$inhabitants)
  }

  # Sample municipalities accordingly
  mun_sample <-
    municipality_shape |>
    dplyr::left_join(n_sp, by = "lan") |>
    dplyr::group_split(lan) |>
    purrr::map_dfr(sample_mun) |>
    dplyr::select(ags, inhabitants)

  # Compute remaining survey samples after assigning minimum to each municipality
  remaining <- sample_size - (min_sample * nrow(mun_sample))

  # Function to generate sampling weights with small random perturbations
  create_weight <- function(inhabitants) {
    weight <- (inhabitants / sum(inhabitants))
    weight <- weight^power + runif(dplyr::n(), -0.05, 0.05)
    weight <- pmax(weight, .01)
    weight <- weight / sum(weight)
  }

  # Assign number of survey samples per municipality
  mun_sample <-
    mun_sample |>
    dplyr::mutate(
      weight = create_weight(inhabitants),
      samples = round(weight * remaining + runif(dplyr::n(), -1, 1)) + min_sample
    )

  # Function to draw 1km grid cells for each municipality sample
  draw_grids <- function(x) {
    census_inhabitants |>
      dplyr::filter(inhabitants >= 3) |>
      dplyr::select(inspid1km, inhabitants, ags_2024) |>
      dplyr::slice_sample(
        n = x$samples, weight_by = inhabitants, replace = TRUE
      ) |>
      dplyr::select(ags = ags_2024)
  }

  # Generate fake survey coordinates by sampling grid cells
  fake_survey_coordinates <-
    mun_sample |>
    dplyr::group_split(dplyr::row_number()) |>
    purrr::map_dfr(draw_grids) |>
    dplyr::mutate(id = 1:dplyr::n(), .before = 1)

  # Save the dataset to package data
  usethis::use_data(fake_survey_coordinates, overwrite = TRUE)
}
