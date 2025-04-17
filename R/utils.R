#' Create Census Inhabitants Dataset
#'
#' This function retrieves 1km grid population data, adds municipality codes (AGS)
#' for available years, and saves the final dataset as an RDS file.
#'
#' @return Saves an RDS file containing census inhabitants data with AGS codes.
#' @import z11
#' @import syntheticgeosample2
#' @import stringr
#' @import purrr
#' @import dplyr
#' @import readr
#' @import tibble
#' @import glue
#' @noRd
create_census_inhabitants <- function() {

  # Retrieve 1km grid attribute for inhabitants from z11 package
  census_inhabitants <- z11::z11_get_1km_attribute(Einwohner, as_raster = FALSE)

  # Extract available years from municipality data files and add AGS codes
  census_inhabitants <-
    dplyr::bind_cols(
      census_inhabitants,
      system.file("extdata", package = "geosynth") |>  # Get package data path
        list.files(pattern = "Einwohnerzahl") |>  # Find relevant files
        stringr::str_extract_all("[0-9]{4}") |>  # Extract years from filenames
        purrr::map(~{  # Iterate over each year
          municipalities_shape <-
            system.file(
              "extdata",
              glue::glue("Gemeindegrenzen_{.x}_mit_Einwohnerzahl.geojson.rds"),
              package = "geosynth"
            ) |>
            readr::read_rds()  # Load municipality shape data

          # Perform a spatial join to add AGS (municipality codes)
          synthgeo::spl_simple_join(
            census_inhabitants,
            municipalities_shape,
            "AGS"
          ) |>
            tibble::as_tibble_col(glue::glue("AGS_", .x))  # Convert to tibble with year-based column name
        })
    )

  # Save the final dataset as an RDS file
  readr::write_rds(census_inhabitants, "./inst/extdata/census_inhabitants.rds")
}

#' Create Municipalities Inhabitants Dataset
#'
#' This function processes municipality population data from shapefiles, transforms
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
    purrr::map(~{
      year <- stringr::str_extract(.x, "[0-9]{4}") # Extract year from filename

      # Load RegioStaR reference data based on the year
      if (year < 2015) {
        regiostar_data <-
          readxl::read_excel(
            "./data-raw/2022 RegioStaR-Referenzdateien-rev1.xlsx",
            sheet = "ReferenzGebietsstand2015"
          )
      } else {
        regiostar_data <-
          readxl::read_excel(
            "./data-raw/2022 RegioStaR-Referenzdateien-rev1.xlsx",
            sheet = glue::glue("ReferenzGebietsstand{year}")
          )
      }

      # Prepare RegioStaR data: Rename AGS column, ensure 8-digit codes, and select relevant columns
      regiostar_data <-
        regiostar_data |>
        dplyr::rename(AGS = 1) |>
        dplyr::mutate(AGS = stringr::str_pad(AGS, 8, pad = "0")) |>
        dplyr::select(AGS, dplyr::contains("RegioStaR"))

      # Load shapefile, transform spatial reference, rename columns, and process municipality data
      sf::read_sf(glue::glue("./data-raw/", .x)) |>
        sf::st_transform(3035) |> # Transform to EPSG:3035 (European LAEA projection)
        dplyr::rename_all(toupper) |> # Convert column names to uppercase
        dplyr::transmute(
          LAN = AGS |> stringr::str_sub(1, 2), # Extract state code
          RBZ = AGS |> stringr::str_sub(1, 3), # Extract administrative district code
          KRS = AGS |> stringr::str_sub(1, 5), # Extract county code
          AGS = AGS |> stringr::str_sub(1, 8), # Ensure 8-digit municipality code

          # Classify population into groups (gkpol) based on population size (EWZ)
          gkpol = dplyr::case_when(
            EWZ <= 1999 ~ 1,
            EWZ > 1999 & EWZ <= 4999 ~ 2,
            EWZ > 4999 & EWZ <= 19999 ~ 3,
            EWZ > 19999 & EWZ <= 49999 ~ 4,
            EWZ > 49999 & EWZ <= 99999 ~ 5,
            EWZ > 99999 & EWZ <= 499999 ~ 6,
            EWZ > 499999 ~ 7,
            TRUE ~ NA
          ),
          EWZ # Keep total population column
        ) |>
        dplyr::left_join(regiostar_data, by = "AGS") |> # Merge with RegioStaR reference data
        readr::write_rds(paste0("./inst/extdata/", .x, ".rds")) # Save as RDS file
    })
}

#' Extract INSPIRE Grid Coordinates
#'
#' This function extracts X and Y coordinates from INSPIRE grid cell IDs
#' for both 1km and 100m grid resolutions.
#'
#' @param inspire_ids A character vector of INSPIRE grid cell identifiers.
#' @return A tibble with extracted X and Y coordinates.
#' @import stringr
#' @import tibble
#' @export
extract_inspire_coordinates <- function (inspire_ids) {

  # Check if the first INSPIRE ID indicates a 1km grid resolution
  if (stringr::str_detect(inspire_ids[1], "1km")) {
    inspire_coordinates <-
      tibble::tibble(
        X =
          substr(inspire_ids, 10, 13) |>  # Extract X coordinate (4-digit grid reference)
          paste0("500") |>  # Append "500" to center the coordinate in the 1km grid
          as.numeric(),
        Y =
          substr(inspire_ids, 5, 8) |>  # Extract Y coordinate (4-digit grid reference)
          paste0("500") |>  # Append "500" to center the coordinate in the 1km grid
          as.numeric()
      )
  }

  # Check if the first INSPIRE ID indicates a 100m grid resolution
  if (stringr::str_detect(inspire_ids[1], "100m")) {
    inspire_coordinates <-
      tibble::tibble(
        X =
          substr(inspire_ids, 12, 16) |>  # Extract X coordinate (5-digit grid reference)
          paste0("50") |>  # Append "50" to center the coordinate in the 100m grid
          as.numeric(),
        Y =
          substr(inspire_ids, 6, 10) |>  # Extract Y coordinate (5-digit grid reference)
          paste0("50") |>  # Append "50" to center the coordinate in the 100m grid
          as.numeric()
      )
  }

  # Return the extracted coordinates
  inspire_coordinates
}

#' Perform a Spatial Join and Extract Attribute
#'
#' This function performs a spatial join between a dataset (`data`) and a shape file (`shape_file`),
#' extracting a specified attribute from the shape file based on spatial intersection.
#'
#' @param data An `sf` object containing the spatial data to be joined.
#' @param shape_file An `sf` object representing the shape file with the desired attribute.
#' @param attribute The attribute (column) from `shape_file` to be extracted.
#' @param replace_na Optional value to replace `NA` values in the extracted attribute (default: `NULL`).
#' @return A vector containing the extracted attribute values for each feature in `data`.
#' @import sf
#' @import dplyr
#' @import rlang
#' @export
spl_simple_join <- function(data, shape_file, attribute, replace_na = NULL) {

  attribute <- rlang::enquo(attribute) # Capture attribute as a quosure for tidy evaluation

  # Perform a spatial join using intersection
  target_value <-
    data |>
    sf::st_join(
      shape_file,
      join = sf::st_intersects
    ) |>
    sf::st_drop_geometry() |> # Remove spatial geometry after the join
    dplyr::select(!!attribute) |> # Select the specified attribute
    unlist() |> # Convert to a simple vector
    as.vector() # Ensure output is a standard vector

  # Replace NA values with specified replacement value if provided
  if (!is.null(replace_na)) {
    target_value[is.na(target_value)] <- replace_na
  }

  # Return the extracted attribute values
  target_value
}


