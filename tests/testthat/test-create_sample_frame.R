# Tests for geosynth::create_sample_frame()

data("fake_survey_coordinates", package = "geosynth")

# ── Output structure ───────────────────────────────────────────────────────────

test_that("create_sample_frame() returns a tibble / data frame", {
  result <-
    create_sample_frame(
      .data    = fake_survey_coordinates,
      year     = "2022",
      geo_unit = "regiostar17"
    )
  expect_s3_class(result, "data.frame")
})

test_that("create_sample_frame() contains expected columns", {
  result <- create_sample_frame(
    .data    = fake_survey_coordinates,
    year     = "2022",
    geo_unit = "regiostar17"
  )
  expected_cols <- c("ags", "lan", "inhabitants", "year")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("create_sample_frame() contains correct number of rows", {
  result <- create_sample_frame(
    .data    = fake_survey_coordinates,
    year     = "2022",
    geo_unit = "regiostar17"
  )

  gem_2022 <- load_mun_shape("2022")

  expect_equal(nrow(result), nrow(gem_2022))
})

test_that("create_sample_frame() sets the year column correctly", {
  result <- create_sample_frame(
    .data    = fake_survey_coordinates,
    year     = "2022",
    geo_unit = "regiostar17"
  )
  expect_true(all(result$year == "2022"))
})

# ── Message behaviour ───────────────────────────────────────────────────────
test_that("create_sample_frame(..., verbose = FALSE) is silent", {
  expect_no_message(
    create_sample_frame(
      .data    = fake_survey_coordinates,
      year     = "2022",
      geo_unit = "regiostar17",
      verbose = FALSE
    )
  )
})

# ── Filtering behaviour ────────────────────────────────────────────────────────

test_that("create_sample_frame() excludes municipalities below inhabitants_threshold", {
  result <- create_sample_frame(
    .data                 = fake_survey_coordinates,
    year                  = "2022",
    geo_unit              = "regiostar17",
    inhabitants_threshold = 100000   # impossibly high → expect empty or minimal frame
  )
  # All retained municipalities must satisfy the threshold or have been
  # rescued via geo_unit adjustment; at a minimum the function should not error.
  gem_2022 <- load_mun_shape("2022")

  ags_evil_cases <-
    dplyr::left_join(
      fake_survey_coordinates,
      dplyr::select(gem_2022, ags, inhabitants)
    ) |>
    dplyr::left_join(
      result |>
        dplyr::select(ags, n_geo_unit)
    ) |>
    dplyr::filter(inhabitants < 100000, n_geo_unit < 10) |>
    dplyr::select(ags) |>
    dplyr::distinct() |>
    dplyr::pull(ags)

  result_ags_evil_cases <-
    result |>
    dplyr::filter(is.na(n_resp_geo_unit)) |>
    dplyr::pull(ags)

  expect_all_false(ags_evil_cases %in% result_ags_evil_cases)
})

test_that("create_sample_frame() retains more rows with a lower inhabitants_threshold", {

  result_low <-
    create_sample_frame(
      .data                 = fake_survey_coordinates,
      year                  = "2022",
      geo_unit              = "regiostar17",
      inhabitants_threshold = 1000,
      minimum_sample_points = 20
    ) |>
    dplyr::filter(!is.na(n_resp_geo_unit))

  result_high <-
    create_sample_frame(
      .data                 = fake_survey_coordinates,
      year                  = "2022",
      geo_unit              = "regiostar17",
      inhabitants_threshold = 500000,
      minimum_sample_points = 20
    ) |>
    dplyr::filter(!is.na(n_resp_geo_unit))

  expect_gte(nrow(result_low), nrow(result_high))
})

# ── geo_unit argument ──────────────────────────────────────────────────────────

test_that("create_sample_frame() works with geo_unit = 'regiostar7'", {
  expect_no_error(
    create_sample_frame(
      .data    = fake_survey_coordinates,
      year     = "2022",
      geo_unit = "regiostar7"
    )
  )
})

test_that("create_sample_frame() works with geo_unit = 'gkpol'", {
  expect_no_error(
    create_sample_frame(
      .data    = fake_survey_coordinates,
      year     = "2022",
      geo_unit = "gkpol"
    )
  )
})

test_that("create_sample_frame() rejects an invalid geo_unit", {
  expect_error(
    create_sample_frame(
      .data    = fake_survey_coordinates,
      year     = "2022",
      geo_unit = "not_a_valid_unit"
    )
  )
})

# ── Input validation ───────────────────────────────────────────────────────────

test_that("create_sample_frame() errors informatively on missing .data", {
  expect_error(
    create_sample_frame(year = "2022", geo_unit = "regiostar17")
  )
})

test_that("create_sample_frame() errors informatively on missing year", {
  expect_error(
    create_sample_frame(
      .data    = fake_survey_coordinates,
      geo_unit = "regiostar17"
    )
  )
})

test_that("create_sample_frame() accepts alternative municipality identifiers", {

  data("fake_survey_coordinates")

  fake_data <- fake_survey_coordinates

  fake_data$municipality_id <- fake_data$ags
  fake_data$ags <- NULL

  result <-
    create_sample_frame(
      .data = fake_data,
      year = "2024",
      mun_id = "municipality_id",
      geo_unit = "regiostar17",
      verbose = TRUE
    )

  expect_s3_class(result, "tbl_df")

  expect_true("year" %in% names(result))
})

test_that("create_sample_frame() derives AGS from geometry", {

  data("fake_survey_coordinates")

  fake_data <- fake_survey_coordinates

  fake_data$ags <- NULL

  result <-
    create_sample_frame(
      .data = fake_data,
      year = "2024",
      geo_unit = "regiostar17",
      verbose = TRUE
    )

  expect_s3_class(result, "tbl_df")

  expect_true("year" %in% names(result))
})

test_that("create_sample_frame() fails without municipality identifiers", {

  fake_data <- data.frame(
    x = 1:10,
    y = 1:10
  )

  expect_error(
    create_sample_frame(
      .data = fake_data,
      year = "2024",
      verbose = FALSE
    ),
    "No municipality identifier available"
  )
})
