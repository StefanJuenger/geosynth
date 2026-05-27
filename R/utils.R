#' @noRd
load_mun_shape <- function(year) {
  get(paste0("mun_", year))
}

#' @noRd
load_census <- function() {
  get("census_inhabitants")
}


