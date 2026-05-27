#' Census 2011 Inhabitants Data
#'
#' @format A data frame/simple feature collection with 361,478 rows and 13 columns:
#' \describe{
#'   \item{inspid1km}{INSPIRE 1 sqkm identifier}
#'   \item{inhabitants}{Inhabitants count}
#'   \item{geometry}{Point geometry column (\code{EPSG:3035})}
#'   \item{ags_2012}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2013}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2014}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2015}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2016}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2017}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2018}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2019}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2020}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2021}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2022}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2023}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#'   \item{ags_2024}{German municipality identifier for the corresponding year ("Allgemeiner Gemeindeschlüssel")}
#' }
#'
#' @source https://www.zensus2011.de
"census_inhabitants"

#' Municipality Level Data
#'
#' @format A data frame with ~11,000 rows (depending on the year) and 6 columns:
#' \describe{
#'   \item{lan}{2-digit German federal state identifier}
#'   \item{ags_YEAR}{8-digit German municipality identifier ("Allgemeiner Gemeindeschlüssel")}
#'   \item{gkpol}{1-digit political municipality size class identifier ("Politische Gemeindegrößenklasse"), see \url{https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/gemeinden/StadtGemeindetyp/StadtGemeindetyp.html}}
#'   \item{regiostar7}{2-digit summarized regional spatial typology statistic identifier ("Regionalstatistische Raumtypologie, Zusammengefasster regionalstatischer Raumtyp"), see \url{https://www.bmv.de/SharedDocs/DE/Artikel/G/regionalstatistische-raumtypologie.html}}
#'   \item{regiostar17}{3- digit regional spatial statistic type identifier "Regionalstatistische Raumtypologie, Regionalstatischer Raumtyp", see \url{https://www.bmv.de/SharedDocs/DE/Artikel/G/regionalstatistische-raumtypologie.html}}
#'   \item{inhabitants}{Inhabitants count in absolute numbers}
#' }
#'
#' @name mun_2012
#'
#' @source https://opendata-esridech.hub.arcgis.com
#' @source https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/15-regiostar.html
#' @source https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/gemeinden/StadtGemeindetyp/StadtGemeindetyp.html
NULL

#' @rdname mun_2012
"mun_2012"

#' @rdname mun_2012
"mun_2013"

#' @rdname mun_2012
"mun_2014"

#' @rdname mun_2012
"mun_2015"

#' @rdname mun_2012
"mun_2016"

#' @rdname mun_2012
"mun_2017"

#' @rdname mun_2012
"mun_2018"

#' @rdname mun_2012
"mun_2019"

#' @rdname mun_2012
"mun_2020"

#' @rdname mun_2012
"mun_2021"

#' @rdname mun_2012
"mun_2022"

#' @rdname mun_2012
"mun_2023"

#' @rdname mun_2012
"mun_2024"

#' Fake Survey Coordinates
#'
#' Fake survey coordinates that are not even synthetic and only serve
#' demonstration purposes
#'
#' @format A data frame/simple feature collection with 2,990 rows and 3 columns:
#' \describe{
#'   \item{id}{Fake respondent identifier}
#'   \item{ags}{8-digit German municipality identifier ("Allgemeiner Gemeindeschlüssel")}
#'   \item{geometry}{Point geometry column (\code{EPSG:3035})}
#' }
#'
#' @source Created using \code{geosynth:::create_fake_survey_coordinates()}
"fake_survey_coordinates"

