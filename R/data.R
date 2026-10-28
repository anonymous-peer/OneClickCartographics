#' Neotropical Bees Occurrence Records in Seasonally Dry Tropical Forests (SDTFs)
#'
#' A curated dataset of 88,919 occurrence records for 1,851 bee species (Hymenoptera: Apoidea)
#' across Neotropical seasonally dry tropical forests (SDTFs).
#'
#' @format A data frame with 88,919 rows and 9 variables:
#' \describe{
#'   \item{countryCode}{Two-letter ISO country code}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Taxonomic genus}
#'   \item{species}{Taxonomic binomial}
#'   \item{year}{Collection or observation year}
#'   \item{source}{Data source}
#'   \item{cite}{Data citation}
#' }
#' @source Anonymous Authors (2026)
"bees"

#' Cuban Green Anoles Curated Occurrence Records
#'
#' A curated dataset containing 1,538 occurrence records for three green anole lizard species
#' endemic to Cuba (\emph{Anolis allisoni}, \emph{Anolis porcatus}, and \emph{Anolis torresfundorai}),
#' including taxonomic type locality designations.
#'
#' @format A data frame with 1,538 rows and 6 variables:
#' \describe{
#'   \item{species}{Taxonomic binomial}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{source}{Occurrence data source}
#'   \item{popmap}{Population identifier code}
#'   \item{type}{Type locality designation (e.g. "type" for holotype)}
#' }
#' @source Anonymous Authors (2026)
"anoles"
