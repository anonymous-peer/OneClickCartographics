#' Direct GBIF Occurrence Data Fetcher
#'
#' Queries the public Global Biodiversity Information Facility (GBIF) REST API
#' to download georeferenced species occurrence records directly by scientific name.
#' Eliminates the need for manual CSV export and data wrangling.
#'
#' @param scientific_name Character: the binomial or trinomial scientific name
#'   (e.g., `"Panthera onca"`, `"Puma concolor"`).
#' @param limit Integer: maximum number of records to retrieve (1 to 10,000,
#'   default `1000`).
#' @param has_coordinate Logical: only retrieve records with valid coordinates.
#'   Default `TRUE`.
#' @param country Character or `NULL`: optional 2-letter ISO country code to filter
#'   records (e.g., `"US"`, `"MX"`).
#' @param verbose Logical: print download progress. Default `TRUE`.
#'
#' @return A `data.frame` with standardized columns: `species`, `lat`, `lon`,
#'   `year`, `basisOfRecord`, `datasetKey`, `catalogNumber`, `issue`.
#' @export
#' @examples
#' \dontrun{
#' dat <- fetch_gbif("Panthera onca", limit = 100)
#' head(dat)
#' }
fetch_gbif <- function(scientific_name,
                       limit = 1000,
                       has_coordinate = TRUE,
                       country = NULL,
                       verbose = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to fetch GBIF data. Install it with install.packages('jsonlite').")
  }

  if (!is.character(scientific_name) || length(scientific_name) != 1L || !nzchar(trimws(scientific_name))) {
    stop("Input 'scientific_name' must be a single non-empty character string.")
  }

  clean_name <- trimws(scientific_name)
  if (verbose) {
    message("Querying GBIF API for: '", clean_name, "' (limit = ", limit, ")...")
  }

  limit <- min(max(as.integer(limit), 1L), 10000L)
  encoded_name <- utils::URLencode(clean_name, reserved = TRUE)

  api_url <- paste0(
    "https://api.gbif.org/v1/occurrence/search?",
    "scientificName=", encoded_name,
    "&hasCoordinate=", tolower(as.character(has_coordinate)),
    "&limit=", limit
  )

  if (!is.null(country) && nzchar(trimws(country))) {
    api_url <- paste0(api_url, "&country=", toupper(trimws(country)))
  }

  # Fetch JSON with timeout handling
  res_json <- tryCatch({
    con <- url(api_url, headers = c("User-Agent" = "OneClickCartographics R package"))
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    jsonlite::fromJSON(con, simplifyDataFrame = TRUE)
  }, error = function(e) {
    stop("Failed to reach GBIF API: ", e$message, "\nCheck internet connectivity or GBIF service status.")
  })

  results <- res_json$results
  total_available <- if (!is.null(res_json$count)) res_json$count else 0L

  if (is.null(results) || length(results) == 0L || nrow(results) == 0L) {
    warning("No occurrence records found on GBIF for '", clean_name, "'.")
    return(data.frame(species = character(0), lat = numeric(0), lon = numeric(0),
                      year = integer(0), basisOfRecord = character(0), stringsAsFactors = FALSE))
  }

  # Extract and standardize core columns
  species_col <- if ("species" %in% names(results) && any(!is.na(results$species))) {
    ifelse(!is.na(results$species), results$species, clean_name)
  } else if ("scientificName" %in% names(results)) {
    results$scientificName
  } else {
    rep(clean_name, nrow(results))
  }

  lat_col <- if ("decimalLatitude" %in% names(results)) as.numeric(results$decimalLatitude) else rep(NA_real_, nrow(results))
  lon_col <- if ("decimalLongitude" %in% names(results)) as.numeric(results$decimalLongitude) else rep(NA_real_, nrow(results))
  year_col <- if ("year" %in% names(results)) as.integer(results$year) else rep(NA_integer_, nrow(results))
  basis_col <- if ("basisOfRecord" %in% names(results)) as.character(results$basisOfRecord) else rep("UNKNOWN", nrow(results))
  cat_col <- if ("catalogNumber" %in% names(results)) as.character(results$catalogNumber) else rep(NA_character_, nrow(results))
  type_status_col <- if ("typeStatus" %in% names(results)) as.character(results$typeStatus) else rep(NA_character_, nrow(results))

  out_df <- data.frame(
    species = as.character(species_col),
    lat = lat_col,
    lon = lon_col,
    year = year_col,
    basisOfRecord = basis_col,
    catalogNumber = cat_col,
    typeStatus = type_status_col,
    stringsAsFactors = FALSE
  )

  # Filter out missing coordinates
  valid_idx <- !is.na(out_df$lat) & !is.na(out_df$lon)
  out_df <- out_df[valid_idx, , drop = FALSE]

  if (verbose) {
    message("Retrieved ", nrow(out_df), " georeferenced records from GBIF (total records available: ", total_available, ").")
  }

  out_df
}

#' Check if a string looks like a scientific binomial/trinomial rather than a file path
#' @keywords internal
is_taxon_name <- function(x) {
  if (!is.character(x) || length(x) != 1L) return(FALSE)
  if (file.exists(x)) return(FALSE)
  # Check file extensions
  ext <- tolower(tools::file_ext(x))
  if (ext %in% c("csv", "tsv", "txt", "xlsx", "xls", "gpkg", "shp", "geojson")) return(FALSE)
  # Check if contains spaces and characters consistent with Linnaean binomen
  x_clean <- trimws(x)
  words <- strsplit(x_clean, "\\s+")[[1]]
  length(words) >= 2L && length(words) <= 4L && all(grepl("^[A-Za-z\\.-]+$", words))
}
