#' Academic Journal Figure Presets
#'
#' Automatically configures figure dimensions (width, height, resolution/DPI)
#' to conform to the formatting guidelines of leading biodiversity, taxonomic,
#' and scientific journals.
#'
#' Supported journals:
#' - `"zootaxa"`: 84 mm (1-col) or 175 mm (2-col), 300 DPI.
#' - `"pensoft"` (ZooKeys, PhytoKeys, Biodiversity Data Journal): 80 mm (1-col), 120 mm (1.5-col), 166 mm (2-col), 300 DPI.
#' - `"nature"` (Nature, Nature Communications, Scientific Reports): 89 mm (1-col), 183 mm (2-col), 300 DPI.
#' - `"plos"` (PLOS ONE, PLOS Biology): 83 mm (1-col), 173 mm (2-col), 300 DPI.
#' - `"elsevier"`: 90 mm (1-col), 140 mm (1.5-col), 190 mm (2-col), 300 DPI.
#' - `"wiley"` (Journal of Biogeography, Diversity and Distributions): 80 mm (1-col), 168 mm (2-col), 300 DPI.
#' - `"systematic_biology"`: 84 mm (1-col), 174 mm (2-col), 300 DPI.
#'
#' @param journal Character: journal name or preset key. Case-insensitive.
#' @param columns Integer: number of columns for the figure: `1` (single column),
#'   `1.5` (intermediate), or `2` (full page width, default).
#' @param aspect_ratio Numeric: ratio of height to width (default `0.75` = 4:3).
#' @param dpi Numeric or `NULL`: override resolution (default `600`, with `300` available for lower resolution).
#'
#' @return A named list with:
#'   - `width`: width in inches.
#'   - `height`: height in inches.
#'   - `dpi`: resolution in dots per inch.
#'   - `journal_name`: formatted display name.
#'   - `column_spec`: character describing column layout.
#' @export
#' @examples
#' dims <- resolve_journal("zootaxa", columns = 2)
#' dims$width # ~6.89 inches (175 mm)
resolve_journal <- function(journal = NULL,
                            columns = 2,
                            aspect_ratio = 0.75,
                            dpi = NULL) {
  if (is.null(journal) || !is.character(journal) || length(journal) != 1L) {
    return(NULL)
  }

  j_clean <- tolower(trimws(journal))

  # Dimensions in millimeters [1-col, 1.5-col, 2-col]
  presets <- list(
    zootaxa = list(
      name = "Zootaxa (Magnolia Press)",
      widths_mm = c("1" = 84, "1.5" = 120, "2" = 175),
      dpi = 600
    ),
    pensoft = list(
      name = "Pensoft (ZooKeys / PhytoKeys / BDJ)",
      widths_mm = c("1" = 80, "1.5" = 120, "2" = 166),
      dpi = 600
    ),
    zookeys = list(
      name = "ZooKeys (Pensoft)",
      widths_mm = c("1" = 80, "1.5" = 120, "2" = 166),
      dpi = 600
    ),
    phytokeys = list(
      name = "PhytoKeys (Pensoft)",
      widths_mm = c("1" = 80, "1.5" = 120, "2" = 166),
      dpi = 600
    ),
    nature = list(
      name = "Nature / Nature Portfolio / Sci Rep",
      widths_mm = c("1" = 89, "1.5" = 138, "2" = 183),
      dpi = 600
    ),
    plos = list(
      name = "PLOS (PLOS ONE / PLOS Biology)",
      widths_mm = c("1" = 83, "1.5" = 125, "2" = 173),
      dpi = 600
    ),
    elsevier = list(
      name = "Elsevier Journals",
      widths_mm = c("1" = 90, "1.5" = 140, "2" = 190),
      dpi = 600
    ),
    wiley = list(
      name = "Wiley (Methods in Ecol. & Evol. / J. Biogeography)",
      widths_mm = c("1" = 85, "1.5" = 125, "2" = 175),
      dpi = 600
    ),
    mee = list(
      name = "Methods in Ecology and Evolution (BES / Wiley)",
      widths_mm = c("1" = 85, "1.5" = 125, "2" = 175),
      dpi = 600
    ),
    systematic_biology = list(
      name = "Systematic Biology (Oxford Univ Press)",
      widths_mm = c("1" = 84, "1.5" = 125, "2" = 174),
      dpi = 600
    )
  )

  # Check aliases
  if (j_clean %in% c("bdj", "biodiversity data journal")) j_clean <- "pensoft"
  if (j_clean %in% c("scirep", "scientific reports")) j_clean <- "nature"
  if (j_clean %in% c("plosone", "plos_one")) j_clean <- "plos"
  if (j_clean %in% c("mee", "methods in ecology and evolution", "bes")) j_clean <- "wiley"

  spec <- presets[[j_clean]]
  if (is.null(spec)) {
    warning("Unrecognized journal preset '", journal, "'. Using standard 10x8 inch layout.")
    return(NULL)
  }

  col_key <- as.character(columns)
  if (!col_key %in% names(spec$widths_mm)) {
    col_key <- "2"
  }

  width_mm <- spec$widths_mm[[col_key]]
  width_in <- round(width_mm / 25.4, 2)
  height_in <- round(width_in * aspect_ratio, 2)
  final_dpi <- if (!is.null(dpi)) as.integer(dpi) else spec$dpi

  list(
    width = width_in,
    height = height_in,
    dpi = final_dpi,
    width_mm = width_mm,
    journal_name = spec$name,
    column_spec = paste0(col_key, "-column (", width_mm, " mm / ", width_in, " in)")
  )
}
