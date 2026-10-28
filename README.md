# OneClickCartographics <img src="man/figures/logo.png" align="right" height="139" alt="OneClickCartographics logo" />

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)]()
[![Package Version](https://img.shields.io/badge/version-1.0.0-informational.svg)]()
[![CRAN status](https://www.r-pkg.org/badges/version/OneClickCartographics)](https://cran.r-project.org/package=OneClickCartographics)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/OneClickCartographics)](https://cran.r-project.org/package=OneClickCartographics)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/OneClickCartographics)](https://cran.r-project.org/package=OneClickCartographics)

> **Automated, Publication-Grade Biodiversity Distribution Maps and Conservation Cartography in a Single Line of Code.**

`OneClickCartographics` empowers taxonomists, evolutionary biologists, ecologists, and conservation scientists to produce publication-grade, mathematically rigorous cartography directly in R without requiring commercial GIS software (e.g., Esri ArcGIS) or writing 80–150+ lines of intricate spatial boilerplate. 

From raw locality coordinates or user-defined scientific names, the package automates coordinate quality cleaning, dynamic 4-tier projection selection, topologically intelligent habitat-constrained IUCN Red List Criterion B evaluations (EOO/AOO), taxonomic type specimen highlighting, WDPA protected area reserve occupancy, dual-metric equal-area stepped grids, continuous 3D topographic relief, and journal-calibrated vector exports (PDF/PNG/GIS), paired with an automated HTML reproducibility report and copy-paste Materials & Methods prose with full BibTeX citations.

---

## Authors & Maintainers

- **Anonymous Author** ([anonymous@peer-review.org](mailto:anonymous@peer-review.org)) — Lead Author, Creator & Maintainer 
- **Anonymous Co-Author** ([anonymous@peer-review.org](mailto:anonymous@peer-review.org)) — Co-creator & Co-maintainer 

---

## Table of Contents

- [What OneClickCartographics Does](#what-one-clickcartographics-does)
- [Installation](#installation)
- [Input Data Format](#input-data-format)
- [Bundled Practice Datasets](#bundled-practice-datasets)
- [Implementation](#implementation)
  - [Quick Start Example](#quick-start-example)
  - [Full Master Example (All Arguments Commented)](#full-master-example-all-arguments-commented)
- [Description of run_map() Arguments](#description-of-run_map-arguments)
- [Core Analytical Capabilities](#core-analytical-capabilities)
  - [1. Zero-Configuration Minimalist Maps & Boundary Layers](#1-zero-configuration-minimalist-maps--boundary-layers)
  - [2. Direct GBIF Taxonomic Retrieval](#2-direct-gbif-taxonomic-retrieval)
  - [3. Automated Pre-Flight Quality Control & Coordinate Cleaning](#3-automated-pre-flight-quality-control--coordinate-cleaning)
  - [4. Topologically Intelligent IUCN Criterion B (EOO & AOO)](#4-topologically-intelligent-iucn-criterion-b-eoo--aoo)
  - [5. Taxonomic Type Specimen Highlighting](#5-taxonomic-type-specimen-highlighting)
  - [6. Protected Area Representation & Reserve Occupancy Analysis](#6-protected-area-representation--reserve-occupancy-analysis)
  - [7. Dual-Metric Equal-Area Stepped Grids (Richness & Density)](#7-dual-metric-equal-area-stepped-grids-richness--density)
  - [8. Continuous Topographic Elevation, 3D Hillshade & Marine Bathymetry](#8-continuous-topographic-elevation-3d-hillshade--marine-bathymetry)
  - [9. Multi-Species Comparative Plates (Faceted Layouts)](#9-multi-species-comparative-plates-faceted-layouts)
  - [10. Journal Dimension Presets](#10-journal-dimension-presets)
  - [11. Modular Cartographic Builder & Selective Post-Hoc Saving](#11-modular-cartographic-builder--selective-post-hoc-saving)
  - [12. Interactive Leaflet Web Cartography](#12-interactive-leaflet-web-cartography)
  - [13. Automated Reproducible HTML Report & Audit Trail](#13-automated-reproducible-html-report--audit-trail)
- [Journal Specifications Table](#journal-specifications-table)
- [Citation](#citation)
- [License](#license)

---

## What OneClickCartographics Does

- **Loads & validates occurrence tables or queries GBIF in real time**  
  Accepts raw CSV, TSV, or Excel spreadsheets, or fetches clean georeferenced records directly from GBIF by taxonomic binomial.
  - `00_cleaned_localities.csv`

- **Automated pre-flight quality control and coordinate curation**  
  Flags and drops $(0,0)$ "Null Island" artifacts, latitude/longitude transpositions, identical placeholder coordinates, and out-of-bounds coordinates.
  - Audit log embedded in `00_map_report.html`

- **Dynamic four-tier projection engine**  
  Analyzes geographic bounding extents to automatically select optimal localized UTM zones or continental equal-area projections (LAEA/Albers), while resolving antimeridian crossing discontinuities without manual math.

- **Generates publication-standard minimalist basemaps**  
  Renders clean, distraction-free distribution maps by default, with optional country, state/province, coastline, lake, and river overlays.
  - `01_simple_map.png` / `01_simple_map.pdf`
  - `02_simple_map_with_points.png` / `02_simple_map_with_points.pdf`

- **Integrative taxonomic type specimen highlighting**  
  Differentiates primary name-bearing types (holotypes, syntypes, lectotypes, neotypes) with prominent diamond markers matching taxon coloration, ensuring type localities are never obscured by non-type clusters.
  - `01_simple_map_with_points_type_highlight.png`

- **Topologically intelligent IUCN Red List Criterion B evaluation**  
  Calculates Extent of Occurrence (EOO, $\text{km}^2$) and Area of Occupancy (AOO, $\text{km}^2$, $2 \times 2\text{ km}$ cells). Eliminates spurious open ocean or dry land from raw convex hulls (`iucn_medium = "land"` or `"water"`), preventing false non-threatened Red List categorizations.
  - `07_iucn_assessment_clipped.png` / `07_iucn_assessment_clipped.pdf`
  - `07_iucn_metrics.csv`

- **Automated protected area network representation and reserve occupancy**  
  Directly interfaces with the World Database on Protected Areas (WDPA; UNEP-WCMC & IUCN, 2024) via `wdpar`, cropping reserves matching the map frame and computing overall and per-species occurrence representation inside vs. outside reserves, and reserve occupancy metrics.
  - `06_protected_areas.png` / `06_protected_areas.pdf`
  - `protected_areas_counts.csv`
  - `protected_areas_summary.csv`

- **Dual-metric equal-area stepped grids (Species Richness & Record Density)**  
  Aggregates occurrences into equal-area cells (10–100 km) using an accessible white-to-red stepped gradient with 1-pixel continuous contours. Quantifies occupied cells while excluding uninformative empty bounding cells.
  - `grid_richness.png` / `grid_density.png`
  - `grid_richness_summary.csv`
  - `grid_record_density_summary.csv`
  - `grid_cell_counts.csv`

- **Continuous topographic elevation, 3D terrain hillshading & ocean bathymetry**  
  Downloads SRTM land elevation and GEBCO ocean bathymetry via `elevatr`, rendering continuous hypsometric gradients from marine trenches (−8,216 m) to alpine summits (+1,974 m) with custom elevation limits.
  - `05_elevation_bathymetry.png` / `05_elevation_bathymetry.pdf`

- **Publisher-calibrated journal presets and vector GIS exports**  
  Auto-scales dimensions, typography, and line weights to exact publisher specifications (*Nature*, *Wiley*, *Elsevier*, *PLOS*, *Pensoft*, *Zootaxa*) and outputs interoperable vector GIS formats.
  - `layers.gpkg` (GeoPackage vector layers)
  - `localities.geojson` (GeoJSON spatial points)

- **Interactive web cartography**  
  Generates interactive Leaflet maps with rich hover tooltips, measurement tools, and layer toggles.
  - `interactive_map.html`

- **Automated HTML reproducibility report and audit trail**  
  Produces a self-contained HTML audit report (Orangutan-R style) featuring map previews, parameter specifications, ready-to-paste Materials & Methods prose, and BibTeX citations.
  - `00_map_report.html`

---

## Installation

### Stable Release (CRAN)

Install the stable release from CRAN:

```r
install.packages("OneClickCartographics")
```

### Development Version (GitHub)

Install the latest development version directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("anonymous-peer/OneClickCartographics")
```

### Optional Dependencies

For elevation terrain rasters, protected area queries, or interactive Leaflet mapping:

```r
install.packages(c("elevatr", "terra", "wdpar", "leaflet", "htmlwidgets", "patchwork"))
```

---

## Input Data Format

`OneClickCartographics` is engineered to accept standard biodiversity tables with flexible, case-insensitive column headers. It automatically recognizes any of the following common naming conventions:

- **Species**: `species`, `taxon`, `scientificName`, `taxa`, `name`
- **Latitude**: `latitude`, `lat`, `decimalLatitude`, `y`, `Latitude`
- **Longitude**: `longitude`, `lon`, `long`, `decimalLongitude`, `x`, `Longitude`
- **Type Status (Optional)**: `type`, `typeStatus`, `type_status`, `typestatus` (values: `"type"`, `"holotype"`, `"syntype"`, `"lectotype"`, `"neotype"`)

### Example Occurrence Table

| species | latitude | longitude | type | popmap | year |
| :--- | :--- | :--- | :--- | :--- | :--- |
| *Anolis allisoni* | 21.8631 | -84.9508 | | West_Cuba | 2013 |
| *Anolis allisoni* | 21.9027 | -84.9350 | | West_Cuba | 2013 |
| *Anolis porcatus* | 23.1167 | -82.3833 | | Havana | 2018 |
| *Anolis porcatus* | 22.9840 | -82.1520 | | Havana | 2021 |
| *Anolis torresfundorai* | 20.6272 | -74.9536 | holotype | Moa | 2024 |
| *Anolis torresfundorai* | 20.6550 | -74.9120 | | Moa | 2024 |

*Note: Any additional metadata columns (e.g., `popmap`, `collector`, `year`, `countryCode`) are preserved throughout the analytical pipeline.*

---

## Bundled Practice Datasets

The package includes two empirical datasets from our published studies, loaded automatically via `LazyData: true`:

### 1. Caribbean Green Anoles (`anoles`)
- **Records**: 1,538 curated occurrence records across three green lizard species endemic to Cuba (*Anolis allisoni*, *Anolis porcatus*, and *Anolis torresfundorai*), including primary holotype localities.
- **Ideal For**: Practicing type locality highlighting, insular land-clipped IUCN Red List Criterion B evaluation, multi-species comparative faceting, and topographic elevation relief.

```r
library(OneClickCartographics)

# Load into R workspace
data(anoles)

# Generate publication map with primary type specimen highlighting
run_map(anoles,
        clean_coords   = TRUE,
        highlight_type = TRUE,
        journal        = "wiley")
```

### 2. Neotropical Dry Forest Bees (`bees`)
- **Records**: 88,919 occurrence records for 1,851 bee species (Hymenoptera: Apoidea) across Neotropical seasonally dry tropical forests (Author et al., 2026).
- **Ideal For**: Continental-scale macroecological mapping, 100-km equal-area grid aggregation (species richness and record density), and protected area network overlap.

```r
library(OneClickCartographics)

# Load into R workspace
data(bees)

# Continental 100-km species richness grid map
run_map(bees,
        grid_km        = 100,
        grid_metric    = "richness",
        show_points    = FALSE,
        show_countries = TRUE,
        journal        = "wiley")
```

Both datasets can also be accessed as raw CSV files for file-path workflows:
```r
f_anoles <- system.file("extdata", "anoles.csv", package = "OneClickCartographics")
f_bees   <- system.file("extdata", "bees.csv",   package = "OneClickCartographics")
```

---

## Implementation

### Quick Start Example

Call `run_map()` with default parameters (reads your spreadsheet, automatically projects, and writes maps and HTML reports to a timestamped folder):

```r
library(OneClickCartographics)

# Map your occurrence spreadsheet in a single call
run_map("data/my_species.csv")

# Or pass a species binomial directly to query GBIF automatically:
run_map("Panthera onca", journal = "zootaxa", preset = "2col")
```

---

### Full Master Example (All Arguments Commented)

Below is a complete, fully commented invocation of `run_map()` demonstrating the breadth of customizable arguments:

```r
library(OneClickCartographics)

run_map(
  # ---------- Input / Output ----------
  data_path           = "data/my_occurrences.csv",  # File path (CSV/TSV/XLSX), data.frame, or scientific name
  output_dir           = "outputs/my_map_run",       # Destination folder (default: auto-timestamped folder)
  formats              = c("png", "pdf"),            # Vector and raster formats ("png", "pdf", "svg", "tiff")
  dpi                  = 300,                        # Image resolution (300 for general print, 600 for fine line art)
  export_gis           = TRUE,                       # Whether to export GeoPackage and GeoJSON vector layers
  
  # ---------- Data Quality & GBIF ----------
  clean_coords         = TRUE,                       # Automated pre-flight cleaning (filters Null Island, swaps, centroids)
  gbif_limit           = 5000,                       # Maximum records to retrieve if querying GBIF by species name
  
  # ---------- Basemap & Administrative Layers ----------
  show_countries       = TRUE,                       # Display national sovereign boundaries
  show_states          = TRUE,                       # Display first-tier administrative boundaries (states/provinces)
  show_water           = TRUE,                       # Display major hydrological features (rivers and lakes)
  admin_level          = 1,                          # Administrative boundary detail level (0 = countries, 1 = states)
  
  # ---------- Protected Areas (WDPA) ----------
  show_protected_areas = TRUE,                       # Query WDPA via wdpar and overlay conservation reserves
  protected_areas_fill = "#2E7D32",                  # Fill color for protected reserves
  protected_areas_border = "#1B5E20",                # Border color for protected reserves
  protected_areas_alpha= 0.35,                       # Transparency of reserve polygons (0 = clear, 1 = opaque)
  
  # ---------- IUCN Red List Criterion B ----------
  iucn                 = TRUE,                       # Calculate Extent of Occurrence (EOO) and Area of Occupancy (AOO)
  show_eoo             = TRUE,                       # Render EOO minimum convex hull polygon
  show_aoo             = FALSE,                      # Render 2 km x 2 km AOO occupied grid cells
  iucn_medium          = "land",                     # Constrain EOO polygon to habitat ("land", "water", or "both")
  iucn_taxa            = c("Anolis porcatus"),       # Specific taxa to evaluate (evaluates all if NULL)
  show_taxa            = NULL,                       # Specific taxa to render as occurrence points
  
  # ---------- Taxonomic Type Specimen Highlighting ----------
  highlight_type       = TRUE,                       # Prominently highlight primary name-bearing types (holotypes)
  type_fill            = NULL,                       # Marker fill color (NULL = matches taxon color; or e.g. "#FFD700")
  type_size            = 4.5,                        # Size of type diamond markers (ggplot2 shape 23)
  
  # ---------- Equal-Area Stepped Grids ----------
  grid_km              = 25,                         # Equal-area grid resolution in kilometers (e.g. 10, 25, 50, 100)
  grid_metric          = "richness",                 # Grid metric: "richness" (species/cell), "records" (density), or "both"
  grid_steps           = "auto",                     # Binning method: "auto", "continuous", or integer number of categories
  grid_category_width  = NULL,                       # Fixed observation span per category (e.g. 15 observations/class)
  grid_breaks          = NULL,                       # Explicit custom break thresholds (e.g. c(1, 5, 25, 75))
  grid_category_method = "pretty",                   # Interval distribution method: "pretty" or "quantile"
  grid_line_col        = "black",                    # Grid cell contour line color
  grid_line_width      = 0.3,                        # Grid cell contour stroke weight (pt)
  
  # ---------- Elevation, Hillshade & Bathymetry ----------
  show_elevation       = TRUE,                       # Fetch digital elevation raster via elevatr
  elevation_scope      = "both",                     # Scope: "land" (topography only), "sea" (bathymetry), or "both"
  water_depth          = TRUE,                       # Whether to include marine bathymetric relief
  elevation_limits     = c(-8216, 1974),             # Explicit scale limits c(min, max) in meters (NULL = raster extrema)
  show_terrain         = TRUE,                       # Render 3D shaded hillshade relief
  
  # ---------- Publisher Presets & Dimensions ----------
  journal              = "wiley",                    # Journal preset ("wiley", "nature", "zootaxa", "pensoft", "plos", "elsevier")
  preset               = "2col",                     # Column preset: "1col" (single column), "1.5col", or "2col" (full width)
  width                = NULL,                       # Custom manual width in inches (overrides preset if set)
  height               = NULL,                       # Custom manual height in inches (auto-calculated to aspect ratio if NULL)
  
  # ---------- Multi-Species Comparative Faceting ----------
  facet                = FALSE,                      # Split multi-species datasets into synchronized comparative panels
  facet_by             = "species",                  # Column to facet by
  facet_layout         = "grid",                     # Panel arrangement: "grid", "row" (horizontal), or "column" (vertical)
  facet_cols           = 2,                          # Number of facet columns
  north_arrow_scale    = 0.75,                       # Relative scaling factor for north arrows in sub-panels
  
  # ---------- Map Furniture & Insets ----------
  show_scale           = TRUE,                       # Display dynamic, unprojected-accurate scale bar
  show_north           = TRUE,                       # Display cartographic compass / north arrow
  show_graticules      = TRUE,                       # Display latitude/longitude graticule lines
  inset_map            = TRUE,                       # Render an orthographic overview globe locator inset
  
  # ---------- Aesthetics & Legends ----------
  point_size           = 2.2,                        # Size of standard occurrence points
  point_alpha          = 0.85,                       # Point opacity (0 = transparent, 1 = opaque)
  show_points          = TRUE,                       # Whether to plot individual point symbols
  show_species_legend  = TRUE,                       # Display taxon color legend (suppressed automatically for >50 taxa)
  legend_position      = "right",                    # Legend placement: "right", "bottom", "left", or "none"
  verbose              = TRUE                        # Print progress messages and analytical summaries to console
)
```

---

## Description of run_map() Arguments

| Argument | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data_path` | string / data.frame | *required* | Path to CSV/TSV/Excel file, an in-memory `data.frame`, or a scientific name for GBIF lookup. |
| `output_dir` | string | `NULL` | Directory to save maps and reports. If `NULL`, generates `OneClickCartographics_YYYYMMDD_HHMMSS/`. |
| `formats` | character vector | `c("png", "pdf")` | Graphic formats to export. Options: `"png"`, `"pdf"`, `"svg"`, `"tiff"`. |
| `dpi` | integer | `300` | Resolution for raster exports (300 for standard publication, 600 for line art). |
| `clean_coords` | logical | `FALSE` | Automatically filter Null Island $(0,0)$ coordinates, transposed lat/lon, and administrative centroids. |
| `gbif_limit` | integer | `5000` | Maximum number of records to retrieve when `data_path` is a taxonomic binomial string. |
| `show_countries` | logical | `FALSE` | Overlay national political boundary lines from Natural Earth. |
| `show_states` | logical | `FALSE` | Overlay first-tier administrative boundaries (states, provinces, departments). |
| `show_water` | logical | `FALSE` | Overlay major rivers, lakes, and hydrographic centerlines. |
| `admin_level` | integer | `0` | Administrative border level (`0` = countries, `1` = states/provinces). |
| `show_protected_areas` | logical | `FALSE` | Query and overlay World Database on Protected Areas (WDPA) conservation reserves via `wdpar`. |
| `protected_areas_fill` | string | `"#2E7D32"` | Hex fill color for protected area polygons. |
| `protected_areas_alpha`| numeric | `0.3` | Opacity of protected area polygons (0 to 1). |
| `iucn` | logical | `FALSE` | Calculate IUCN Red List Criterion B Extent of Occurrence (EOO) and Area of Occupancy (AOO). |
| `show_eoo` | logical | `TRUE` | Render the EOO minimum convex hull polygon on the map when `iucn = TRUE`. |
| `show_aoo` | logical | `FALSE` | Render IUCN-standard $2 	imes 2	ext{ km}$ occupied grid cells when `iucn = TRUE`. |
| `iucn_medium` | string | `"both"` | Constrain EOO polygon to species habitat: `"land"` (terrestrial), `"water"` (marine), or `"both"`. |
| `iucn_taxa` | character vector | `NULL` | Specific species to evaluate for IUCN metrics. If `NULL`, evaluates all taxa. |
| `show_taxa` | character vector | `NULL` | Specific species to display as point localities alongside IUCN hulls. |
| `highlight_type` | logical | `FALSE` | Emphasize primary name-bearing types (holotypes, syntypes) with distinct diamond markers. |
| `type_fill` | string | `NULL` | Fill color for type markers. If `NULL`, matches the taxon's assigned palette color. |
| `type_size` | numeric | `4.0` | Size of type specimen diamond glyphs (ggplot2 shape 23). |
| `grid_km` | numeric | `NULL` | Equal-area cell resolution in kilometers (e.g. `10`, `25`, `50`, `100`). |
| `grid_metric` | string | `"richness"` | Metric for grid cells: `"richness"` (species/cell), `"records"` (density), or `"both"`. |
| `grid_steps` | string / integer | `"auto"` | Categorization scheme: `"auto"` (adaptive integer steps), `"continuous"`, or an integer number of bins. |
| `grid_category_width` | numeric | `NULL` | Fixed observation count width per category across all classes (e.g. `25` records/class). |
| `grid_breaks` | numeric vector | `NULL` | Explicit custom cutoff thresholds per category (e.g. `c(1, 5, 25, 75)`). |
| `grid_category_method`| string | `"pretty"` | Interval distribution algorithm: `"pretty"` (rounded integer breaks) or `"quantile"`. |
| `show_elevation` | logical | `FALSE` | Download and render continuous elevation relief via `elevatr`. |
| `elevation_scope` | string | `"both"` | Scope for elevation raster: `"both"` (land + sea), `"land"` (terrestrial only), or `"sea"` (bathymetry). |
| `water_depth` | logical | `TRUE` | Include marine bathymetric relief gradients below sea level. |
| `elevation_limits` | numeric vector | `NULL` | Explicit minimum and maximum scale limits `c(min, max)` in meters (e.g. `c(-8216, 1974)`). |
| `show_terrain` | logical | `FALSE` | Render 3D shaded relief hillshading on land topography. |
| `journal` | string | `NULL` | Journal preset styling: `"wiley"`, `"nature"`, `"zootaxa"`, `"pensoft"`, `"plos"`, `"elsevier"`. |
| `preset` | string | `"2col"` | Column preset dimension: `"1col"` (single column), `"1.5col"`, or `"2col"` (full page spread). |
| `facet` | logical | `FALSE` | Split multi-taxon occurrence datasets into synchronized multi-panel comparative plates. |
| `facet_by` | string | `"species"` | Column attribute to facet panels by. |
| `facet_layout` | string | `"grid"` | Multi-panel layout structure: `"grid"`, `"row"` (horizontal stack), or `"column"` (vertical stack). |
| `north_arrow_scale` | numeric | `0.75` | Relative proportional scaling factor for north arrows in faceted sub-panels. |
| `inset_map` | logical | `FALSE` | Render an orthographic overview globe locator inset in the corner of the map frame. |
| `show_points` | logical | `TRUE` | Plot individual occurrence point markers (set to `FALSE` for pure grid/relief maps). |
| `show_scale` | logical | `TRUE` | Display a distance scale bar calibrated to the projection. |
| `show_north` | logical | `TRUE` | Display a cartographic compass north arrow. |
| `export_gis` | logical | `TRUE` | Export GIS vector layers (`layers.gpkg` and `localities.geojson`). |
| `verbose` | logical | `TRUE` | Print analytical summaries and progress updates to the console. |

---

## Core Analytical Capabilities

### 1. Zero-Configuration Minimalist Maps & Boundary Layers

By default, `OneClickCartographics` adheres to a strict minimalist aesthetic—displaying clean locality points on an uncluttered backdrop without visual distractions. Boundaries, water bodies, and graticules remain opt-in:

```r
# Minimalist clean basemap:
run_map("data/my_occurrences.csv")

# Enrich with multi-tier administrative boundaries and hydrology:
run_map("data/my_occurrences.csv",
        show_countries = TRUE,
        show_states    = TRUE,
        show_water     = TRUE)
```

---

### 2. Direct GBIF Taxonomic Retrieval

Pass any valid scientific name to automatically download georeferenced records, eliminate spatial duplicates, and produce a complete cartographic suite:

```r
run_map("Ceiba pentandra",
        clean_coords = TRUE,
        iucn         = TRUE,
        journal      = "wiley",
        preset       = "2col")
```

---

### 3. Automated Pre-Flight Quality Control & Coordinate Cleaning

Enable automated coordinate validation to remove problematic records prior to analysis:

```r
run_map("data/raw_field_localities.csv",
        clean_coords = TRUE)
```

- **Filters Null Island**: Automatically detects and drops $(0, 0)$ coordinates.
- **Transposition Correction**: Identifies swapped latitude and longitude values where coordinates fall into oceans instead of expected land areas.
- **Centroid & Grid Filtering**: Flags artificial administrative country/province centroids and low-precision integer placeholders.
- **Audit Logging**: Summarizes cleaning statistics directly in `00_map_report.html` and exports `00_cleaned_localities.csv`.

---

### 4. Topologically Intelligent IUCN Criterion B (EOO & AOO)

Calculate Extent of Occurrence (EOO) and Area of Occupancy (AOO) under IUCN Red List Criterion B, automatically clipping non-habitat expanses:

```r
run_map(anoles,
        iucn        = TRUE,
        show_eoo    = TRUE,
        show_aoo    = FALSE,
        iucn_medium = "land",
        iucn_taxa   = c("Anolis porcatus", "Anolis torresfundorai"),
        journal     = "wiley")
```

- **Intelligent Medium Clipping (`iucn_medium = "land"`)**: Prevents terrestrial species hulls from incorrectly spanning open ocean water. In our Cuban anole case study, land-clipping eliminated 66,131.5 km² (48.67%) of spurious marine waters from the raw hull of *Anolis porcatus*.
- **Preliminary Red List Categories**: Classifies taxa into Critically Endangered (CR), Endangered (EN), Vulnerable (VU), or Least Concern (LC) based on IUCN thresholds.
- **Metrics Export**: Exports quantitative results to `07_iucn_metrics.csv`.

---

### 5. Taxonomic Type Specimen Highlighting

Distinguish primary name-bearing types from ordinary occurrence clusters:

```r
run_map(anoles,
        highlight_type = TRUE,
        journal        = "zootaxa")
```

- **Prominent Glyphs**: Holotypes, syntypes, lectotypes, and neotypes are rendered as prominent diamond markers (`shape = 23`) matching taxon coloration.
- **Layer Stacking**: Type markers are plotted on the topmost layer, ensuring they remain visible in dense population clusters.

---

### 6. Protected Area Representation & Reserve Occupancy Analysis

Quantify conservation network representation across national reserves:

```r
run_map(anoles,
        show_protected_areas = TRUE,
        protected_areas_fill = "#2E7D32",
        protected_areas_alpha= 0.35)
```

- **WDPA Integration**: Queries the World Database on Protected Areas via `wdpar` and clips reserves matching the map extent.
- **Reserve Metrics**: Computes overall and per-species occurrence representation inside vs. outside reserves, and reserve occupancy metrics (occupied vs. empty reserves), exporting `protected_areas_counts.csv` and `protected_areas_summary.csv`.

---

### 7. Dual-Metric Equal-Area Stepped Grids (Richness & Density)

Aggregate occurrences into equal-area cells (10–100 km) to reveal spatial sampling bias and regional biodiversity patterns:

```r
# Continental bee richness and sampling density across 100-km cells:
run_map(bees,
        grid_km        = 100,
        grid_metric    = "both",
        show_points    = FALSE,
        show_countries = TRUE)
```

- **Two Dedicated CSV Summaries**: Automatically exports `grid_richness_summary.csv` and `grid_record_density_summary.csv`, strictly quantifying occupied cells (total occupied cells, min, max, mean, and median values per cell) while excluding uninformative empty cells across the bounding extent.
- **Customizable Intervals**: Users can define category counts (`grid_categories = 5`), uniform observation spans (`grid_category_width = 25`), or independent cutoffs (`grid_breaks = c(1, 5, 25, 100)`).

---

### 8. Continuous Topographic Elevation, 3D Hillshade & Marine Bathymetry

Render continuous topographic relief rasters fetched via `elevatr`:

```r
# Continuous topography & bathymetry spanning -8,216 m to +1,974 m:
run_map(anoles,
        show_elevation   = TRUE,
        elevation_scope  = "both",
        water_depth      = TRUE,
        elevation_limits = c(-8216, 1974),
        show_terrain     = TRUE)
```

- **Elevation Scope**: `"both"` (land topography + ocean bathymetry), `"land"` (land topography only), or `"sea"` (ocean depth only).
- **Custom Limits**: Explicit scale limits via `elevation_limits = c(min, max)` ensure extreme mountain summits and oceanic trenches are properly scaled.

---

### 9. Multi-Species Comparative Plates (Faceted Layouts)

Compare distributions across related taxa on synchronized, publication-ready multi-panel plates:

```r
run_map(anoles,
        facet             = TRUE,
        facet_by          = "species",
        facet_layout      = "row",
        north_arrow_scale = 0.75,
        journal           = "wiley")
```

- **Synchronized Geometries**: Uniform scales, coordinate systems, and bounding frames across all panels.
- **Proportional North Arrows**: North arrows auto-scale (`north_arrow_scale = 0.75`) to avoid visual crowding.

---

### 10. Journal Dimension Presets

Format maps precisely to publisher specifications without trial-and-error cropping:

```r
# Zootaxa full-width 2-column plate (175 mm width)
run_map(anoles, journal = "zootaxa", preset = "2col")

# Pensoft / ZooKeys single-column figure (80 mm width)
run_map(anoles, journal = "pensoft", preset = "1col")

# Nature 1.5-column format (120 mm width)
run_map(anoles, journal = "nature", preset = "1.5col")
```

---

### 11. Modular Cartographic Builder & Selective Post-Hoc Saving

For custom pipelines, construct layers incrementally and decide *after building* which maps to export:

```r
# 1. Build a multi-layered cartographic pipeline:
res <- construct_map("data/my_species.csv",
                     water   = TRUE,
                     admin   = "state",
                     terrain = TRUE,
                     grid    = "richness",
                     grid_km = 25)

# 2. Inspect built map objects in memory:
names(res$maps)
# Returns: "simple"  "points"  "terrain"  "richness"  "final"

# 3. View any map variation interactively:
print(res$maps$terrain)

# 4. Save selected variations post-hoc:
save_maps(res, select = c("terrain", "richness"), formats = c("png", "pdf"))
```

---

### 12. Interactive Leaflet Web Cartography

Inspect localities interactively with rich hover tooltips and layer toggles:

```r
# Launch interactive browser map:
interactive_map(anoles, color_by = "species")

# Export as a self-contained HTML file:
interactive_map(anoles, output_file = "interactive_map.html")
```

---

### 13. Automated Reproducible HTML Report & Audit Trail

Every execution automatically generates `00_map_report.html` in the output directory, featuring:
1. **Visual Map Previews**: Embedded high-resolution previews of all rendered figures.
2. **File Inventory & Audit**: Detailed catalog of all exported PNG, PDF, GeoPackage, and CSV files.
3. **Materials & Methods Prose**: Ready-to-paste manuscript paragraph detailing CRS, datums, base layers, and analytical procedures.
4. **BibTeX Citations**: Complete citations for all underlying data sources (Natural Earth, elevatr, WDPA, IUCN).

Example generated Methods text:
> *"Species distribution maps were generated using the R package OneClickCartographics (Anonymous Authors, 2026). Cartographic boundaries were derived from Natural Earth public domain data. Spatial coordinates were projected into an equal-area projection centered on the geographic extent of the data. Minimum convex polygons (EOO) and Area of Occupancy (AOO) were computed following IUCN Red List guidelines with habitat-boundary constraints."*

---

## Journal Specifications Table

| Journal / Publisher | Preset | Target Width (mm) | Recommended Uses |
| :--- | :--- | :--- | :--- |
| **Zootaxa** | `"1col"` | 80 mm | Single species or localized geographic area |
| **Zootaxa** | `"2col"` | 175 mm | Multi-species comparative plates, continental maps |
| **Pensoft** (ZooKeys / PhytoKeys) | `"1col"` | 80 mm | Single-column text figures |
| **Pensoft** (ZooKeys / PhytoKeys) | `"2col"` | 170 mm | Full page width figures |
| **Nature** | `"1col"` | 89 mm | Brief Communications, standard columns |
| **Nature** | `"1.5col"` | 120 mm | Intermediate width multi-panel figures |
| **Nature** | `"2col"` | 180 mm | Full page width articles |
| **PLOS** | `"1col"` | 83 mm | Standard column width |
| **PLOS** | `"2col"` | 173 mm | Full-width landscape or portrait plates |
| **Elsevier** | `"1col"` / `"2col"` | 90 mm / 190 mm | Standard and wide spreads |
| **Wiley** | `"1col"` / `"2col"` | 85 mm / 175 mm | BES ecology and evolution journals |

---

## Citation

If you use `OneClickCartographics` in your research, taxonomic descriptions, or conservation assessments, please cite:

```text
Anonymous Authors (2026). OneClickCartographics: 
Automated Publication-Grade Cartography for Biodiversity Science. 
Methods in Ecology and Evolution (in review).
R package version 1.0.0. https://github.com/anonymous-peer/OneClickCartographics
```

### BibTeX Entry

```bibtex
@Manual{Anonymous2026,
  title  = {OneClickCartographics: Automated Publication-Grade Cartography for Biodiversity Science},
  author = {Anonymous Authors},
  year   = {2026},
  note   = {R package version 1.0.0},
  url    = {https://github.com/anonymous-peer/OneClickCartographics}
}
```

---

## License

This project is open-source software licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

Copyright (c) 2026 Anonymous Author & Anonymous Co-Author.
