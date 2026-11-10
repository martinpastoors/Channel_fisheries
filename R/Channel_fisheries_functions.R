# ==============================================================================
# Channel_fisheries_functions.R
#
# Libraries, helper functions and data-reading functions for
# channel_fisheries_combined_report (Rmd). The Rmd sources this file in its
# setup chunk; all settings (study area, paths, years, parameters) stay in the
# Rmd. The read functions take their inputs as arguments, so they can be run
# or stepped through on their own after running the setup chunk.
#
# Development note: developed with assistance from Claude (Anthropic).
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(patchwork)    # combine panels for AER vs GFW comparison
library(captioner)    # auto-numbered figure and table captions
library(flextable)

# Spatial (GFW section)
library(sf)
library(spatstat)
library(rnaturalearth)
library(rnaturalearthdata)

# Concentration metrics (GFW section)
library(ineq)

# ==============================================================================
# r helpers-gfw
# ==============================================================================

load_object <- function(filepath, object_name = NULL) {
  local({
    e <- new.env()
    load(filepath, envir = e)
    if (is.null(object_name)) {
      objects <- ls(e)
      if (length(objects) == 1) {
        e[[objects]]
      } else {
        stop("File contains ", length(objects), " objects (",
             paste(objects, collapse=", "),
             "). Please specify object_name.")
      }
    } else {
      e[[object_name]]
    }
  })
}

# msg() — like message() but flushes immediately to the RStudio console.
# Useful when running chunks interactively: without flush.console(), messages
# from inside long-running blocks are buffered and only appear after the block
# completes, making it look like nothing is happening.
msg <- function(...) {
  message(...)
  flush.console()
}

apply_flag_other <- function(df, flag_col = "vessel_flag",
                              days_col = "fishing_days") {
  df %>%
    group_by(across(all_of(flag_col))) %>%
    mutate(total_flag_days = sum(.data[[days_col]])) %>%
    ungroup() %>%
    mutate(
      total_days = sum(.data[[days_col]]),
      flag_pct   = total_flag_days / total_days * 100,
      across(all_of(flag_col),
             ~ if_else(flag_pct < threshold_pct, "Other", .x))
    )
}

# ==============================================================================
# r helpers-aer
# ==============================================================================

map_gear_aer <- function(x) {
  dplyr::recode(str_to_upper(str_trim(x)),
    DTS = "Bottom trawl/seine", TBB = "Beam trawl",
    PEL = "Midwater trawl",     DFN = "Gillnet",
    HOK = "Handline/rod",       FPO = "Pot/trap",
    DRB = "Dredge",             PS  = "Purse seine",
    LL  = "Longline",
    .default = "Other/mixed"
  )
}

# UK length groups -> AER vessel length classes (lower bound of each group)
map_length_uk <- function(x) {
  lb <- suppressWarnings(readr::parse_number(x))   # "Unknown" -> NA
  dplyr::case_when(
    lb <  10 ~ "VL0010",
    lb <  12 ~ "VL1012",
    lb <  18 ~ "VL1218",
    lb <  24 ~ "VL1824",
    lb <  40 ~ "VL2440",
    lb >= 40 ~ "VL40XX",
    TRUE     ~ NA_character_
  )
}

# UK gear categories -> AER gear categories (as returned by map_gear_aer)
map_gear_uk <- function(x) {
  lookup <- c(
    "Beam trawl"           = "Beam trawl",
    "Demersal trawls"      = "Bottom trawl/seine",
    "Demersal seine"       = "Bottom trawl/seine",
    "Dredge"               = "Dredge",
    "Drift and fixed nets" = "Gillnet",
    "Handlines"            = "Handline/rod",
    "Longlines"            = "Handline/rod",
    "Pots and traps"       = "Pot/trap",
    "Pelagic seine"        = "Purse seine",
    "Pelagic trawls"       = "Other/mixed",
    "Other mobile gears"   = "Other/mixed",
    "Other passive gears"  = "Other/mixed",
    "Unknown"              = "Other/mixed"
  )
  dplyr::recode(str_trim(x), !!!lookup, .default = "Other/mixed")
}

# Harmonise species codes that differ between AER and UK for the same species
harmonise_species <- function(code) {
  dplyr::case_when(
    code %in% c("HOM", "JAX")                           ~ "JAX",
    code %in% c("CTC", "CTL")                           ~ "CTL",
    code %in% c("MON", "ANF", "MNZ")                    ~ "MNZ",
    code %in% c("SQZ", "SQR", "SQC", "SQA", "OMZ")      ~ "SQZ",
    str_detect(code, "^RJ") | code %in% c("SKA", "RAJ") ~ "SKA",
    TRUE                                                ~ code
  )
}

harmonised_names <- c(
  JAX = "Horse mackerels",
  CTL = "Cuttlefish",
  MNZ = "Monkfish",
  SQZ = "Squids",
  SKA = "Skates and rays"
)

# ==============================================================================
# Species groups — replaces assign_species_group() and species_group_levels
# in Channel_fisheries_functions.R
# ==============================================================================
# Changes compared with the previous version:
#   - Roundfish now includes the other gadoids: pouting/bib, pollack, saithe,
#     hake and ling (previously pouting and pollack were "Other demersal fish")
#   - New group "Seabass & seabreams": European seabass, black seabream and
#     other seabreams
#   - New group "Elasmobranchs": sharks, skates and rays (catsharks,
#     smooth-hounds, tope, thornback, blonde, spotted, undulate ray, etc.)
#   - Common European bittersweet (GKL, Glycymeris glycymeris) is a bivalve and
#     moves from "Other demersal fish" to "Shellfish & crustaceans"
#   - "Other demersal fish" keeps the rest: conger, monkfish, John dory,
#     wrasses, weevers and "marine fishes nei"
#
# The order of the conditions matters: the first one that matches wins.

assign_species_group <- function(species_code, species_name) {
  sn <- tolower(species_name)
  case_when(
    # ---- Pelagic fish -------------------------------------------------------
    str_detect(sn, "herring|sardine|pilchard") |
      species_code %in% c("HER","PIL","SPR","SAR","SIH") |
      str_detect(sn, "^mackerel") |
      species_code %in% c("MAC","MAS") |
      str_detect(sn, "horse mackerel|scad") |
      species_code %in% c("HOM","HMM","JAA","JAX")
    ~ "Pelagic fish",
    
    # ---- Seaweeds -----------------------------------------------------------
    str_detect(sn, "seaweed|kelp|tangle|laminaria|fucus|ulva|algae|alga|macroalg") |
      species_code %in% c("LQD","LAH","SWX","SWA","SWB","SWC","SWD","SWE",
                          "SWF","SWG","SWH","SWI","SWJ","SWK","SWL","SWM",
                          "SWN","SWO","SWP","SWQ","SWR","SWS","SWT","SWU",
                          "SWV","SWW","ASP","ULV","FUC","LAM","ALG")
    ~ "Seaweeds",
    
    # ---- Cephalopods --------------------------------------------------------
    str_detect(sn, "cuttlefish|sepia") |
      species_code %in% c("CTC","CTL","CTZ","CTB","CTS","SEB","OCC")
    ~ "Cuttlefish",
    
    str_detect(sn, "squid|loligo|bobtail|ommastrep|shortfin") |
      species_code %in% c("SQC","SQR","SQE","SQM","LOL","LOG","SQS","SQZ",
                          "SQX","EUR","BOC","SQI","OMS","TOD","NBS","OMP","ILL")
    ~ "Squid",
    
    # ---- Demersal fish ------------------------------------------------------
    str_detect(sn, "mullet") |
      species_code %in% c("MUL","MUG","TFM","MUT","MUX")
    ~ "Mullets",
    
    str_detect(sn, "gurnard|searobin") |
      species_code %in% c("GUG","GUR","GUU","GUX","TUB","BIR")
    ~ "Gurnards",
    
    # Gadoids: cod, haddock, whiting, pouting, pollack, saithe, hake, ling
    str_detect(sn, "whiting|haddock|\\bcod\\b|pouting|\\bbib\\b|pollack|pollock|saithe|\\bhake\\b|\\bling\\b") |
      species_code %in% c("WHG","HAD","COD","BIB","POL","POK","HKE","LIN")
    ~ "Roundfish",
    
    # Seabass and seabreams (before the elasmobranchs, so that "Ray's bream"
    # is not taken for a ray)
    str_detect(sn, "seabass|sea bass|seabream|sea bream|porgies|\\bbass\\b|bream") |
      species_code %in% c("BSS","BRB","SBR","SBG","SBA","SBX","DEX")
    ~ "Seabass & seabreams",
    
    # Elasmobranchs: sharks, skates and rays
    str_detect(sn, "\\bray\\b|\\brays\\b|skate|shark|dogfish|catshark|smooth.?hound|spurdog|torpedo") |
      species_code %in% c("SYC","SYT","SDV","SMD","GAG","DGS","DGH","SCK","SDS",
                          "RJC","RJH","RJB","RJN","RJM","RJI","RJU","RJE","RJA",
                          "RJF","RJO","RAJ","SKA","SRX","TTO")
    ~ "Elasmobranchs",
    
    species_code %in% c(
      "SOL","SOM","SOO","SOZ",
      "PLE",
      "TUR","BLL",
      "DAB","FLE",
      "MEG","LDB","MES",
      "LEM","WIT","SCL","POO",
      "HAL","GHL",
      "SXS","CET",
      "FLX","FLO","BUT","RHG"
    ) |
      str_detect(sn, "plaice|\\bsole\\b|turbot|brill|\\bdab\\b|flounder|halibut|megrim|witch|limande|cardine|fletan|plie|pliee")
    ~ "Flatfishes",
    
    # ---- Shellfish & invertebrates ------------------------------------------
    str_detect(sn, "scallop|pecten") |
      species_code %in% c("SCE","QSC","PEC","SCA","SCB")
    ~ "Scallops",
    
    str_detect(sn, "shrimp|crangon|pandalus|crevette") |
      species_code %in% c("CSH","PRA","TGS","CRG","CPR","PAN","CFW")
    ~ "Shrimps",
    
    # ---- Other shellfish & crustaceans --------------------------------------
    str_detect(sn, "whelk|buccin|crab|lobster|crawfish|langoustine|nephrops|venus|mussel|clam|cockle|oyster|bittersweet|glycymeris") |
      species_code %in% c("WHE","BUC","CRE","SCR","LBE","NEP","VEV",
                          "MYV","MYE","COC","OYF","OYG","CLX","SCX","GKL")
    ~ "Shellfish & crustaceans",
    
    # ---- Other demersal fish ------------------------------------------------
    str_detect(sn, "conger|monkfish|anglerfish|john dory|\\bdory\\b|wrasse|weever|marine fishes") |
      species_code %in% c("COE","MON","ANF","MNZ","JOD","WRA","USB","WEG","MZZ")
    ~ "Other demersal fish",
    
    TRUE ~ NA_character_
  )
}

# Ordered factor levels (also the order in the figures and tables)
species_group_levels <- c("Pelagic fish", "Seaweeds",
                          "Cuttlefish", "Squid",
                          "Mullets", "Gurnards", "Roundfish",
                          "Seabass & seabreams", "Elasmobranchs",
                          "Flatfishes", "Scallops", "Shrimps",
                          "Shellfish & crustaceans",
                          "Other demersal fish")

# ---- Pelagic data anomaly flag ----------------------------------------------
# NLD VL40XX Other/mixed pelagic landings in 27.4.C in 2012 are anomalously
# high (~84,000 t vs ~10,000 t in surrounding years), consistent with a
# sub-region misattribution in the AER source data (Channel landings apparently
# reported under 27.4.C rather than 27.7.D). Requires verification with STECF/CBS.
pelagic_species_codes <- c("HER","PIL","HOM","MAC","SPR","JAX","HMM","JAA","MAS","SAR","SIH")

pelagic_anomaly_note <- paste0(
  "* 2012 NLD pelagic landings in 27.4.C excluded due to anomalously high values ",
  "Requires verification with STECF/AER."
)

remove_pelagic_anomaly <- function(df) {
  df |>
    filter(!(country_code  == "NLD"         &
             sub_reg       == "27.4.c"      &
             year          == 2012          &
             gear_category == "Other/mixed" &
             species_code  %in% pelagic_species_codes))
}



# ==============================================================================
# r helpers-fdi
# ==============================================================================

safe_numeric <- function(x, extra_sentinels = character(0)) {
  base_pat <- "NK|na|N/A|#N/A|CONFID|confidential|BEL|\\."
  full_pat  <- if (length(extra_sentinels) > 0)
    paste0("^(", base_pat, "|", paste(extra_sentinels, collapse = "|"), ")$")
  else
    paste0("^(", base_pat, ")$")
  cleaned <- str_trim(x) |>
    str_replace_all(regex(full_pat, ignore_case = TRUE), NA_character_)
  val <- suppressWarnings(as.numeric(cleaned))
  val[!is.na(val) & val == -1] <- NA_real_
  val
}

normalise_division <- function(x) {
  x <- str_trim(x) |> str_to_upper()
  x <- str_remove(x, "^27\\.")
  str_replace(x, "^(\\d+)([A-Z])$", "\\1.\\2")
}

map_gear_fdi <- function(gear_code) {
  lookup <- c(
    OTB="Bottom trawl", OTT="Bottom trawl", PTB="Bottom trawl",
    OTM="Midwater trawl", PTM="Midwater trawl",
    TBB="Beam trawl",
    SSC="Scottish seine", SDN="Danish seine",
    GNS="Gillnet", GND="Gillnet",
    FPN="Pot/trap", FPO="Pot/trap",
    LLS="Longline", LLD="Longline",
    DRB="Dredge",
    MIS="Other/mixed", NK="Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(gear_code)),
                !!!lookup, .default = "Other/mixed")
}



# ==============================================================================
# ---- Division code converters -------------------------------------------------
# ==============================================================================
# Divisions are defined in the short ICES form ("7.d"). AER and MMO use "27.7.d",
# GFW and fao_sf (after toupper) use "27.7.D", FDI uses "7.D".

div_aer   <- function(x) paste0("27.", tolower(x))
div_gfw   <- function(x) paste0("27.", toupper(substr(x, 1, nchar(x) - 1)),
                                toupper(substr(x, nchar(x), nchar(x))))
div_fdi   <- function(x) toupper(x)
div_label <- function(x) paste0("27.", tolower(x))

# ==============================================================================
# ---- Cache helper -------------------------------------------------------------
# ==============================================================================

# TRUE if a cache exists, is newer than all its source files and contains all
# required objects
cache_ok <- function(cache, sources, objects = NULL) {
  if (!file.exists(cache)) return(FALSE)
  sources <- sources[file.exists(sources)]
  newer   <- length(sources) == 0 ||
             file.mtime(cache) > max(file.mtime(sources), na.rm = TRUE)
  complete <- is.null(objects) ||
              all(objects %in% load(cache, envir = new.env()))
  newer && complete
}

# ==============================================================================
# ---- ICES divisions -----------------------------------------------------------
# ==============================================================================

# Study-division polygons and neighbouring divisions (for maps), WGS84.
# Returns list(fao_sf_division, fao_sf_context).
read_divisions <- function(fao_file, divisions_gfw, buffer_deg = 1) {

  s2_old <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(s2_old), add = TRUE)

  fao_divs <- load_object(fao_file, "fao_sf") |>
    filter(F_LEVEL == "DIVISION") |>
    dplyr::select(division = F_DIVISION) |>
    mutate(division = toupper(division)) |>
    sf::st_transform(4326)

  fao_sf_division <- fao_divs |>
    filter(division %in% divisions_gfw)

  if (length(unique(fao_sf_division$division)) != length(divisions_gfw))
    stop("Not all study divisions found in fao_sf: ",
         paste(setdiff(divisions_gfw, fao_sf_division$division), collapse = ", "),
         call. = FALSE)

  # Context window around the study divisions
  bb  <- sf::st_bbox(fao_sf_division)
  ctx <- sf::st_as_sfc(sf::st_bbox(c(xmin = bb[["xmin"]] - buffer_deg - 1,
                                     xmax = bb[["xmax"]] + buffer_deg + 1,
                                     ymin = bb[["ymin"]] - buffer_deg - 1,
                                     ymax = bb[["ymax"]] + buffer_deg + 1),
                                   crs = 4326))

  # Planar-geometry notes from sf are expected here (s2 is off) and suppressed
  suppressMessages(suppressWarnings({
    fao_sf_division <- fao_sf_division |>
      sf::st_make_valid() |>
      group_by(division) |>
      summarise(geometry = sf::st_union(geometry), .groups = "drop")

    fao_sf_context <- fao_divs |>
      sf::st_filter(ctx) |>
      sf::st_make_valid() |>
      group_by(division) |>
      summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
      sf::st_intersection(ctx)
  }))

  list(fao_sf_division = fao_sf_division, fao_sf_context = fao_sf_context)
}

# Extents (lon/lat) of the study divisions plus a buffer.
# Returns list(xlim, ylim, map_xlim, map_ylim).
study_extents <- function(fao_sf_division, window_buffer_deg = 0.5, map_buffer_deg = 1) {
  bb <- sf::st_bbox(fao_sf_division)
  list(
    xlim     = c(bb[["xmin"]], bb[["xmax"]]) + c(-1, 1) * window_buffer_deg,
    ylim     = c(bb[["ymin"]], bb[["ymax"]]) + c(-1, 1) * window_buffer_deg,
    map_xlim = c(bb[["xmin"]], bb[["xmax"]]) + c(-1, 1) * map_buffer_deg,
    map_ylim = c(bb[["ymin"]], bb[["ymax"]]) + c(-1, 1) * map_buffer_deg
  )
}

# ==============================================================================
# ---- AER ----------------------------------------------------------------------
# ==============================================================================

aer_required_cols <- c("year", "country_code", "sub_reg", "fishing_tech",
                       "vessel_length", "variable_code", "value")

# Find sheets by name pattern (case-insensitive) unless given explicitly.
# Pivot sheets are always skipped.
aer_find_sheets <- function(file, given, include, exclude = "pivot") {
  if (!is.null(given)) return(given)
  sh  <- readxl::excel_sheets(file)
  hit <- sh[str_detect(sh, regex(include, ignore_case = TRUE))]
  if (!is.null(exclude))
    hit <- hit[!str_detect(hit, regex(exclude, ignore_case = TRUE))]
  if (length(hit) == 0)
    stop("No sheet matching '", include, "' in ", basename(file),
         ". Sheets: ", paste(sh, collapse = " | "),
         ". Set effort_sheets / landings_sheets in aer_releases.", call. = FALSE)
  hit
}

# Read one AER sheet, check its columns and filter to the study divisions
# (divisions in AER format, e.g. "27.7.d")
aer_read_sheet <- function(file, sheet, divisions, extra_cols = character(0)) {
  df <- readxl::read_excel(file, sheet = sheet, guess_max = 10000) |>
    clean_names()
  missing <- setdiff(c(aer_required_cols, extra_cols), names(df))
  if (length(missing) > 0)
    stop("Sheet '", sheet, "' in ", basename(file), " lacks column(s): ",
         paste(missing, collapse = ", "), ". Columns: ",
         paste(names(df), collapse = ", "), call. = FALSE)
  df <- df |> mutate(sub_reg = str_to_lower(str_trim(sub_reg)))
  if (!any(df$sub_reg %in% divisions))
    msg("  WARNING: no rows for ", paste(divisions, collapse = ", "),
        " in sheet '", sheet, "'. sub_reg values: ",
        paste(head(sort(unique(df$sub_reg)), 30), collapse = ", "))
  df |>
    filter(sub_reg %in% divisions,
           !is.na(variable_code), !is.na(value)) |>
    mutate(year         = as.integer(year),
           country_code = str_to_upper(str_trim(country_code)),
           value        = suppressWarnings(as.numeric(value))) |>
    filter(!is.na(value))
}

# Read one AER release (an element of aer_releases).
# Returns list(effort, landings), both wide (one column per variable_code).
read_aer_release <- function(rel, divisions) {
  msg("AER ", rel$label, ": reading ", basename(rel$file))
  if (!file.exists(rel$file)) stop("AER file not found: ", rel$file, call. = FALSE)

  eff_sheets  <- aer_find_sheets(rel$file, rel$effort_sheets,   "^FS_effort.*region")
  land_sheets <- aer_find_sheets(rel$file, rel$landings_sheets, "^FS_landings")
  msg("  effort sheet(s):   ", paste(eff_sheets,  collapse = " | "))
  msg("  landings sheet(s): ", paste(land_sheets, collapse = " | "))

  keep_rows <- function(df) {
    df <- df |> filter(year %in% rel$years)
    if (!is.null(rel$countries)) df <- df |> filter(country_code %in% rel$countries)
    df
  }

  effort <- purrr::map(eff_sheets, \(s) aer_read_sheet(rel$file, s, divisions)) |>
    bind_rows() |>
    keep_rows() |>
    mutate(gear_category = map_gear_aer(fishing_tech), aer_source = rel$label) |>
    # fs_name is deliberately not a key: it can differ within a stratum
    dplyr::select(aer_source, year, country_code, sub_reg, fishing_tech,
                  vessel_length, gear_category, variable_code, value) |>
    pivot_wider(names_from = variable_code, values_from = value, values_fn = sum)

  landings <- purrr::map(land_sheets, \(s) aer_read_sheet(rel$file, s, divisions,
                                          c("species_code", "species_name"))) |>
    bind_rows() |>
    keep_rows() |>
    mutate(gear_category = map_gear_aer(fishing_tech), aer_source = rel$label) |>
    dplyr::select(aer_source, year, country_code, sub_reg, fishing_tech,
                  vessel_length, gear_category, species_code, species_name,
                  variable_code, value) |>
    pivot_wider(names_from = variable_code, values_from = value, values_fn = sum)

  if (!all(c("totwghtlandg", "totvallandg") %in% names(landings)))
    stop("AER ", rel$label, ": landings variables totwghtlandg/totvallandg not found. ",
         "Variables: ", paste(setdiff(names(landings), c("aer_source", "year", "country_code",
         "sub_reg", "fishing_tech", "vessel_length", "gear_category", "species_code",
         "species_name")), collapse = ", "), call. = FALSE)

  landings <- landings |>
    rename(live_weight_kg = totwghtlandg, value_euro = totvallandg) |>
    mutate(live_weight_t = live_weight_kg / 1000)

  msg("  ", nrow(effort), " effort strata, ", nrow(landings), " landings rows; ",
      "countries: ", paste(sort(unique(effort$country_code)), collapse = ", "),
      "; years: ", min(effort$year), "–", max(effort$year))

  list(effort = effort, landings = landings)
}

# Combine releases: per country x year, keep the release with the highest priority
combine_by_priority <- function(df, priority_labels) {
  df |>
    mutate(prio = match(aer_source, priority_labels)) |>
    group_by(country_code, year) |>
    filter(prio == min(prio)) |>
    ungroup() |>
    dplyr::select(-prio)
}

# Read all releases and build the combined series and summaries.
# Returns a named list with the objects used in the report.
read_aer_all <- function(releases, priority, divisions, kw_to_gt_ratio) {

  aer_list <- purrr::map(releases[priority], \(rel) read_aer_release(rel, divisions))
  priority_labels <- vapply(releases[priority], `[[`, "", "label")

  aer_effort_bysource   <- bind_rows(purrr::map(aer_list, "effort"))
  aer_landings_bysource <- bind_rows(purrr::map(aer_list, "landings"))

  aer_effort_raw   <- combine_by_priority(aer_effort_bysource,   priority_labels)
  aer_landings_raw <- combine_by_priority(aer_landings_bysource, priority_labels)

  msg("AER combined: source per country (years):")
  aer_effort_raw |>
    group_by(aer_source, country_code) |>
    summarise(years = paste0(min(year), "–", max(year)), .groups = "drop") |>
    print(n = Inf)

  # Fishing-days summary — retains vessel_length for capacity plots
  aer_effort_summary <- aer_effort_raw |>
    group_by(aer_source, year, country_code, gear_category, sub_reg, vessel_length) |>
    summarise(
      across(any_of(c("totfishdays", "totseadays", "totkwfishdays", "totgtfishdays")),
             ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Capacity-weighted effort: GT-fishing days where available; imputed from
  # kW-fishing days for countries that only report kW (fleet-average ratio)
  aer_effort_capacity_metric <- aer_effort_summary |>
    mutate(
      gt_fishdays_source = case_when(
        totgtfishdays > 0 ~ "GT (reported)",
        totkwfishdays > 0 ~ "GT (imputed from kW)",
        TRUE              ~ "Not available"
      ),
      gt_fishdays_harmonised = case_when(
        totgtfishdays > 0 ~ totgtfishdays,
        totkwfishdays > 0 ~ totkwfishdays * kw_to_gt_ratio,
        TRUE              ~ NA_real_
      )
    )

  aer_landings_summary <- aer_landings_raw |>
    group_by(aer_source, year, country_code, gear_category,
             sub_reg, species_code, species_name) |>
    summarise(
      live_weight_t = sum(live_weight_t, na.rm = TRUE),
      value_euro    = sum(value_euro,    na.rm = TRUE),
      .groups = "drop"
    )

  list(aer_effort_bysource        = aer_effort_bysource,
       aer_landings_bysource      = aer_landings_bysource,
       aer_effort_raw             = aer_effort_raw,
       aer_effort_summary         = aer_effort_summary,
       aer_effort_capacity_metric = aer_effort_capacity_metric,
       aer_landings_raw           = aer_landings_raw,
       aer_landings_summary       = aer_landings_summary)
}

# ---- AER inspection ------------------------------------------------------------
# Describes the structure and contents of an AER workbook, without the filters
# and pivots used in read_aer_release(). Returns a list per sheet with:
#   columns   — every column: type, number of NAs, distinct values, examples
#   variables — per variable (all descriptive columns such as variable_name,
#               unit, ...): rows, NA / non-numeric / zero values, value range,
#               years and countries, restricted to `divisions` if given
#   non_numeric — the non-numeric strings found in the value column (e.g.
#               confidentiality flags) and how often they occur
#   coverage  — rows per country x year (study divisions only, if given)
# Also returns the raw content of NOTES / info / code sheets.
inspect_aer_sheet <- function(file, sheet, divisions = NULL, max_examples = 3) {

  df <- readxl::read_excel(file, sheet = sheet, guess_max = 100000) |>
    clean_names()
  msg("\n==== ", sheet, ": ", nrow(df), " rows x ", ncol(df), " columns")

  columns <- tibble::tibble(
    column     = names(df),
    type       = purrr::map_chr(df, \(x) class(x)[1]),
    n_na       = purrr::map_int(df, \(x) sum(is.na(x))),
    n_distinct = purrr::map_int(df, dplyr::n_distinct),
    examples   = purrr::map_chr(df, \(x) paste(head(unique(na.omit(x)), max_examples),
                                               collapse = " | "))
  )

  if (!is.null(divisions) && "sub_reg" %in% names(df)) {
    df <- df |> filter(str_to_lower(str_trim(sub_reg)) %in% divisions)
    msg("  rows in ", paste(divisions, collapse = ", "), ": ", nrow(df))
  }

  out <- list(columns = columns)

  if (all(c("variable_code", "value") %in% names(df))) {

    # All columns that describe the variable (name, group, unit, description ...)
    var_cols <- union("variable_code",
                      grep("variable|unit|desc", names(df), value = TRUE))

    df <- df |>
      mutate(value_chr = as.character(value),
             value_num = suppressWarnings(as.numeric(value_chr)))

    out$variables <- df |>
      group_by(across(all_of(var_cols))) |>
      summarise(
        n_rows        = n(),
        n_na          = sum(is.na(value_chr)),
        n_non_numeric = sum(!is.na(value_chr) & is.na(value_num)),
        n_zero        = sum(value_num == 0, na.rm = TRUE),
        n_negative    = sum(value_num <  0, na.rm = TRUE),
        min           = suppressWarnings(min(value_num, na.rm = TRUE)),
        median        = median(value_num, na.rm = TRUE),
        max           = suppressWarnings(max(value_num, na.rm = TRUE)),
        years         = if ("year" %in% names(df))
                          paste0(min(year, na.rm = TRUE), "–", max(year, na.rm = TRUE)) else NA,
        countries     = if ("country_code" %in% names(df))
                          paste(sort(unique(country_code)), collapse = ",") else NA,
        .groups = "drop"
      ) |>
      arrange(variable_code)

    out$non_numeric <- df |>
      filter(!is.na(value_chr), is.na(value_num)) |>
      count(variable_code, value_chr, name = "n") |>
      arrange(desc(n))

    if (all(c("country_code", "year") %in% names(df)))
      out$coverage <- df |>
        count(country_code, year) |>
        pivot_wider(names_from = year, values_from = n, values_fill = 0,
                    names_sort = TRUE)
  }

  out
}

inspect_aer_file <- function(file, divisions = NULL,
                             data_pattern = "^FS_",
                             note_pattern = "note|readme|info|code") {
  sheets <- readxl::excel_sheets(file)
  msg("File: ", basename(file))
  msg("Sheets: ", paste(sheets, collapse = " | "))

  note_sheets <- sheets[str_detect(sheets, regex(note_pattern, ignore_case = TRUE))]
  data_sheets <- sheets[str_detect(sheets, regex(data_pattern, ignore_case = TRUE)) &
                        !str_detect(sheets, regex("pivot", ignore_case = TRUE))]

  list(
    sheets = sheets,
    notes  = purrr::map(purrr::set_names(note_sheets), \(s)
               readxl::read_excel(file, sheet = s, col_names = FALSE,
                                  .name_repair = "minimal")),
    data   = purrr::map(purrr::set_names(data_sheets), \(s)
               inspect_aer_sheet(file, s, divisions))
  )
}

# Write the inspection tables to CSV files (one per sheet and table)
write_aer_inspection <- function(insp, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  for (s in names(insp$data)) {
    for (tab in names(insp$data[[s]])) {
      f <- file.path(out_dir, paste0(make.names(s), "_", tab, ".csv"))
      readr::write_csv(insp$data[[s]][[tab]], f)
    }
  }
  msg("Inspection tables written to ", out_dir)
}

# ==============================================================================
# ---- FDI ----------------------------------------------------------------------
# ==============================================================================

# Read the CSV outputs of FDI_channel_analysis.R, filtered to the study
# divisions (FDI format, e.g. "7.D"). Returns list(fdi_gearcode, fdi_catches);
# fdi_gearcode is NULL when its CSV is missing, fdi_catches when that CSV is missing.
read_fdi <- function(fdi_files, divisions) {

  read_one <- function(f) {
    readr::read_csv(f, show_col_types = FALSE) |>
      mutate(division = str_to_upper(str_trim(division))) |>
      filter(division %in% divisions)
  }

  fdi_gearcode <- if (file.exists(fdi_files[["effort_gearcode"]])) {
    x <- read_one(fdi_files[["effort_gearcode"]])
    msg("FDI gear-code effort: ", nrow(x), " rows; divisions: ",
        paste(sort(unique(x$division)), collapse = ", "))
    x
  } else {
    msg("FDI: gear-code CSV not found at\n  ", fdi_files[["effort_gearcode"]],
        "\n  Run FDI_channel_analysis.R first. The gear split will not be available.")
    NULL
  }

  fdi_catches <- if (file.exists(fdi_files[["catches"]])) read_one(fdi_files[["catches"]]) else NULL

  list(fdi_gearcode = fdi_gearcode, fdi_catches = fdi_catches)
}

# ==============================================================================
# ---- UK landings (MMO) ----------------------------------------------------------
# ==============================================================================

# Read the MMO landings workbook (one sheet per year), keep UK (and optionally
# Channel Islands) vessels in the study divisions (AER format, e.g. "27.7.d").
# Returns list(uk_landings_raw, uk_landings_summary).
read_uk_mmo <- function(uk_file, years, divisions, gbp_per_eur,
                        include_channel_islands = TRUE) {

  keep_nat <- c("UK - England", "UK - Scotland", "UK - Wales",
                "UK - Northern Ireland",
                if (include_channel_islands) c("Jersey", "Guernsey"))

  all_sheets  <- readxl::excel_sheets(uk_file)
  year_sheets <- all_sheets[str_detect(all_sheets, paste(years, collapse = "|"))]
  msg("UK: sheets to read: ", paste(year_sheets, collapse = " | "))

  uk_list <- vector("list", length(year_sheets))

  for (i in seq_along(year_sheets)) {
    s <- year_sheets[i]

    # Header row: the first row (of the top 20) that contains "nationality"
    top <- readxl::read_excel(uk_file, sheet = s, col_names = FALSE,
                              n_max = 20, .name_repair = "minimal")
    header_row <- which(apply(top, 1, \(r) any(str_detect(
      str_to_lower(as.character(r)), "nationality"), na.rm = TRUE)))[1]

    if (is.na(header_row)) {
      msg("UK: sheet '", s, "' has no header row containing 'nationality'; skipped")
      next
    }

    df <- readxl::read_excel(uk_file, sheet = s, skip = header_row - 1) |>
      clean_names()

    if (!all(c("vessel_nationality", "fao_division") %in% names(df))) {
      msg("UK: sheet '", s, "' lacks vessel_nationality or fao_division; skipped")
      next
    }

    uk_list[[i]] <- df |>
      filter(vessel_nationality %in% keep_nat,
             str_to_lower(str_trim(fao_division)) %in% divisions)
  }

  uk_landings_raw <- dplyr::bind_rows(uk_list) |>
    mutate(
      year          = as.integer(year),
      month         = as.integer(month),
      sub_reg       = str_to_lower(str_trim(fao_division)),
      country_code  = if_else(vessel_nationality %in% c("Jersey", "Guernsey"),
                              "CHI", "GBR"),
      uk_nation     = str_remove(vessel_nationality, "^UK - "),
      vessel_length = map_length_uk(length_group),
      gear_uk       = gear_category,
      gear_category = map_gear_uk(gear_category),
      live_weight_t = live_weight_tonnes,
      value_gbp     = value,
      value_euro    = value / gbp_per_eur[as.character(year)],
      source        = "UK (MMO)"
    ) |>
    filter(year %in% years) |>
    dplyr::select(year, month, country_code, uk_nation, sub_reg, rectangle,
                  port_of_landing, port_nationality, vessel_length,
                  gear_uk, gear_category, species_code, species_name,
                  live_weight_t, value_gbp, value_euro, source)

  # Same structure as aer_landings_summary
  uk_landings_summary <- uk_landings_raw |>
    group_by(year, country_code, gear_category, sub_reg,
             species_code, species_name) |>
    summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE),
              value_euro    = sum(value_euro,    na.rm = TRUE),
              .groups = "drop")

  list(uk_landings_raw = uk_landings_raw, uk_landings_summary = uk_landings_summary)
}

# AER + MMO landings: MMO rows are only added for country x year combinations
# that are not in the AER (no double counting where both sources exist)
combine_landings <- function(aer_landings_summary, uk_landings_summary) {

  aer_country_years <- aer_landings_summary |> distinct(country_code, year)

  uk_added <- uk_landings_summary |>
    anti_join(aer_country_years, by = c("country_code", "year"))

  bind_rows(
    aer_landings_summary |> mutate(source = paste0("AER (", aer_source, ")")),
    uk_added             |> mutate(source = "UK (MMO)")
  ) |>
    mutate(
      species_code_harm = harmonise_species(species_code),
      species_name_harm = coalesce(harmonised_names[species_code_harm], species_name)
    )
}

# ==============================================================================
# ---- GFW ----------------------------------------------------------------------
# ==============================================================================

# Load the GFW pipeline outputs, filtered to the study divisions (GFW format,
# e.g. "27.7.D"). Returns list(fishing_days_study, events_classified_fr,
# vessel_meta, world).
read_gfw <- function(gfw_files, divisions) {

  keep_divs <- function(df, what) {
    if ("division" %in% names(df)) {
      df |> filter(toupper(division) %in% divisions)
    } else {
      msg("GFW: no 'division' column in ", what, "; not filtered by division")
      df
    }
  }

  fishing_days_study   <- load_object(gfw_files[["effort"]], "fishing_days_study") |>
    keep_divs("fishing_days_study")
  events_classified_fr <- load_object(gfw_files[["zones"]], "events_classified_fr") |>
    keep_divs("events_classified_fr")
  vessel_meta          <- load_object(gfw_files[["vessel_meta"]], "vessel_meta")
  world                <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  msg("GFW: ", nrow(events_classified_fr), " fishing events in ",
      paste(divisions, collapse = ", "))

  list(fishing_days_study = fishing_days_study,
       events_classified_fr = events_classified_fr,
       vessel_meta = vessel_meta,
       world = world)
}

# Spatial objects, KDE and concentration metrics for the whole study area.
# Returns a list with:
#   land_study, study_window_proj, study_owin, world_proj, fao_div_proj
#                        spatial objects for the maps and the KDE window
#   kde_results          KDE per year (sample of kde_sample events per year)
#   cell_effort_all      number of events per grid cell and year (all events)
#   concentration_full   Gini coefficient and top-10% cell share per year
build_gfw_spatial <- function(events_classified_fr,
                              fao_sf_division,
                              world,
                              xlim, ylim,
                              years,
                              crs_proj,
                              sigma_all,
                              kde_sample,
                              cell_size) {

  # ---- Base spatial objects ----------------------------------------------------
  land_study <- world %>%
    st_crop(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                      ymin = ylim[1], ymax = ylim[2]), crs = 4326)) %>%
    st_union()

  study_window_sf <- fao_sf_division %>%
    st_union() %>%
    st_difference(land_study) %>%
    st_make_valid()

  study_window_proj <- study_window_sf %>% st_transform(crs_proj)
  study_owin        <- as.owin(study_window_proj)
  world_proj        <- world           %>% st_transform(crs_proj)
  fao_div_proj      <- fao_sf_division %>% st_transform(crs_proj)

  # ---- Event positions in projected coordinates (all events) ----------------------
  events_proj <- events_classified_fr %>%
    mutate(year = lubridate::year(start)) %>%
    filter(year %in% years, !is.na(lon), !is.na(lat)) %>%
    dplyr::select(year, lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs_proj) %>%
    mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
    st_drop_geometry()

  # ---- KDE per year (sample of kde_sample events per year) -------------------------
  events_for_kde_proj <- events_proj %>%
    group_by(year) %>%
    slice_sample(n = kde_sample) %>%
    ungroup()

  kde_one <- function(pts, window, sigma, yr) {
    if (nrow(pts) < 50) return(NULL)
    ppp_yr <- tryCatch(ppp(x = pts$x, y = pts$y, window = window),
                       error = function(e) NULL)
    if (is.null(ppp_yr)) return(NULL)
    kde <- density(ppp_yr, sigma = sigma, dimyx = c(200, 200), edge = TRUE)
    as.data.frame(kde) %>%
      rename(x = x, y = y, density = value) %>%
      filter(!is.na(density), density > 0) %>%
      mutate(ndensity = density / max(density), year = yr)
  }

  kde_results <- map_dfr(years, \(yr)
    kde_one(events_for_kde_proj %>% filter(year == yr), study_owin, sigma_all, yr))

  # ---- Grid-cell effort (all events, not the KDE sample) -----------------------------
  cell_effort_all <- events_proj %>%
    mutate(
      x_cell  = floor(x / cell_size) * cell_size + cell_size / 2,
      y_cell  = floor(y / cell_size) * cell_size + cell_size / 2,
      cell_id = paste(x_cell, y_cell, sep = "_")) %>%
    group_by(year, cell_id, x_cell, y_cell) %>%
    summarise(n_events = n(), .groups = "drop")

  # ---- Concentration metrics per year ------------------------------------------------
  # Gini coefficient and share of effort in the top-10% cells (cells with effort)
  concentration_full <- cell_effort_all %>%
    group_by(year) %>%
    summarise(
      n_cells      = n(),
      total_events = sum(n_events),
      gini         = ineq::Gini(n_events),
      top10_pct    = {
        s     <- sort(n_events, decreasing = TRUE)
        n_top <- max(1L, ceiling(0.10 * length(s)))
        sum(s[seq_len(n_top)]) / sum(s) * 100
      },
      .groups = "drop")

  list(land_study         = land_study,
       study_window_proj  = study_window_proj,
       study_owin         = study_owin,
       world_proj         = world_proj,
       fao_div_proj       = fao_div_proj,
       kde_results        = kde_results,
       cell_effort_all    = cell_effort_all,
       concentration_full = concentration_full)
}

# ==============================================================================
# ---- AER bottom trawl/seine split (FDI gear codes) -------------------------------
# ==============================================================================

# Disaggregates AER Bottom trawl/seine fishing days into gear codes using FDI
# proportional split keys. One row per (year, country_code, vessel_length,
# sub_reg, gear_code) with gear_share, totfishdays_split, split_source and
# gear_type. Returns NULL when fdi_gearcode is NULL.
build_aer_split <- function(aer_effort_summary, fdi_gearcode, countries, years) {

  if (is.null(fdi_gearcode)) {
    msg("AER split: FDI gear-code table not available;",
        " the gear-split figures will be empty.")
    return(NULL)
  }

  # Country name to ISO3 (FDI uses full names, AER uses ISO3)
  cty <- c(BELGIUM = "BEL", DENMARK = "DNK", FRANCE = "FRA", GERMANY = "DEU",
           IRELAND = "IRL", NETHERLANDS = "NLD", SWEDEN = "SWE",
           "UNITED KINGDOM" = "GBR", "EUROPEAN UNION" = NA_character_)

  # Gear categories that belong to the AER "Bottom trawl/seine" category
  bts_gears <- c("Bottom trawl", "Scottish seine", "Danish seine")

  # Primary key: FDI shares per (year, country, vessel_lc, division, gear_code),
  # rescaled to sum to 1 within each stratum (handles suppressed rows)
  fdi_key <- fdi_gearcode |>
    filter(gear_category %in% bts_gears, fishing_days_sum > 0) |>
    mutate(country_code = cty[country]) |>
    filter(!is.na(country_code), year %in% years) |>
    group_by(year, country_code, vessel_lc, division) |>
    mutate(gear_share = fishing_days_sum / sum(fishing_days_sum)) |>
    ungroup() |>
    dplyr::select(year, country_code, vessel_lc, division, gear_code, gear_share)

  # Pooled fallback key: across all countries and years, by vessel_lc only
  fallback_key <- fdi_key |>
    group_by(vessel_lc, gear_code) |>
    summarise(days = sum(gear_share), .groups = "drop") |>
    group_by(vessel_lc) |>
    mutate(gear_share = days / sum(days)) |>
    ungroup() |>
    dplyr::select(vessel_lc, gear_code, gear_share)

  aer_bts <- aer_effort_summary |>
    filter(gear_category == "Bottom trawl/seine",
           year         %in% years,
           country_code %in% countries) |>
    mutate(division = normalise_division(sub_reg))

  aer_joined <- aer_bts |>
    left_join(fdi_key, by = c("year", "country_code",
                              "vessel_length" = "vessel_lc", "division"))

  aer_good    <- aer_joined |> filter(!is.na(gear_share))
  aer_missing <- aer_joined |>
    filter(is.na(gear_share)) |>
    dplyr::select(-gear_code, -gear_share) |>
    left_join(fallback_key, by = c("vessel_length" = "vessel_lc"),
              relationship = "many-to-many") |>
    mutate(gear_share = replace_na(gear_share, 1),
           gear_code  = replace_na(gear_code,  "Unsplit"))

  aer_split <- bind_rows(
      aer_good    |> mutate(split_source = "FDI direct"),
      aer_missing |> mutate(split_source = if_else(gear_code == "Unsplit",
                                                   "Unsplit", "FDI pooled fallback"))
    ) |>
    mutate(
      totfishdays_split = totfishdays * gear_share,
      gear_type = case_when(
        gear_code %in% c("SSC", "SDN")       ~ "Seine (SSC/SDN)",
        gear_code %in% c("OTB", "OTT", "PTB") ~ "Bottom trawl (OTB/OTT/PTB)",
        TRUE                                 ~ "Unsplit"
      )
    )

  # Conservation check
  orig  <- aer_bts |>
    distinct(year, country_code, vessel_length, sub_reg, totfishdays) |>
    pull(totfishdays) |> sum(na.rm = TRUE)
  split <- sum(aer_split$totfishdays_split, na.rm = TRUE)
  ratio <- round(split / orig, 4)
  msg("AER split conservation: original=", round(orig),
      "  split=", round(split), "  ratio=", ratio)
  if (abs(ratio - 1) > 0.001) warning("Conservation check FAILED (ratio=", ratio, ")")

  aer_split
}

# ==============================================================================
# ---- Cache wrapper ----------------------------------------------------------------
# ==============================================================================

# Load `objects` from `cache` when it is up to date; otherwise run `build()`
# (a function returning a named list), save the result and return it.
# The objects are returned as a list; use list2env() to put them in the
# calling environment. Set save_if to a function of the result to skip saving
# (e.g. when an essential input was missing).
with_cache <- function(cache, sources, objects, build, label,
                       save_if = function(res) TRUE) {
  if (cache_ok(cache, sources, objects)) {
    msg(label, ": loading from cache (", format(file.mtime(cache), "%Y-%m-%d %H:%M"), ")")
    e <- new.env()
    load(cache, envir = e)
    return(mget(objects, envir = e))
  }
  msg(label, ": cache missing, stale or incomplete — building ...")
  res <- build()
  if (save_if(res)) {
    e <- list2env(res[objects])
    save(list = objects, envir = e, file = cache)
    msg(label, ": cache saved → ", cache)
  }
  res[objects]
}

# ==============================================================================
# table_helpers
# ==============================================================================

# ---- Shared flextable style -------------------------------------------------
# Call this on any flextable to apply consistent report formatting.
# Adjust font, size and colours once here; all tables update automatically.

style_ft <- function(ft, font_size = 6, font_name = "Calibri", max_width = 6.5) {
  n_rows <- nrow(ft$body$dataset)
  
  ft <- ft |>
    flextable::font(fontname = font_name, part = "all") |>
    flextable::fontsize(size = font_size,     part = "body") |>
    flextable::fontsize(size = font_size + 1, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::bold(j = 1,      part = "body") |>        # bold row label column
    flextable::bold(i = n_rows, part = "body") |>        # bold Total row
    flextable::bg(part = "header", bg = "#D9E1F2") |>
    flextable::bg(i = n_rows, part = "body", bg = "#F2F2F2") |>
    flextable::hline_top(part = "header",
                         border = officer::fp_border(width = 1.5)) |>
    flextable::hline_bottom(part = "header",
                            border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "body",
                            border = officer::fp_border(width = 1.5)) |>
    flextable::colformat_num(big.mark = ",", digits = 0) |>
    flextable::align(align = "left", part = "all") |>
    flextable::autofit()                                 # column widths from the content
  
  # Never wider than the text width of the page: scale all columns down
  # proportionally (long texts then wrap within their cells)
  col_widths <- dim(ft)$widths
  if (sum(col_widths) > max_width) {
    ft <- flextable::width(ft, width = col_widths * max_width / sum(col_widths))
  }
  
  # Keep these widths in Word (no stretching) and place the table on the left
  ft |>
    flextable::set_table_properties(layout = "fixed", align = "left")
}

# ---- Shared table builder ---------------------------------------------------
make_annex_table <- function(df, row_var, col_var, value_var,
                              row_label  = row_var,
                              col_levels = NULL,
                              other_label   = "Other",
                              add_other     = TRUE,
                              add_total_row = TRUE,
                              add_total_col = TRUE,
                              caption       = NULL,
                              font_size     = 8) {

  # Lump unlisted categories into "Other"
  if (add_other && !is.null(col_levels)) {
    df <- df |>
      mutate(!!col_var := if_else(
        .data[[col_var]] %in% col_levels,
        .data[[col_var]], other_label))
  }

  # Summarise and pivot wide
  wide <- df |>
    group_by(.data[[row_var]], .data[[col_var]]) |>
    summarise(val = sum(.data[[value_var]], na.rm = TRUE), .groups = "drop") |>
    mutate(val = round(val)) |>
    pivot_wider(names_from  = all_of(col_var),
                values_from = val,
                values_fill = 0L)

  # Order columns: specified levels first, then Other if present
  col_order <- if (!is.null(col_levels)) {
    present <- col_levels[col_levels %in% names(wide)]
    other_present <- if (add_other && other_label %in% names(wide))
      other_label else character(0)
    c(present, other_present)
  } else {
    setdiff(names(wide), row_var)
  }
  wide <- wide |> dplyr::select(all_of(c(row_var, col_order)))

  # Total column
  num_cols <- col_order   # all non-row columns at this point
  if (add_total_col) {
    wide <- wide |>
      mutate(Total = rowSums(across(all_of(num_cols)), na.rm = TRUE))
    num_cols <- c(num_cols, "Total")
  }

  # Total row — coerce row_var to character to avoid type clash in bind_rows
  if (add_total_row) {
    total_row <- wide |>
      summarise(across(all_of(num_cols), \(x) sum(x, na.rm = TRUE))) |>
      mutate(!!row_var := "Total")          # character; matches after coercion below
    wide <- wide |>
      mutate(!!row_var := as.character(.data[[row_var]])) |>  # coerce before bind
      bind_rows(total_row)
  }

  # Rename row label column
  names(wide)[1] <- row_label

  # Build flextable
  ft <- flextable::flextable(wide) |>
    style_ft(font_size = font_size)

  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption = caption)
  }

  ft
}
