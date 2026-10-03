# =============================================================================
# STECF FDI Data Analysis – Channel & Southern North Sea Fisheries
# =============================================================================
#
# DATA SOURCES (four FDI tables):
#   Effort   : fishing days and effort metrics per metier × division × quarter
#   Capacity : vessel counts and GT/kW per fleet segment (NO division/quarter)
#   Landings : retained catch weight and value per metier × division × species
#   Catches  : landings + discards per metier × division × species (country-level)
#
# BIOLOGICAL DATA:
#   Kept separate (FDI Landings Age/Length, Discards Age/Length).
#   Not processed in this script — requires species-specific analysis.
#
# KEY DATA-QUALITY ISSUES HANDLED:
#   1. Confidentiality suppression  – "NK", "-1", "CONFID" etc. -> NA + flag
#   2. Mixed-type columns           – everything read as character, coerced safely
#   3. Vessel count aggregation     – vessels in multiple metiers double-counted
#   4. Division labelling           – normalised to "7.D" format
#   5. Duplicate rows               – removed on full natural key
#   6. Gear harmonisation           – DCF codes mapped to broad categories
#   7. Landings EU aggregation      – landings file is EU-level; use catches
#                                     for country-level breakdowns
# =============================================================================

library(tidyverse)
library(janitor)
library(skimr)      # dataset overview: skim() gives type, completeness, distributions

# -----------------------------------------------------------------------------
# 0.  USER SETTINGS
# -----------------------------------------------------------------------------

FDI_DIR    <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/STECF 2025_05_FDI_data"
STUDY_DIVS <- c("4.C", "7.D", "7.E")
OUT_DIR    <- file.path(FDI_DIR, "output")

# Each table type lives in its own subfolder — non-recursive search prevents
# cross-contamination (e.g. "FDI Landings Age.csv" in Biological/ being
# picked up by the landings search).
FDI_EFFORT_DIR   <- file.path(FDI_DIR, "Effort")
FDI_CAPACITY_DIR <- file.path(FDI_DIR, "Capacity")
FDI_LANDINGS_DIR <- file.path(FDI_DIR, "Landings")
FDI_CATCHES_DIR  <- file.path(FDI_DIR, "Catches")
# Biological data not processed in this script:
# FDI_BIOLOGICAL_DIR <- file.path(FDI_DIR, "Biological")

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# =============================================================================
# 1.  HELPER FUNCTIONS
# =============================================================================

#' Read character to numeric, treating all confidentiality sentinels as NA.
#' extra_sentinels: additional exact-match strings to treat as NA.
#' Example: safe_numeric(x, "C") for catches where "C" means confidential.
safe_numeric <- function(x, extra_sentinels = character(0)) {
  base_pat <- "NK|na|N/A|#N/A|CONFID|confidential|BEL|\\."
  if (length(extra_sentinels) > 0) {
    full_pat <- paste0("^(", base_pat, "|",
                       paste(extra_sentinels, collapse = "|"), ")$")
  } else {
    full_pat <- paste0("^(", base_pat, ")$")
  }
  cleaned <- str_trim(x) |>
    str_replace_all(regex(full_pat, ignore_case = TRUE), NA_character_)
  val <- suppressWarnings(as.numeric(cleaned))
  val[!is.na(val) & val == -1] <- NA_real_
  val
}

#' TRUE when a raw string was suppressed (non-empty but coerced to NA).
is_suppressed <- function(raw_chr, numeric_val) {
  is.na(numeric_val) & !is.na(raw_chr) & str_trim(raw_chr) != ""
}

#' Normalise ICES division to upper-case dotted format: "27.7.d" -> "7.D".
normalise_division <- function(x) {
  x <- str_trim(x) |> str_to_upper()
  x <- str_remove(x, "^27\\.")
  x <- str_replace(x, "^(\\d+)([A-Z])$", "\\1.\\2")
  roman_map <- c(
    "^I\\.?"="1.", "^II\\.?"="2.", "^III\\.?"="3.", "^IV\\.?"="4.",
    "^V\\.?"="5.", "^VI\\.?"="6.", "^VII\\.?"="7.", "^VIII\\.?"="8.",
    "^IX\\.?"="9.", "^X\\.?"="10.", "^XI\\.?"="11.", "^XII\\.?"="12."
  )
  for (pat in names(roman_map))
    x <- str_replace(x, regex(pat, ignore_case = TRUE), roman_map[[pat]])
  x
}

#' Map DCF gear codes to broad gear categories.
map_gear_category <- function(gear_code) {
  lookup <- c(
    OTB="Bottom trawl", OTT="Bottom trawl", PTB="Bottom trawl",
    OTM="Midwater trawl", PTM="Midwater trawl",
    TBB="Beam trawl", TBN="Beam trawl",
    SSC="Scottish seine", SDN="Danish seine",
    GNS="Gillnet", GND="Gillnet", GNC="Gillnet",
    GTR="Gillnet", GTN="Gillnet",
    FPN="Pot/trap", FPO="Pot/trap", FYK="Pot/trap",
    LLS="Longline", LLD="Longline", LL="Longline",
    LHP="Handline/rod", LHM="Handline/rod", LTL="Handline/rod",
    DRB="Dredge", HMD="Dredge",
    PS="Purse seine", LA="Purse seine",
    MIS="Other/mixed", NK="Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(gear_code)),
                !!!lookup, .default = "Other/mixed")
}

#' Map DCF fishing_technique codes (capacity table) to broad gear categories.
map_fishing_technique <- function(ftec) {
  lookup <- c(
    DTS="Bottom trawl", TBB="Beam trawl", PEL="Midwater trawl",
    DFN="Gillnet", PS="Purse seine", LL="Longline",
    HOK="Handline/rod", FPO="Pot/trap", DRB="Dredge",
    INACTIVE="Inactive", NK="Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(ftec)),
                !!!lookup, .default = "Other/mixed")
}

#' Standardise vessel length-class labels to DCF bracket codes.
harmonise_vessel_lc <- function(x) {
  str_trim(x) |> str_to_upper() |>
    str_replace_all(c(
      "VL06-12"="VL0612", "VL0012"="VL0612",
      "VL12-24"="VL1224", "VL0024"="VL1224",
      "VL24-40"="VL2440",
      "VL>40"="VL40XX", "VL>=40"="VL40XX",
      "^NK$"="UNKNOWN"
    ))
}

#' Find a column by regex patterns; returns first match or NA.
find_col <- function(df, patterns) {
  hits <- names(df)[str_detect(names(df),
                               regex(paste(patterns, collapse = "|"),
                                     ignore_case = TRUE))]
  if (length(hits) == 0) NA_character_ else hits[1]
}

#' Standard CSV loader for FDI files — reads all as character to handle
#' mixed confidential/numeric columns, then calls clean_names().
load_fdi_csv <- function(dir, pattern, label) {
  files <- list.files(dir, pattern = regex(pattern, ignore_case = TRUE),
                      recursive = FALSE, full.names = TRUE)
  if (length(files) == 0) {
    message(label, ": no file found in ", dir, " — skipping.")
    return(NULL)
  }
  message(label, " file(s) found:\n  ",
          paste(basename(files), collapse = "\n  "))
  out <- map_dfr(
    files,
    ~ read_csv(.x,
               col_types      = cols(.default = col_character()),
               na             = c("", "NA", "na", "N/A", "#N/A", "NK",
                                  "CONFID", "confidential", "-", "."),
               locale         = locale(encoding = "UTF-8"),
               show_col_types = FALSE),
    .id = "source_file"
  ) |> clean_names()
  message(label, ": ", nrow(out), " rows, ", ncol(out), " columns")
  out
}

# =============================================================================
# 2.  LOAD RAW DATA — one section per table
# =============================================================================

# -----------------------------------------------------------------------------
# 2a. EFFORT
# -----------------------------------------------------------------------------

raw_effort <- load_fdi_csv(FDI_EFFORT_DIR, "FDI.*Effort.*\\.csv", "Effort")

message("\n--- Effort raw overview ---")
skim(raw_effort)

# -----------------------------------------------------------------------------
# 2b. CAPACITY
# -----------------------------------------------------------------------------

raw_capacity <- load_fdi_csv(FDI_CAPACITY_DIR, "FDI.*Capacity.*\\.csv", "Capacity")

message("\n--- Capacity raw overview ---")
skim(raw_capacity)

# -----------------------------------------------------------------------------
# 2c. LANDINGS
# NOTE: The FDI landings file is EU-aggregated (country = "European Union").
#       It is useful for total landed weight/value by species in the study area
#       but cannot be broken down by member state. Use the catches table
#       (Section 2d) for country-level analysis.
# -----------------------------------------------------------------------------

raw_landings <- load_fdi_csv(FDI_LANDINGS_DIR, "FDI.*Landings.*\\.csv", "Landings")

if (!is.null(raw_landings)) {
  message("\n--- Landings raw overview ---")
  skim(raw_landings)
}

# -----------------------------------------------------------------------------
# 2d. CATCHES
# NOTE: Country-level table with landings AND discards by species.
#       This is the primary source for country-level landings analysis.
# -----------------------------------------------------------------------------

raw_catches <- load_fdi_csv(FDI_CATCHES_DIR, "FDI.*Catches.*\\.csv", "Catches")

if (!is.null(raw_catches)) {
  message("\n--- Catches raw overview ---")
  skim(raw_catches)
}

# =============================================================================
# 3.  AUTO-DETECT COLUMNS
# =============================================================================

# ---- 3a. Effort ----
e_year    <- find_col(raw_effort, c("^year$", "^ref_year$"))
e_country <- find_col(raw_effort, c("^country$", "^country_code$", "^ms$"))
e_quarter <- find_col(raw_effort, c("^quarter$", "^qtr$"))
e_vlc     <- find_col(raw_effort, c("vessel_length", "length_cat",
                                     "sizeclass", "length_class"))
e_gear    <- find_col(raw_effort, c("^gear_type$", "^gear$"))
e_ftec    <- find_col(raw_effort, c("^fishing_technique$", "^fish_tech$"))
e_div     <- find_col(raw_effort, c("^sub_region$", "^division$"))
e_eez     <- find_col(raw_effort, c("^eez"))
e_days    <- find_col(raw_effort, c("^total_fishing_days$", "^fishing_days$",
                                     "^days_at_sea$"))
e_target  <- find_col(raw_effort, c("^target_assemblage$"))
e_mesh    <- find_col(raw_effort, c("^mesh_size_range$", "^mesh_size$"))
e_confid  <- find_col(raw_effort, c("^confidential$"))

message(
  "\nEffort columns:",
  "\n  year=", e_year, "  country=", e_country,
  "  quarter=", ifelse(is.na(e_quarter), "NA", e_quarter),
  "\n  vessel_lc=", e_vlc, "  gear=", e_gear, "  ftec=", e_ftec,
  "\n  division=", e_div, "  eez=", e_eez, "  days=", e_days,
  "\n  target=", ifelse(is.na(e_target), "NA", e_target),
  "  mesh=", ifelse(is.na(e_mesh), "NA", e_mesh),
  "  confid=", ifelse(is.na(e_confid), "NA", e_confid)
)

# ---- 3b. Capacity ----
c_year    <- find_col(raw_capacity, c("^year$"))
c_country <- find_col(raw_capacity, c("^country$", "^ms$"))
c_vlc     <- find_col(raw_capacity, c("vessel_length", "length_cat"))
c_ftec    <- find_col(raw_capacity, c("^fishing_technique$"))
c_vessels <- find_col(raw_capacity, c("^no_vessels$", "^total_vessels$",
                                       "^num_vessels$"))
c_confid  <- find_col(raw_capacity, c("^confidential$"))

message(
  "\nCapacity columns:",
  "\n  year=", c_year, "  country=", c_country,
  "  vessel_lc=", c_vlc, "  ftec=", c_ftec,
  "  vessels=", c_vessels, "  confid=", ifelse(is.na(c_confid), "NA", c_confid)
)

# ---- 3c. Landings ----
if (!is.null(raw_landings)) {
  l_year    <- find_col(raw_landings, c("^year$"))
  l_country <- find_col(raw_landings, c("^country$", "^ms$"))
  l_quarter <- find_col(raw_landings, c("^quarter$"))
  l_vlc     <- find_col(raw_landings, c("vessel_length", "length_cat"))
  l_gear    <- find_col(raw_landings, c("^gear_type$", "^gear$"))
  l_ftec    <- find_col(raw_landings, c("^fishing_technique$"))
  l_div     <- find_col(raw_landings, c("^sub_region$", "^division$"))
  l_species <- find_col(raw_landings, c("^species$"))
  l_wgt     <- find_col(raw_landings, c("^totwghtlandg$",
                                         "tot_live_weight_landed",
                                         "^landing_weight$"))
  l_val     <- find_col(raw_landings, c("^totvallandg$",
                                         "tot_value_landings",
                                         "^landing_value$"))
  l_confid  <- find_col(raw_landings, c("^confidential$"))
  message(
    "\nLandings columns:",
    "\n  year=", l_year, "  country=", l_country,
    "  quarter=", ifelse(is.na(l_quarter), "NA", l_quarter),
    "\n  gear=", ifelse(is.na(l_gear), "NA", l_gear),
    "  division=", l_div,
    "  species=", ifelse(is.na(l_species), "NA", l_species),
    "\n  weight=", ifelse(is.na(l_wgt), "NA", l_wgt),
    "  value=", ifelse(is.na(l_val), "NA", l_val),
    "  confid=", ifelse(is.na(l_confid), "NA", l_confid)
  )
}

# ---- 3d. Catches ----
if (!is.null(raw_catches)) {
  ca_year    <- find_col(raw_catches, c("^year$"))
  ca_country <- find_col(raw_catches, c("^country$", "^ms$"))
  ca_quarter <- find_col(raw_catches, c("^quarter$"))
  ca_vlc     <- find_col(raw_catches, c("vessel_length", "length_cat",
                                         "vessel_lc"))
  ca_gear    <- find_col(raw_catches, c("^gear_type$", "^gear$"))
  ca_ftec    <- find_col(raw_catches, c("^fishing_tech", "^fishing_technique$"))
  ca_div     <- find_col(raw_catches, c("^sub_region$", "^division$"))
  ca_eez     <- find_col(raw_catches, c("^eez"))
  ca_species <- find_col(raw_catches, c("^species$"))
  ca_landings<- find_col(raw_catches, c("^totwghtlandg$",
                                         "^total_live_weight_landed",
                                         "^landing_weight$"))
  ca_discards<- find_col(raw_catches, c("^tot_discards_tonnes$",
                                         "^discards$", "^total_discards$"))
  ca_value   <- find_col(raw_catches, c("^total_value_of_landings$",
                                         "^totvallandg$", "^landing_value$"))
  ca_confid  <- find_col(raw_catches, c("^confidential$"))
  message(
    "\nCatches columns:",
    "\n  year=", ca_year, "  country=", ca_country,
    "  quarter=", ifelse(is.na(ca_quarter), "NA", ca_quarter),
    "\n  vessel_lc=", ifelse(is.na(ca_vlc), "NA", ca_vlc),
    "  gear=", ifelse(is.na(ca_gear), "NA", ca_gear),
    "  ftec=", ifelse(is.na(ca_ftec), "NA", ca_ftec),
    "\n  division=", ca_div, "    eez=", ca_eez,
    "  species=", ifelse(is.na(ca_species), "NA", ca_species),
    "\n  landings=", ifelse(is.na(ca_landings), "NA", ca_landings),
    "  discards=", ifelse(is.na(ca_discards), "NA", ca_discards),
    "  value=", ifelse(is.na(ca_value), "NA", ca_value),
    "  confid=", ifelse(is.na(ca_confid), "NA", ca_confid)
  )
}

# =============================================================================
# 4.  CLEAN EFFORT
# =============================================================================

effort_raw2 <- raw_effort |>
  rename(
    year              = all_of(e_year),
    country           = all_of(e_country),
    vessel_lc         = all_of(e_vlc),
    gear_code         = all_of(e_gear),
    fishing_technique = all_of(e_ftec),
    division_raw      = all_of(e_div),
    eez_raw           = all_of(e_eez),
    fishing_days_raw  = all_of(e_days),
    target_assemblage = all_of(e_target),
    mesh_size_range   = all_of(e_mesh)
  )

if (!is.na(e_quarter)) {
  effort_raw2 <- effort_raw2 |> rename(quarter = all_of(e_quarter))
} else {
  effort_raw2 <- effort_raw2 |> mutate(quarter = NA_character_)
}

effort <- effort_raw2 |>
  mutate(
    year              = as.integer(year),
    quarter           = as.integer(quarter),
    country           = str_to_upper(str_trim(country)),
    vessel_lc         = harmonise_vessel_lc(vessel_lc),
    gear_code         = str_to_upper(str_trim(gear_code)),
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    gear_category     = map_gear_category(gear_code),
    division          = normalise_division(division_raw),
    eez               = eez_raw,
    fishing_days      = safe_numeric(fishing_days_raw),
    days_suppressed   = if (!is.na(e_confid))
      str_to_upper(str_trim(.data[[e_confid]])) %in% c("Y", "A", "C")
    else
      is_suppressed(fishing_days_raw, fishing_days)
  )

# Deduplicate on full natural key
n_before <- nrow(effort)
effort2 <- effort |>
  group_by(year, quarter, country, vessel_lc,
           fishing_technique, gear_code,
           target_assemblage, mesh_size_range,
           division, eez, geo_indicator,
           specon_tech, deep) %>% 
  mutate(n=n())



# effort <- effort |>
#   distinct(year, quarter, country, vessel_lc,
#            fishing_technique, gear_code,
#            target_assemblage, mesh_size_range,
#            division, eez_indicator, geo_indicator,
#            specon_tech, deep,
#            .keep_all = TRUE)
message("Effort: removed ", n_before - nrow(effort),
        " duplicate rows. Remaining: ", nrow(effort))

# Filter to study divisions
effort_study <- effort |> filter(division %in% STUDY_DIVS)
message("Effort rows in study area: ", nrow(effort_study))

message("\n--- Effort cleaned overview (study area) ---")
skim(effort_study)

# =============================================================================
# 5.  CLEAN CAPACITY
# =============================================================================

capacity <- raw_capacity |>
  rename(
    year              = all_of(c_year),
    country           = all_of(c_country),
    vessel_lc         = all_of(c_vlc),
    fishing_technique = all_of(c_ftec),
    n_vessels_raw     = all_of(c_vessels)
  ) |>
  mutate(
    year              = as.integer(year),
    country           = str_to_upper(str_trim(country)),
    vessel_lc         = harmonise_vessel_lc(vessel_lc),
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    gear_category     = map_fishing_technique(fishing_technique),
    n_vessels         = safe_numeric(n_vessels_raw),
    vessels_suppressed = if (!is.na(c_confid))
      str_to_upper(str_trim(.data[[c_confid]])) %in% c("Y", "A", "C")
    else
      is_suppressed(n_vessels_raw, n_vessels)
  )

message("Capacity cleaned: ", nrow(capacity), " rows")
message("\n--- Capacity cleaned overview ---")
skim(capacity)

# =============================================================================
# 6.  CLEAN LANDINGS
# =============================================================================

if (!is.null(raw_landings)) {

  landings_renames <- c(year=l_year, country=l_country,
                         vessel_lc=l_vlc, gear_code=l_gear, division_raw=l_div)
  if (!is.na(l_ftec)) landings_renames <- c(landings_renames,
                                             fishing_technique=l_ftec)

  landings_raw2 <- raw_landings |> rename(any_of(landings_renames))

  if (!"fishing_technique" %in% names(landings_raw2))
    landings_raw2 <- landings_raw2 |> mutate(fishing_technique = NA_character_)

  if (!is.na(l_species)) landings_raw2 <- landings_raw2 |>
    rename(species = all_of(l_species))
  if (!is.na(l_wgt))     landings_raw2 <- landings_raw2 |>
    rename(totwghtlandg = all_of(l_wgt))
  if (!is.na(l_val))     landings_raw2 <- landings_raw2 |>
    rename(totvallandg = all_of(l_val))
  if (!is.na(l_quarter)) {
    landings_raw2 <- landings_raw2 |> rename(quarter = all_of(l_quarter))
  } else {
    landings_raw2 <- landings_raw2 |> mutate(quarter = NA_character_)
  }

  landings_clean <- landings_raw2 |>
    mutate(
      year              = as.integer(year),
      quarter           = as.integer(quarter),
      country           = str_to_upper(str_trim(country)),
      vessel_lc         = harmonise_vessel_lc(vessel_lc),
      gear_code         = str_to_upper(str_trim(gear_code)),
      fishing_technique = str_to_upper(str_trim(fishing_technique)),
      gear_category     = map_gear_category(gear_code),
      division          = normalise_division(division_raw),
      # Detect suppression BEFORE coercing to numeric — uses raw character value
      landing_suppressed = if (!is.na(l_confid))
        str_to_upper(str_trim(.data[[l_confid]])) %in% c("Y", "A", "C")
      else if ("totwghtlandg" %in% names(landings_raw2))
        str_trim(totwghtlandg) == "C"
      else
        FALSE,
      # Now coerce to numeric (safe_numeric treats "C" as NA)
      totwghtlandg      = if ("totwghtlandg" %in% names(landings_raw2))
                            safe_numeric(totwghtlandg, "C") else NA_real_,
      totvallandg       = if ("totvallandg" %in% names(landings_raw2))
                            safe_numeric(totvallandg,  "C") else NA_real_
    )

  # Collapse age/length rows to one row per stratum x species
  # (weight/value are repeated on every biological row — keep only first)
  land_key <- c("year", "quarter", "country", "vessel_lc",
                "fishing_technique", "gear_code", "division")
  if ("species" %in% names(landings_clean)) land_key <- c(land_key, "species")

  n_before <- nrow(landings_clean)
  landings <- landings_clean |>
    select(all_of(c(land_key, "gear_category", "totwghtlandg",
                    "totvallandg", "landing_suppressed", "division_raw"))) |>
    distinct(across(all_of(land_key)), .keep_all = TRUE)

  message("Landings: collapsed ", n_before - nrow(landings),
          " rows to ", nrow(landings), " strata")

  landings_study <- landings |> filter(division %in% STUDY_DIVS)
  message("Landings study area rows: ", nrow(landings_study),
          " | weight non-NA: ",
          sum(!is.na(landings_study$totwghtlandg)))

  message("\n--- Landings cleaned overview (study area) ---")
  skim(landings_study)

  # Top 15 species by weight
  if ("species" %in% names(landings_study)) {
    message("\nTop 15 species by landed weight (study area):")
    landings_study |>
      group_by(species) |>
      summarise(total_t = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") |>
      slice_max(total_t, n = 15) |>
      print(n = 15)
  }

} else {
  landings <- landings_study <- NULL
  message("Landings: not loaded.")
}

# =============================================================================
# 7.  CLEAN CATCHES
# =============================================================================

if (!is.null(raw_catches)) {

  catches_renames <- c(year=ca_year, country=ca_country,
                        vessel_lc=ca_vlc, gear_code=ca_gear, division_raw=ca_div)
  if (!is.na(ca_ftec)) catches_renames <- c(catches_renames,
                                             fishing_technique=ca_ftec)

  catches_raw2 <- raw_catches |> rename(any_of(catches_renames))

  if (!"fishing_technique" %in% names(catches_raw2))
    catches_raw2 <- catches_raw2 |> mutate(fishing_technique = NA_character_)

  if (!is.na(ca_species))  catches_raw2 <- catches_raw2 |>
    rename(species = all_of(ca_species))
  if (!is.na(ca_landings)) catches_raw2 <- catches_raw2 |>
    rename(totwghtlandg = all_of(ca_landings))
  if (!is.na(ca_discards)) catches_raw2 <- catches_raw2 |>
    rename(discards = all_of(ca_discards))
  if (!is.na(ca_value))    catches_raw2 <- catches_raw2 |>
    rename(totvallandg = all_of(ca_value))
  if (!is.na(ca_quarter)) {
    catches_raw2 <- catches_raw2 |> rename(quarter = all_of(ca_quarter))
  } else {
    catches_raw2 <- catches_raw2 |> mutate(quarter = NA_character_)
  }

  catches <- catches_raw2 |>
    mutate(
      year              = as.integer(year),
      quarter           = as.integer(quarter),
      country           = str_to_upper(str_trim(country)),
      vessel_lc         = harmonise_vessel_lc(vessel_lc),
      gear_code         = str_to_upper(str_trim(gear_code)),
      fishing_technique = str_to_upper(str_trim(fishing_technique)),
      gear_category     = map_gear_category(gear_code),
      division          = normalise_division(division_raw),
      # "C" in value columns = confidential in the catches table
      # "A" in the confidential column = aggregated/suppressed (not "Y")
      totwghtlandg      = if ("totwghtlandg" %in% names(catches_raw2))
                            safe_numeric(totwghtlandg, "C") else NA_real_,
      totvallandg       = if ("totvallandg" %in% names(catches_raw2))
                            safe_numeric(totvallandg,  "C") else NA_real_,
      discards          = if ("discards" %in% names(catches_raw2))
                            safe_numeric(discards,      "C") else NA_real_,
      total_catch       = coalesce(totwghtlandg, 0) + coalesce(discards, 0),
      catch_suppressed  = if (!is.na(ca_confid))
        str_to_upper(str_trim(.data[[ca_confid]])) %in% c("Y", "A", "C")
      else
        # Infer suppression from "C" in the weight column when no confid col
        str_trim(.data[[ca_landings]]) == "C" 
    )

  catch_key <- c("year", "quarter", "country", "vessel_lc",
                 "fishing_technique", "gear_code", "division")
  if ("species" %in% names(catches)) catch_key <- c(catch_key, "species")

  n_before <- nrow(catches)
  catches <- catches |>
    distinct(across(all_of(catch_key)), .keep_all = TRUE)
  message("Catches: removed ", n_before - nrow(catches),
          " duplicates. Remaining: ", nrow(catches))

  catches_study <- catches |> filter(division %in% STUDY_DIVS)
  message("Catches study area rows: ", nrow(catches_study))


  # QA: report what confidential sentinel values appear in catches
  message("\nConfidential column values in catches:")
  if (!is.na(ca_confid))
    print(catches |> count(.data[[ca_confid]], sort = TRUE) |> head(10))
  message("Catch_suppressed: ",
          sum(catches$catch_suppressed, na.rm = TRUE), " / ", nrow(catches),
          " rows (", round(100 * mean(catches$catch_suppressed, na.rm = TRUE), 1), "%)")

  message("\n--- Catches cleaned overview (study area) ---")
  skim(catches_study)

  # Country coverage check
  message("\nCatches — countries in study area:")
  catches_study |> count(country, sort = TRUE) |> print(n = 30)

  # Top 15 species by landed weight (country-level)
  if ("species" %in% names(catches_study)) {
    message("\nTop 15 species by landed weight — catches table (study area):")
    catches_study |>
      group_by(species) |>
      summarise(
        total_landings_t = sum(totwghtlandg, na.rm = TRUE),
        total_discards_t = sum(discards,     na.rm = TRUE),
        .groups = "drop"
      ) |>
      slice_max(total_landings_t, n = 15) |>
      print(n = 15)
  }

} else {
  catches <- catches_study <- NULL
  message("Catches: not loaded.")
}

# =============================================================================
# 8.  EFFORT UNITS INVESTIGATION
# =============================================================================

effort_metric_cols <- names(effort_study)[str_detect(
  names(effort_study),
  regex("^total_days|^fishing_days$|^total_k_w|^total_gt|^hours_at_sea|^k_w_hours|^gt_hours",
        ignore_case = TRUE)
)]
message("\nEffort metric columns: ", paste(effort_metric_cols, collapse = ", "))

effort_metrics_num <- effort_study |>
  mutate(across(any_of(c("total_days_at_sea",
                          "total_k_w_days_at_sea", "total_gt_days_at_sea",
                          "total_k_w_fishing_days", "total_gt_fishing_days",
                          "hours_at_sea", "k_w_hours_at_sea", "gt_hours_at_sea")),
                safe_numeric))

effort_units <- effort_metrics_num |>
  summarise(across(
    any_of(c("fishing_days", "total_days_at_sea",
             "total_k_w_days_at_sea", "total_gt_days_at_sea",
             "total_k_w_fishing_days", "total_gt_fishing_days",
             "hours_at_sea", "k_w_hours_at_sea", "gt_hours_at_sea")),
    list(n_nonNA = ~sum(!is.na(.x)),
         pct_NA  = ~round(100*mean(is.na(.x)), 1),
         median  = ~median(.x, na.rm = TRUE),
         total   = ~sum(.x, na.rm = TRUE)),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(), names_to=c("metric","stat"), names_sep="__") |>
  pivot_wider(names_from=stat, values_from=value) |>
  arrange(pct_NA, metric)

message("\nEffort metric coverage (study area):")
print(effort_units, n = 20)

# =============================================================================
# 9.  SUMMARIES
# =============================================================================

# ---- 9a. Fishing days ----
days_summary <- effort_study |>
  group_by(year, country, vessel_lc, gear_category, division) |>
  summarise(
    fishing_days_sum      = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
    n_quarters_suppressed = sum(days_suppressed, na.rm = TRUE),
    days_complete         = all(!days_suppressed),
    .groups = "drop"
  ) |>
  mutate(days_note = case_when(
    n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
    !days_complete            ~ "Partial: >=1 quarter NA",
    TRUE                      ~ "Complete"
  )) |>
  arrange(year, country, vessel_lc, gear_category, division)

message("Days summary: ", nrow(days_summary), " rows")

# ---- 9b. Fishing days by gear code ----
days_by_gearcode <- effort_study |>
  group_by(year, country, vessel_lc, fishing_technique, gear_code, division) |>
  summarise(
    fishing_days_sum      = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
    n_quarters_suppressed = sum(days_suppressed, na.rm = TRUE),
    days_complete         = all(!days_suppressed),
    .groups = "drop"
  ) |>
  mutate(
    gear_category = map_gear_category(gear_code),
    days_note = case_when(
      n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
      !days_complete            ~ "Partial: >=1 quarter NA",
      TRUE                      ~ "Complete"
    )
  ) |>
  arrange(year, country, vessel_lc, fishing_technique, gear_code, division)

message("Days by gear code: ", nrow(days_by_gearcode), " rows")

# ---- 9c. Vessels ----
vessels_summary <- capacity |>
  filter(country %in% unique(effort_study$country)) |>
  group_by(year, country, vessel_lc, fishing_technique, gear_category) |>
  summarise(
    n_vessels_sum               = sum(n_vessels, na.rm = TRUE),
    n_strata                    = n(),
    n_strata_suppressed_vessels = sum(vessels_suppressed, na.rm = TRUE),
    vessels_complete            = all(!vessels_suppressed),
    .groups = "drop"
  ) |>
  mutate(vessels_note = case_when(
    n_strata_suppressed_vessels > 0 ~ "Partial: >=1 stratum confidential",
    !vessels_complete               ~ "Partial: >=1 stratum NA",
    n_strata > 1                    ~ "Sum across strata — possible double-count",
    TRUE                            ~ "Complete"
  )) |>
  arrange(year, country, vessel_lc, fishing_technique, gear_category)

message("Vessels summary: ", nrow(vessels_summary), " rows")

# ---- 9d. Vessels + fishing days joined ----
joined_summary <- effort_study |>
  group_by(year, country, vessel_lc, fishing_technique,
           gear_category, gear_code, division) |>
  summarise(
    fishing_days_sum    = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported = n_distinct(quarter[!is.na(quarter)]),
    days_suppressed_any = any(days_suppressed, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    capacity |>
      group_by(year, country, vessel_lc, fishing_technique) |>
      summarise(n_vessels         = sum(n_vessels, na.rm = TRUE),
                vessels_suppressed = any(vessels_suppressed, na.rm = TRUE),
                .groups = "drop"),
    by = c("year", "country", "vessel_lc", "fishing_technique")
  ) |>
  mutate(
    vessels_note = case_when(
      is.na(n_vessels) & vessels_suppressed ~ "Confidential",
      is.na(n_vessels)                      ~ "Not in capacity table",
      vessels_suppressed                    ~ "Partial",
      TRUE                                  ~ "Complete"
    )
  ) |>
  arrange(year, country, vessel_lc, gear_category, division)

message("Joined summary: ", nrow(joined_summary), " rows")

# ---- 9e. Landings summary ----
if (!is.null(landings_study)) {
  land_grp <- c("year", "country", "vessel_lc", "gear_category", "division")
  if ("species" %in% names(landings_study)) land_grp <- c(land_grp, "species")

  landings_summary <- landings_study |>
    group_by(across(all_of(land_grp))) |>
    summarise(
      totwghtlandg_sum      = sum(totwghtlandg, na.rm = TRUE),
      totvallandg_sum       = sum(totvallandg,  na.rm = TRUE),
      n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
      n_quarters_suppressed = sum(landing_suppressed, na.rm = TRUE),
      landing_complete      = all(!landing_suppressed),
      .groups = "drop"
    ) |>
    mutate(landing_note = case_when(
      n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
      !landing_complete         ~ "Partial: >=1 quarter NA",
      TRUE                      ~ "Complete"
    )) |>
    arrange(year, country, vessel_lc, gear_category, division)

  message("Landings summary: ", nrow(landings_summary), " rows")
} else {
  landings_summary <- NULL
}

# ---- 9f. Catches summary ----
if (!is.null(catches_study)) {
  catch_grp <- c("year", "country", "vessel_lc", "gear_category", "division")
  if ("species" %in% names(catches_study)) catch_grp <- c(catch_grp, "species")

  catches_summary <- catches_study |>
    group_by(across(all_of(catch_grp))) |>
    summarise(
      totwghtlandg_sum      = sum(totwghtlandg, na.rm = TRUE),
      totvallandg_sum       = sum(totvallandg,  na.rm = TRUE),
      discards_sum          = sum(discards,      na.rm = TRUE),
      total_catch_sum       = sum(total_catch,   na.rm = TRUE),
      n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
      n_quarters_suppressed = sum(catch_suppressed, na.rm = TRUE),
      catch_complete        = all(!catch_suppressed),
      .groups = "drop"
    ) |>
    mutate(
      discard_rate = round(discards_sum / pmax(total_catch_sum, 0.001), 3),
      catch_note   = case_when(
        n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
        !catch_complete           ~ "Partial: >=1 quarter NA",
        TRUE                      ~ "Complete"
      )
    ) |>
    arrange(year, country, vessel_lc, gear_category, division)

  message("Catches summary: ", nrow(catches_summary), " rows")

  # EU vs country landings comparison
  if (!is.null(landings_study) && nrow(landings_study) > 0) {
    eu_total      <- landings_study |>
      group_by(year, division) |>
      summarise(wgt_eu = sum(totwghtlandg, na.rm=TRUE), .groups="drop")
    country_total <- catches_study |>
      group_by(year, division) |>
      summarise(wgt_country = sum(totwghtlandg, na.rm=TRUE), .groups="drop")

    comparison_eu_country <- left_join(eu_total, country_total,
                                        by=c("year","division")) |>
      mutate(ratio    = round(wgt_country / wgt_eu, 3),
             diff_t   = round(wgt_country - wgt_eu, 1),
             pct_diff = round(100 * (wgt_country - wgt_eu) / wgt_eu, 1))

    message("\nEU landings vs country catches (study area, first 20 rows):")
    comparison_eu_country |> arrange(division, year) |> print(n = 20)
    write_csv(comparison_eu_country,
              file.path(OUT_DIR, "qa_eu_vs_country_landings.csv"))
  }

  # Discard rates
  message("\nDiscard rates by gear category (study area, all years):")
  catches_study |>
    group_by(gear_category) |>
    summarise(
      landings_t  = sum(totwghtlandg, na.rm=TRUE),
      discards_t  = sum(discards,     na.rm=TRUE),
      total_t     = landings_t + discards_t,
      discard_pct = round(100 * discards_t / pmax(total_t, 0.001), 1),
      .groups = "drop"
    ) |>
    arrange(desc(total_t)) |>
    print(n = 20)

} else {
  catches_summary <- NULL
}

# =============================================================================
# 10.  QA CHECKS
# =============================================================================

qa <- list()

qa$effort_suppression <- effort_study |>
  group_by(year, country) |>
  summarise(
    n_strata       = n(),
    n_suppressed   = sum(days_suppressed, na.rm=TRUE),
    pct_suppressed = round(100 * n_suppressed / n_strata, 1),
    .groups = "drop"
  ) |>
  filter(n_suppressed > 0) |>
  arrange(desc(pct_suppressed))

qa$unmapped_gears <- bind_rows(
  effort_study |> distinct(gear_code, gear_category) |> mutate(source="effort"),
  capacity     |> distinct(fishing_technique, gear_category) |>
    rename(gear_code=fishing_technique) |> mutate(source="capacity")
) |> distinct() |>
  filter(gear_category %in% c("Other/mixed", "Unknown")) |>
  arrange(source, gear_code)

qa$coverage <- effort_study |>
  distinct(year, country) |>
  arrange(year, country)

message("\n--- QA ---")
message("Years:     ", paste(sort(unique(effort_study$year)), collapse=", "))
message("Countries: ", paste(sort(unique(effort_study$country)), collapse=", "))
message("Suppressed effort strata: ",
        sum(effort_study$days_suppressed, na.rm=TRUE), " / ",
        nrow(effort_study))
if (nrow(qa$unmapped_gears) > 0) {
  message("Unmapped gear codes: ",
          paste(qa$unmapped_gears$gear_code, collapse=", "))
}

# =============================================================================
# 11.  WRITE OUTPUTS
# =============================================================================

write_csv(days_summary,
  file.path(OUT_DIR, "fdi_fishingdays_by_country_sizeclass_gear_division_year.csv"))
write_csv(days_by_gearcode,
  file.path(OUT_DIR, "fdi_fishingdays_by_country_sizeclass_gearcode_division_year.csv"))
write_csv(vessels_summary,
  file.path(OUT_DIR, "fdi_vessels_by_country_sizeclass_gear_year.csv"))
write_csv(joined_summary,
  file.path(OUT_DIR, "fdi_vessels_and_days_by_country_sizeclass_gear_division_year.csv"))

if (!is.null(landings_summary))
  write_csv(landings_summary,
    file.path(OUT_DIR, "fdi_landings_by_country_sizeclass_gear_division_year.csv"))

if (!is.null(catches_summary))
  write_csv(catches_summary,
    file.path(OUT_DIR, "fdi_catches_by_country_sizeclass_gear_division_year.csv"))

write_csv(qa$effort_suppression,
  file.path(OUT_DIR, "qa_effort_suppression_rate.csv"))
write_csv(qa$coverage,
  file.path(OUT_DIR, "qa_data_coverage.csv"))
if (nrow(qa$unmapped_gears) > 0)
  write_csv(qa$unmapped_gears,
    file.path(OUT_DIR, "qa_unmapped_gear_codes.csv"))

message("\nOutputs written to: ", normalizePath(OUT_DIR))

# =============================================================================
# 12.  DIAGNOSTIC PLOTS
# =============================================================================

library(ggplot2)

# Fishing days by country, gear and division
p_days <- days_summary |>
  group_by(year, country, division, gear_category) |>
  summarise(fishing_days_sum=sum(fishing_days_sum),
            complete=all(days_complete), .groups="drop") |>
  ggplot(aes(x=year, y=fishing_days_sum,
             colour=country, linetype=division)) +
  geom_line() +
  geom_point(aes(shape=complete), size=2) +
  scale_shape_manual(values=c("TRUE"=16,"FALSE"=1),
                     labels=c("TRUE"="Complete","FALSE"="Partial"),
                     name="Data") +
  scale_x_continuous(breaks=seq(2013, 2024, by=2)) +
  facet_wrap(~gear_category, scales="free_y") +
  labs(title="FDI — Fishing days",
       subtitle=paste("Divisions:", paste(STUDY_DIVS, collapse=" & ")),
       x="Year", y="Fishing days") +
  theme_bw() + theme(strip.text=element_text(size=7))

ggsave(file.path(OUT_DIR, "plot_fishingdays_by_year_country_gear.png"),
       p_days, width=14, height=8)

# Catches by country and gear (if available)
if (!is.null(catches_summary)) {
  p_catches <- catches_summary |>
    group_by(year, country, gear_category, division) |>
    summarise(totwghtlandg_sum=sum(totwghtlandg_sum, na.rm=TRUE),
              .groups="drop") |>
    filter(country %in% c("FRANCE","UNITED KINGDOM","NETHERLANDS","BELGIUM")) |>
    ggplot(aes(x=year, y=totwghtlandg_sum/1000,
               colour=country, linetype=division)) +
    geom_line() + geom_point(size=1.5) +
    scale_x_continuous(breaks=seq(2013, 2024, by=2)) +
    facet_wrap(~gear_category, scales="free_y") +
    labs(title="FDI — Landed weight (catches table)",
         subtitle=paste("Divisions:", paste(STUDY_DIVS, collapse=" & ")),
         x="Year", y="Landed weight (1000 t)") +
    theme_bw() + theme(strip.text=element_text(size=7))

  ggsave(file.path(OUT_DIR, "plot_catches_by_year_country_gear.png"),
         p_catches, width=14, height=8)
}

message("\nDone.")


# sandbox

landings_summary |>
  group_by(year, country, species) |>
  summarise(totwghtlandg_sum=sum(totwghtlandg_sum, na.rm=TRUE),
            .groups="drop") |>
  filter(species %in% c("CTC")) |>
  ggplot(aes(x=year, y=totwghtlandg_sum/1000,
             colour=country)) +
  geom_line() + geom_point(size=1.5) +
  scale_x_continuous(breaks=seq(2013, 2024, by=2)) +
  facet_wrap(~species) +
  theme_bw() + theme(strip.text=element_text(size=7))

catches_summary |>
  group_by(year, country, species) |>
  summarise(totwghtlandg_sum=sum(totwghtlandg_sum, na.rm=TRUE),
            .groups="drop") |>
  filter(country %in% c("FRANCE","UNITED KINGDOM","NETHERLANDS","BELGIUM")) |>
  filter(species %in% c("CTC")) |>
  ggplot(aes(x=year, y=totwghtlandg_sum,
             colour=country)) +
  geom_line() + geom_point(size=1.5) +
  scale_x_continuous(breaks=seq(2013, 2024, by=2)) +
  facet_wrap(~species) +
  theme_bw() + theme(strip.text=element_text(size=7))


days_by_gearcode <- read.csv(file.path(OUT_DIR, "fdi_fishingdays_by_country_sizeclass_gearcode_division_year.csv"))
study_divisions_fdi <- c("4.C", "7.D", "7.E")

# 1. Compute FDI fraction within the combined Bottom trawl/seine group
fdi_split_key <- days_by_gearcode |>
  filter(division %in% study_divisions_fdi,
         gear_category %in% c("Bottom trawl", "Scottish seine", "Danish seine"),
         fishing_days_sum > 0) |>
  # Relabel to match the AER combined category for the join key
  mutate(aer_gear_category = "Bottom trawl/seine") |>
  group_by(year, country, vessel_lc, division, aer_gear_category) |>
  mutate(
    total_days = sum(fishing_days_sum, na.rm = TRUE),
    gear_share = fishing_days_sum / total_days
  ) |>
  ungroup()

# Country name to ISO3 lookup
country_lookup <- c(
  "BELGIUM"        = "BEL",
  "DENMARK"        = "DNK",
  "FRANCE"         = "FRA",
  "GERMANY"        = "DEU",
  "IRELAND"        = "IRL",
  "NETHERLANDS"    = "NLD",
  "SWEDEN"         = "SWE",
  "UNITED KINGDOM" = "GBR",
  "EUROPEAN UNION" = NA_character_   # drop — no AER counterpart
)

fdi_split_key_iso <- fdi_split_key |>
  mutate(country_code = country_lookup[country]) |>
  filter(!is.na(country_code))        # drop EU rows

# Build a fallback split key: pooled share across all countries and years
# for each vessel_lc + division combination
fallback_key <- fdi_split_key_iso |>
  group_by(vessel_lc, division, gear_code) |>
  summarise(fishing_days_sum = sum(fishing_days_sum, na.rm = TRUE),
            .groups = "drop") |>
  group_by(vessel_lc, division) |>
  mutate(
    total_days = sum(fishing_days_sum),
    gear_share = fishing_days_sum / total_days
  ) |>
  ungroup()

fallback_key_pooled <- fdi_split_key_iso |>
  group_by(vessel_lc, gear_code) |>
  summarise(fishing_days_sum = sum(fishing_days_sum, na.rm=TRUE),
            .groups="drop") |>
  group_by(vessel_lc) |>
  mutate(gear_share = fishing_days_sum / sum(fishing_days_sum)) |>
  ungroup()

# Split into hardcoded and non-hardcoded BEFORE the join
fallback_input <- aer_split |>
  filter(gear_code == "OTB/SSC (unsplit)") |>
  dplyr::select(-gear_code, -gear_share, -totfishdays_split) |>
  mutate(use_hardcode = country_code == "BEL" & vessel_length == "VL2440")

# Part 1: Belgium VL2440 hardcoded as OTB, single row per stratum
fallback_hardcoded <- fallback_input |>
  filter(use_hardcode) |>
  dplyr::select(-use_hardcode) |>
  mutate(
    gear_code         = "OTB",
    gear_share        = 1,
    totfishdays_split = totfishdays,
    split_source      = "Hardcoded (BEL beam trawl)"
  )

# Part 2: everything else — expand via pooled fallback key
fallback_expanded <- fallback_input |>
  filter(!use_hardcode) |>
  dplyr::select(-use_hardcode) |>
  left_join(
    fallback_key_pooled |>
      dplyr::select(vessel_lc, gear_code, gear_share),
    by = c("vessel_length" = "vessel_lc"),
    relationship = "many-to-many"
  ) |>
  mutate(
    totfishdays_split = totfishdays * gear_share,
    split_source      = if_else(year < 2013,
                                "Pre-FDI period (pooled fallback)",
                                "FDI pooled fallback")
  )

# Combine all three parts
aer_split_filled <- bind_rows(
  aer_split_good,
  fallback_hardcoded,
  fallback_expanded
)

# Conservation check
original_total <- aer_split_filled |>
  distinct(year, country_code, vessel_length, sub_reg, totfishdays) |>
  pull(totfishdays) |> sum(na.rm=TRUE)

split_total <- aer_split_filled |>
  pull(totfishdays_split) |> sum(na.rm=TRUE)

cat("Original (deduplicated):", round(original_total), "\n")
cat("Split total:            ", round(split_total),    "\n")
cat("Ratio:                  ", round(split_total / original_total, 4), "\n")



# Final summary: fishing days by gear code
aer_split_filled |>
  group_by(gear_code, split_source) |>
  summarise(
    fishdays_t = round(sum(totfishdays_split, na.rm=TRUE)),
    .groups    = "drop"
  ) |>
  mutate(pct = round(100 * fishdays_t / sum(fishdays_t), 1)) |>
  arrange(desc(fishdays_t))

# Trend over time: OTB vs seine (SSC/SDN) share
aer_split_filled |>
  mutate(gear_type = case_when(
    gear_code %in% c("SSC", "SDN") ~ "Seine (SSC/SDN)",
    gear_code %in% c("OTB", "OTT", "PTB") ~ "Bottom trawl (OTB/OTT/PTB)",
    TRUE ~ "Unsplit"
  )) |>
  group_by(year, gear_type) |>
  summarise(fishdays = sum(totfishdays_split, na.rm=TRUE), .groups="drop") |>
  group_by(year) |>
  mutate(pct = round(100 * fishdays / sum(fishdays), 1)) |>
  arrange(year, gear_type) |>
  print(n=40)

# By country: who is OTB vs SSC/SDN dominated
aer_split_filled |>
  mutate(gear_type = case_when(
    gear_code %in% c("SSC", "SDN") ~ "Seine (SSC/SDN)",
    gear_code %in% c("OTB", "OTT", "PTB") ~ "Bottom trawl (OTB/OTT/PTB)",
    TRUE ~ "Unsplit"
  )) |>
  group_by(country_code, gear_type) |>
  summarise(fishdays = sum(totfishdays_split, na.rm=TRUE), .groups="drop") |>
  group_by(country_code) |>
  mutate(pct = round(100 * fishdays / sum(fishdays), 1)) |>
  arrange(country_code, gear_type)

library(ggplot2)

aer_split_filled |>
  filter(year < 2024) |>   # exclude anomalous 2024
  mutate(gear_type = case_when(
    gear_code %in% c("SSC", "SDN") ~ "Seine (SSC/SDN)",
    gear_code %in% c("OTB", "OTT", "PTB") ~ "Bottom trawl (OTB/OTT/PTB)",
    TRUE ~ "Unsplit"
  )) |>
  group_by(year, gear_type) |>
  summarise(fishdays = sum(totfishdays_split, na.rm=TRUE), .groups="drop") |>
  ggplot(aes(x=year, y=fishdays, fill=gear_type)) +
  geom_col(position="stack", width=0.8) +
  scale_fill_manual(
    values = c(
      "Bottom trawl (OTB/OTT/PTB)" = "#E15759",
      "Seine (SSC/SDN)"             = "#4E79A7",
      "Unsplit"                     = "#BAB0AC"
    ),
    name = "Gear type"
  ) +
  scale_x_continuous(breaks = seq(2008, 2023, by=2)) +
  scale_y_continuous(limits = c(0,NA),
                     expand = expansion(mult=c(0,0.05)),
                     labels = scales::label_comma()) +
  labs(
    title    = "AER — Bottom trawl/seine disaggregated by gear code",
    subtitle = paste0("Divisions: ", paste(study_divisions_aer, collapse=" & "),
                      "  —  Source: FDI gear-code split applied to AER fishing days"),
    caption  = "Pre-2013: pooled FDI fallback applied. 2024 excluded (incomplete FDI data).",
    x = "Year", y = "Fishing days"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust=1))

aer_split_filled |>
  filter(year >= 2013,
         year < 2024,
         country_code %in% c("FRA", "NLD"),
         split_source == "FDI gear-code split") |>
  mutate(gear_type = case_when(
    gear_code %in% c("SSC", "SDN") ~ "Seine (SSC/SDN)",
    TRUE                            ~ "Bottom trawl (OTB/OTT/PTB)"
  )) |>
  group_by(year, country_code, gear_type) |>
  summarise(fishdays = sum(totfishdays_split, na.rm=TRUE), .groups="drop") |>
  group_by(year, country_code) |>
  mutate(pct = 100 * fishdays / sum(fishdays)) |>
  ungroup() |>
  mutate(country_gear = paste0(country_code, "\n", gear_type)) |>
  ggplot(aes(x=year, y=pct, fill=interaction(gear_type, country_code))) +
  geom_col(position="fill", width=0.8) +
  scale_fill_manual(
    values = c(
      "Bottom trawl (OTB/OTT/PTB).FRA" = "#E15759",
      "Seine (SSC/SDN).FRA"             = "#ff9ea0",
      "Bottom trawl (OTB/OTT/PTB).NLD" = "#4E79A7",
      "Seine (SSC/SDN).NLD"             = "#a0c4e8"
    ),
    labels = c(
      "Bottom trawl (OTB/OTT/PTB).FRA" = "FRA — Bottom trawl",
      "Seine (SSC/SDN).FRA"             = "FRA — Seine",
      "Bottom trawl (OTB/OTT/PTB).NLD" = "NLD — Bottom trawl",
      "Seine (SSC/SDN).NLD"             = "NLD — Seine"
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks=seq(2013, 2023, by=1)) +
  scale_y_continuous(labels=scales::label_percent(scale=100),
                     expand=expansion(mult=c(0,0.02))) +
  labs(
    title    = "AER — Bottom trawl vs seine share: FRA and NLD combined",
    subtitle = paste0("Divisions: ", paste(study_divisions_aer, collapse=" & "),
                      "  —  FDI gear-code split applied to AER fishing days"),
    caption  = "Percentage of total FRA + NLD bottom trawl/seine fishing days.",
    x = "Year", y = "Share of fishing days (%)"
  ) +
  theme_bw() +
  theme(axis.text.x   = element_text(angle=45, hjust=1),
        legend.position = "bottom")
