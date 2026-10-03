# =============================================================================
# STECF AER Data Analysis – Channel & Southern North Sea Fisheries
# =============================================================================

library(tidyverse)
library(readxl)

# AER_FILE <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/STECF 2025-07_AER_data/STECF_25 07_EU Fleet Economic and Transversal data_fleet segment level.xlsx"

# # ── Tab names ────────────────────────────────────────────────────────────────
# all_tabs <- excel_sheets(AER_FILE)
# message("Tabs in file:")
# print(all_tabs)
# 
# # ── Helper: inspect one tab ──────────────────────────────────────────────────
# inspect_tab <- function(file, sheet) {
#   df <- read_excel(file, sheet = sheet, n_max = 5)
#   message("\n=== Tab: '", sheet, "' ===")
#   message("Dimensions (first 5 rows): ", nrow(df), " x ", ncol(df))
#   message("Column names:")
#   print(names(df))
#   message("First 5 rows:")
#   print(df, width = 200)
#   message("Column types:")
#   print(sapply(df, class))
# }
# 
# # ── Inspect each relevant tab ────────────────────────────────────────────────
# inspect_tab(AER_FILE, "FS_effort by sub-region ")
# inspect_tab(AER_FILE, "FS_landings FAO 2008-2012")
# inspect_tab(AER_FILE, "FS_landings FAO 2013-2017")
# inspect_tab(AER_FILE, "FS_landings FAO 2018-2023")
# 
# 
# # Fixed inventory — convert everything to character before pivoting
# for (sheet in c("FS_effort by sub-region ",
#                 "FS_landings FAO 2008-2012",
#                 "FS_landings FAO 2013-2017",
#                 "FS_landings FAO 2018-2023")) {
#   df <- read_excel(AER_FILE, sheet = sheet, n_max = 2000)
#   message("\n=== Full inventory: '", sheet, "' ===")
#   message("Rows (sample of 2000): ", nrow(df))
#   
#   # Build inventory manually to avoid type conflicts
#   inventory <- map_dfr(names(df), function(col) {
#     x <- df[[col]]
#     tibble(
#       column   = col,
#       type     = class(x)[1],
#       n_unique = n_distinct(x, na.rm = TRUE),
#       n_NA     = sum(is.na(x)),
#       pct_NA   = round(100 * mean(is.na(x)), 1),
#       example  = as.character(x[!is.na(x)][1])
#     )
#   })
#   print(inventory, n = 60)
# }
# 
# 
# # ── variable_code values in effort tab ───────────────────────────────────────
# effort <- read_excel(AER_FILE, sheet = "FS_effort by sub-region ")
# message("\nEffort: ", nrow(effort), " total rows")
# message("\nEffort variable_codes:")
# effort |> count(variable_code, variable_name, unit, sort = TRUE) |> print(n = 30)
# message("\nEffort sub_reg values (", n_distinct(effort$sub_reg), " unique):")
# sort(unique(effort$sub_reg)) |> print()
# message("\nEffort year range:")
# print(table(effort$year))
# message("\nEffort countries:")
# effort |> count(country_code, sort = TRUE) |> print(n = 30)
# 
# # ── landings tabs ─────────────────────────────────────────────────────────────
# for (sheet in c("FS_landings FAO 2008-2012",
#                 "FS_landings FAO 2013-2017",
#                 "FS_landings FAO 2018-2023")) {
#   df <- read_excel(AER_FILE, sheet = sheet)
#   message("\n'", sheet, "': ", nrow(df), " total rows")
#   message("variable_codes:")
#   df |> count(variable_code, variable_name, unit, sort = TRUE) |> print(n = 10)
#   message("Year range:")
#   print(table(df$year))
#   message("sub_reg (", n_distinct(df$sub_reg), " unique):")
#   print(sort(unique(df$sub_reg)))
#   message("Countries (", n_distinct(df$country_code), "):")
#   df |> count(country_code, sort = TRUE) |> print(n = 30)
# }
# 
# # ── Species code overlap between tabs ────────────────────────────────────────
# l1 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2008-2012") |>
#   distinct(species_code, species_name)
# l2 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2013-2017") |>
#   distinct(species_code, species_name)
# l3 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2018-2023") |>
#   distinct(species_code, species_name)
# message("\nTotal unique species: ",
#         nrow(l1), " (2008-12) / ", nrow(l2), " (2013-17) / ", nrow(l3), " (2018-23)")
# message("In 2008-2012 but not 2013-2017:")
# anti_join(l1, l2, by = "species_code") |> print(n = 30)
# message("In 2013-2017 but not 2018-2023:")
# anti_join(l2, l3, by = "species_code") |> print(n = 30)
# 
# # ── Species code overlap between tabs ────────────────────────────────────────
# l1 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2008-2012") |>
#   distinct(species_code, species_name)
# l2 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2013-2017") |>
#   distinct(species_code, species_name)
# l3 <- read_excel(AER_FILE, sheet = "FS_landings FAO 2018-2023") |>
#   distinct(species_code, species_name)
# 
# message("\nSpecies in 2008-2012 not in 2013-2017:")
# anti_join(l1, l2, by = "species_code") |> print(n = 20)
# message("\nSpecies in 2013-2017 not in 2018-2023:")
# anti_join(l2, l3, by = "species_code") |> print(n = 20)
# message("\nTotal unique species per tab: ",
#         nrow(l1), " / ", nrow(l2), " / ", nrow(l3))


# =============================================================================
# STECF AER Data Analysis — Channel Fisheries
# Source: STECF 25-07 EU Fleet Economic and Transversal data
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)

AER_FILE <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/STECF 2025-07_AER_data/STECF_25 07_EU Fleet Economic and Transversal data_fleet segment level.xlsx"
STUDY_DIVS <- c("27.7.d", "27.7.e", "27.4.c")   # full format as in sub_reg
OUT_DIR    <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/output"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

map_gear <- function(x) {
  dplyr::recode(str_to_upper(str_trim(x)),
                DTS="Bottom trawl/seine", TBB="Beam trawl",
                PEL="Midwater trawl",     DFN="Gillnet",
                HOK="Handline/rod",       FPO="Pot/trap",
                DRB="Dredge",             PS="Purse seine",
                LL="Longline",
                .default = "Other/mixed"
  )
}

# ── Effort ────────────────────────────────────────────────────────────────────
effort <- read_excel(AER_FILE, sheet = "FS_effort by sub-region ") |>
  clean_names() |>
  filter(!is.na(variable_code), !is.na(value)) |>
  mutate(year         = as.integer(year),
         sub_reg      = str_to_lower(str_trim(sub_reg)),
         gear_category = map_gear(fishing_tech)) |>
  select(year, country_code, sub_reg, fishing_tech,
         vessel_length, gear_category, fs_name,
         variable_code, value) |>
  pivot_wider(names_from = variable_code, values_from = value,
              values_fn = sum) |>
  filter(sub_reg %in% STUDY_DIVS)

# ── Landings ──────────────────────────────────────────────────────────────────
landings <- bind_rows(
  read_excel(AER_FILE, sheet = "FS_landings FAO 2008-2012") |> clean_names(),
  read_excel(AER_FILE, sheet = "FS_landings FAO 2013-2017") |> clean_names(),
  read_excel(AER_FILE, sheet = "FS_landings FAO 2018-2023") |> clean_names()
) |>
  filter(!is.na(variable_code), !is.na(value)) |>
  mutate(year         = as.integer(year),
         sub_reg      = str_to_lower(str_trim(sub_reg)),
         gear_category = map_gear(fishing_tech)) |>
  select(year, country_code, sub_reg, fishing_tech,
         vessel_length, gear_category,
         species_code, species_name,
         variable_code, value) |>
  pivot_wider(names_from = variable_code, values_from = value,
              values_fn = sum) |>
  filter(sub_reg %in% STUDY_DIVS) |>
  rename(live_weight_kg = totwghtlandg,
         value_euro     = totvallandg) |>
  mutate(live_weight_t = live_weight_kg / 1000)

# ── Summaries ─────────────────────────────────────────────────────────────────
effort_summary <- effort |>
  group_by(year, country_code, gear_category, sub_reg) |>
  summarise(across(any_of(c("totfishdays","totseadays",
                            "totkwfishdays","totgtfishdays")),
                   ~sum(.x, na.rm = TRUE)),
            .groups = "drop")

landings_summary <- landings |>
  group_by(year, country_code, gear_category,
           sub_reg, species_code, species_name) |>
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE),
            value_euro    = sum(value_euro,    na.rm = TRUE),
            .groups = "drop")

# ── Write outputs ─────────────────────────────────────────────────────────────
write_csv(effort_summary,
          file.path(OUT_DIR, "aer_effort_by_country_gear_division_year.csv"))
write_csv(landings_summary,
          file.path(OUT_DIR, "aer_landings_by_country_gear_division_species_year.csv"))

message("Done. Effort: ", nrow(effort_summary),
        " rows. Landings: ", nrow(landings_summary), " rows.")

# =============================================================================
# 5.  DIAGNOSTIC PLOTS
# =============================================================================

library(ggplot2)

# Fishing days trend
effort_summary |>
  filter(country_code %in% c("FRA", "GBR", "NLD", "BEL", "IRL", "DNK")) |>
  group_by(year, country_code, gear_category) |>
  summarise(totfishdays = sum(totfishdays, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = totfishdays,
             colour = country_code)) +
  geom_line() + geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 2)) +
  facet_wrap(~ gear_category, scales = "free_y") +
  labs(title    = "AER — Fishing days by gear category",
       subtitle = paste("Divisions:", paste(STUDY_DIVS, collapse = " & ")),
       x = "Year", y = "Fishing days", colour = "Country") +
  theme_bw() +
  theme(strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_DIR, "aer_plot_fishingdays_by_gear_country.png"),
       width = 14, height = 8)

# Top 10 species landed weight trend
top10_species <- landings_summary |>
  group_by(species_code) |>
  summarise(total_t = sum(live_weight_t), .groups = "drop") |>
  slice_max(total_t, n = 10) |>
  pull(species_code)

landings_summary |>
  filter(grepl("squid|cuttle", tolower(species_name)),
         country_code %in% c("FRA", "GBR", "NLD", "BEL")) |>
  # filter(species_code %in% c("CTC","SQR","MUL","GUG"),
  #        country_code %in% c("FRA", "GBR", "NLD", "BEL")) |>
  # filter(species_code %in% top10_species,
  #        country_code %in% c("FRA", "GBR", "NLD", "BEL")) |>
  group_by(year, country_code, species_code, species_name) |>
  summarise(live_weight_t = sum(live_weight_t), .groups = "drop") |>
  ggplot(aes(x = year, y = live_weight_t,
             colour = country_code)) +
  geom_line() + geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 2)) +
  facet_wrap(~ species_name, scales = "free_y") +
  labs(title    = "AER — Landed weight top 10 species",
       subtitle = paste("Divisions:", paste(STUDY_DIVS, collapse = " & ")),
       x = "Year", y = "Landed weight (t)", colour = "Country") +
  theme_bw() +
  theme(strip.text      = element_text(size = 7),
        axis.text.x     = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_DIR, "aer_plot_landings_top10species.png"),
       width = 16, height = 10)





landings_summary |>
  filter(country_code %in% c("FRA", "GBR", "NLD", "BEL")) |>
  filter(year < 2024) %>% 
  mutate(
    ceph_group = case_when(
      str_detect(tolower(species_name), "cuttlefish|sepia") |
        species_code %in% c("CTC","CTL","CTZ","CTB","CTS","SEB","OCC") ~ "Cuttlefish",
      str_detect(tolower(species_name), "flying squid|ommastrep|shortfin") |
        species_code %in% c("SQI","OMS","TOD","NBS","OMP","ILL") ~ "Flying squid",
      str_detect(tolower(species_name), "squid|loligo|bobtail") |
        species_code %in% c("SQC","SQR","SQE","SQM","LOL","LOG",
                            "SQS","SQZ","SQX","EUR","BOC") ~ "Squid",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(ceph_group)) |>
  group_by(year, country_code, ceph_group) |>
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE),
            .groups = "drop") |>
  # Ensure all country x year combinations exist (fill gaps with 0)
  complete(year, country_code, ceph_group,
           fill = list(live_weight_t = 0)) |>
  mutate(country_code = factor(country_code,
                               levels = c("FRA", "GBR", "NLD", "BEL"))) |>
  ggplot(aes(x = year, y = live_weight_t,
             fill = country_code)) +
  geom_area(position = "stack", alpha = 0.8, colour = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2008, 2024, by = 2)) +
  facet_wrap(~ ceph_group, scales = "free_y", nrow = 1) +
  labs(title    = "AER — Cuttlefish and squid landings",
       subtitle = paste("Divisions:", paste(STUDY_DIVS, collapse = " & ")),
       caption  = "Countries stacked additively",
       x = "Year", y = "Landed weight (t)",
       fill = "Country") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


ggsave(file.path(OUT_DIR, "aer_plot_landings_top10species.png"),
       width = 16, height = 10)

message("Done.")
