# =============================================================================
# UK Data Analysis – Channel & Southern North Sea Fisheries (CATCHES ONLY)
# =============================================================================

library(tidyverse)
library(readxl)
library(readODS)

UK_file <- "C:/TEMP/SFS24_UK_into_all_ports_non_uk_into_uk_ports_landings_2020_24.xlsx"

# df <- readxl::read_xlsx (UK_file, sheet = "2020")

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


# ── Landings ──────────────────────────────────────────────────────────────────
tmp <- bind_rows(
  read_excel(UK_file, sheet = "2020") |> clean_names(),
  read_excel(UK_file, sheet = "2021") |> clean_names(),
  read_excel(UK_file, sheet = "2022") |> clean_names(),
  read_excel(UK_file, sheet = "2023") |> clean_names(),
  read_excel(UK_file, sheet = "2024") |> clean_names()
) 

tmp |> count(vessel_nationality)                     # UK labels and Crown dependencies
tmp |> filter(str_detect(vessel_nationality, "^UK")) |>
  count(fao_division) |> print(n = Inf)               # division spelling and case
tmp |> count(length_group)                            # full set of length classes
tmp |> count(gear_category)                           # full set of gear labels
aer_landings_raw |> distinct(fishing_tech, gear_category)   # AER labels to match

# UK species codes in the study area that don't occur in the AER
tmp |>
  filter(str_detect(vessel_nationality, "^UK"),
         str_to_lower(fao_division) %in% study_divisions_aer) |>
  distinct(species_code, species_name) |>
  anti_join(aer_landings_raw |> distinct(species_code), by = "species_code")

uk_study <- tmp |>
  filter(str_detect(vessel_nationality, "^UK"),
         str_to_lower(fao_division) %in% study_divisions_aer)

# Tonnage of the codes missing from the AER, with all 23 printed
uk_study |>
  anti_join(aer_landings_raw |> distinct(species_code), by = "species_code") |>
  group_by(species_code, species_name) |>
  summarise(t = sum(live_weight_tonnes), .groups = "drop") |>
  mutate(pct_of_uk_total = 100 * t / sum(uk_study$live_weight_tonnes)) |>
  arrange(desc(t)) |> print(n = Inf)

# Top species on each side, to spot codes that differ for the same species
full_join(
  uk_study |> group_by(species_code) |>
    summarise(uk_t = sum(live_weight_tonnes)),
  aer_landings_raw |> filter(year >= 2020) |> group_by(species_code) |>
    summarise(aer_t = sum(live_weight_t, na.rm = TRUE)),
  by = "species_code"
) |>
  arrange(desc(coalesce(uk_t, 0) + coalesce(aer_t, 0))) |>
  print(n = 40)

landings <-
  tmp |>
  filter(is.na(value)) |>
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
