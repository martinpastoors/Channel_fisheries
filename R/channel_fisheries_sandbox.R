# Channel fisheries sandbox


# ---- Split AER bottom trawl/seine landings into trawl and seine --------------
# Split key: FDI landings shares by gear type, per
# (year, country, vessel length, division, species group).
# Fallback: shares pooled over years and vessel lengths, per
# (country, division, species group).
# UK: demersal trawl and demersal seine are reported directly (MMO).

fdi_catch_csv <- file.path(fdi_dir, "output",
                           "fdi_catches_by_country_sizeclass_gear_division_year.csv")

read.csv(fdi_catch_csv) |> glimpse()

# Defined again here: in load-aer-split it only exists when that cache is rebuilt
cty <- c(BELGIUM="BEL", DENMARK="DNK", FRANCE="FRA", GERMANY="DEU",
         IRELAND="IRL", NETHERLANDS="NLD", SWEDEN="SWE")

gear_type_levels <- c("Bottom trawl (OTB/OTT/PTB)", "Seine (SSC/SDN)")

# ---- 1. FDI landings split key ----------------------------------------------
fdi_land <- read.csv(fdi_catch_csv) |>
  rename(species_code = species,          # <- adjust to actual column name
         landings     = totwghtlandg_sum ) |>  # <- adjust to actual column name
  filter(division      %in% study_divisions_fdi,
         gear_category %in% c("Bottom trawl", "Scottish seine", "Danish seine"),
         year          %in% combined_years,
         landings > 0) |>
  mutate(country_code  = cty[country],
         gear_type     = if_else(gear_category == "Bottom trawl",
                                 gear_type_levels[1], gear_type_levels[2]),
         species_group = assign_species_group(harmonise_species(species_code),
                                              NA_character_) |>
           replace_na("Other")) |>
  filter(!is.na(country_code))

key_primary <- fdi_land |>
  group_by(year, country_code, vessel_lc, division, species_group, gear_type) |>
  summarise(landings = sum(landings), .groups = "drop") |>
  group_by(year, country_code, vessel_lc, division, species_group) |>
  mutate(share = landings / sum(landings)) |>
  ungroup() |>
  dplyr::select(-landings)

key_fallback <- fdi_land |>
  group_by(country_code, division, species_group, gear_type) |>
  summarise(landings = sum(landings), .groups = "drop") |>
  group_by(country_code, division, species_group) |>
  mutate(share = landings / sum(landings)) |>
  ungroup() |>
  dplyr::select(-landings)

# ---- 2. AER bottom trawl/seine landings by species group ---------------------
aer_bts_land <- aer_landings_raw |>
  filter(gear_category == "Bottom trawl/seine",
         year %in% combined_years) |>
  mutate(division      = normalise_division(sub_reg),
         species_group = assign_species_group(harmonise_species(species_code),
                                              species_name) |>
           replace_na("Other")) |>
  group_by(year, country_code, vessel_length, division, sub_reg, species_group) |>
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE), .groups = "drop")

# ---- 3. Apply primary key, then fallback, then leave unsplit -----------------
joined <- aer_bts_land |>
  left_join(key_primary,
            by = c("year", "country_code", "vessel_length" = "vessel_lc",
                   "division", "species_group"))

aer_land_split <- bind_rows(
  joined |> filter(!is.na(share)) |> mutate(split_source = "FDI direct"),
  joined |> filter(is.na(share)) |>
    dplyr::select(-gear_type, -share) |>
    left_join(key_fallback,
              by = c("country_code", "division", "species_group"),
              relationship = "many-to-many") |>
    mutate(split_source = if_else(is.na(share), "Unsplit", "FDI pooled fallback"),
           gear_type    = replace_na(gear_type, "Unsplit"),
           share        = replace_na(share, 1))
) |>
  mutate(live_weight_split = live_weight_t * share)

# ---- 4. Conservation check and coverage --------------------------------------
ratio <- sum(aer_land_split$live_weight_split) / sum(aer_bts_land$live_weight_t)
msg("Landings split conservation ratio: ", round(ratio, 4))
if (abs(ratio - 1) > 0.001) warning("Landings split conservation check FAILED")

land_split_coverage <- aer_land_split |>
  group_by(split_source) |>
  summarise(t = sum(live_weight_split), .groups = "drop") |>
  mutate(pct = round(100 * t / sum(t), 1))
msg("Landings split coverage: ",
    paste0(land_split_coverage$split_source, " ", land_split_coverage$pct, "%",
           collapse = "; "))

# ---- 5. Add UK demersal trawl and seine landings -----------------------------
uk_bts_land <- uk_landings_raw |>
  filter(gear_uk %in% c("Demersal trawls", "Demersal seine"),
         year    %in% combined_years) |>
  mutate(gear_type     = if_else(gear_uk == "Demersal seine",
                                 gear_type_levels[2], gear_type_levels[1]),
         species_group = assign_species_group(harmonise_species(species_code),
                                              species_name) |>
           replace_na("Other")) |>
  group_by(year, country_code, sub_reg, species_group, gear_type) |>
  summarise(live_weight_split = sum(live_weight_t, na.rm = TRUE), .groups = "drop") |>
  mutate(split_source = "UK reported")

bts_landings_by_gear <- bind_rows(
  aer_land_split |>
    dplyr::select(year, country_code, sub_reg, species_group, gear_type,
                  live_weight_split, split_source),
  uk_bts_land
)

bts_landings_by_gear |>
  filter(gear_type != "Unsplit") |>
  filter(country_code %in% c("FRA","GBR","NLD")) %>% 
  mutate(species_group = factor(species_group,
                                levels = c(species_group_levels, "Other")),
         gear_type     = factor(gear_type, levels = gear_type_levels)) |>
  group_by(year, gear_type, species_group, country_code) |>
  summarise(t = sum(live_weight_split, na.rm = TRUE) / 1000, .groups = "drop") |>
  
  ggplot(aes(x = year, y = t, fill = species_group)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values = species_group_colours, name = "Species group") +
  scale_x_continuous(breaks = combined_years) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)),
                     labels = scales::label_comma()) +
  facet_grid(country_code~gear_type, scales="free_y") +
  labs(title    = "Bottom trawl and seine landings by species group, EU + UK",
       subtitle = paste0("Divisions: ", paste(study_divisions_aer, collapse = " & "),
                         "  —  EU split using FDI landings shares; UK as reported"),
       x = "Year", y = "Landed weight (1000 t)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 3))





data_source_colours <- c(AER = "#4E79A7", FDI = "#76B7B2",
                         MMO = "#59A14F", GFW = "#F28E2B")

# ---- Top panel: timeline ----------------------------------------------------
data_timeline <- tibble::tribble(
  ~source, ~label,                                     ~start,                 ~end,
  "AER",   "AER (STECF 25-07)\nEffort and landings, EU fleets",
  aer_years_start,        aer_years_end,
  "FDI",   "FDI (STECF 25-10)\nGear split key (OTB vs SSC/SDN)",
  aer_split_years_start,  aer_years_end,
  "MMO",   "MMO (UK)\nLandings, UK and Channel Islands",
  min(uk_years),          max(uk_years),
  "GFW",   "GFW (AIS)\nSpatial distribution of effort",
  gfw_years_start,        max(study_years)
) |>
  mutate(label = factor(label, levels = rev(label)))

p_time <- ggplot(data_timeline) +
  annotate("rect",
           xmin = min(combined_years) - 0.5, xmax = max(combined_years) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6) +
  annotate("text", x = mean(combined_years), y = Inf,
           label = "EU + UK landings\ncombined", vjust = 1.2, size = 2.6,
           colour = "grey30") +
  geom_vline(xintercept = 2020.5, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 2020.4, y = -Inf, label = "End of Brexit\ntransition",
           hjust = 1, vjust = -0.3, size = 2.6, colour = "grey30") +
  geom_linerange(aes(y = label, xmin = start - 0.45, xmax = end + 0.45,
                     colour = source),
                 linewidth = 9) +
  geom_text(aes(y = label, x = (start + end) / 2,
                label = paste0(start, "\u2013", end)),
            colour = "white", fontface = "bold", size = 3) +
  scale_colour_manual(values = data_source_colours, guide = "none") +
  scale_x_continuous(breaks = aer_years_start:max(study_years),
                     limits = c(aer_years_start - 0.5, max(study_years) + 0.5),
                     expand = expansion(0)) +
  labs(subtitle = "Period covered and role in this report", x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x        = element_text(angle = 90, vjust = 0.5),
        axis.text.y        = element_text(size = 8, hjust = 0),
        panel.grid.major.y = element_blank())

# ---- Bottom panel: content matrix -------------------------------------------
dims <- c("Fleets", "Effort", "Landings", "Gear", "Vessel size",
          "Spatial resolution", "Time resolution")

data_content <- tibble::tribble(
  ~source, ~dimension,           ~status,   ~text,
  "AER",   "Fleets",             "Partial", "EU Member States\n(no UK)",
  "AER",   "Effort",             "Full",    "Fishing days, days at sea,\nkW- and GT-days",
  "AER",   "Landings",           "Full",    "Weight and value\nby species",
  "AER",   "Gear",               "Partial", "Fishing technique\n(trawl + seine combined)",
  "AER",   "Vessel size",        "Full",    "Length classes\n(VL0010\u2013VL40XX)",
  "AER",   "Spatial resolution", "Partial", "ICES division",
  "AER",   "Time resolution",    "Partial", "Annual",
  
  "FDI",   "Fleets",             "Partial", "EU Member States\n(no UK)",
  "FDI",   "Effort",             "Partial", "Fishing days by gear code\n(confidential cells suppressed)",
  "FDI",   "Landings",           "Partial", "EU-aggregated or\nsuppressed",
  "FDI",   "Gear",               "Full",    "Gear code\n(OTB, SSC, SDN \u2026)",
  "FDI",   "Vessel size",        "Full",    "Length classes",
  "FDI",   "Spatial resolution", "Partial", "ICES division\n(spatial tables unavailable)",
  "FDI",   "Time resolution",    "Partial", "Annual / quarterly",
  
  "MMO",   "Fleets",             "Partial", "UK and\nChannel Islands",
  "MMO",   "Effort",             "None",    "Not available",
  "MMO",   "Landings",           "Full",    "Live weight and value\nby species",
  "MMO",   "Gear",               "Full",    "UK gear category\n(trawl and seine separate)",
  "MMO",   "Vessel size",        "Full",    "Length groups",
  "MMO",   "Spatial resolution", "Full",    "ICES rectangle",
  "MMO",   "Time resolution",    "Full",    "Monthly",
  
  "GFW",   "Fleets",             "Full",    "All flags with AIS\n(incl. UK)",
  "GFW",   "Effort",             "Partial", "AIS vessel-days\n(mainly vessels >15 m)",
  "GFW",   "Landings",           "None",    "Not available",
  "GFW",   "Gear",               "Partial", "Inferred from\nAIS behaviour",
  "GFW",   "Vessel size",        "Partial", "GT size classes",
  "GFW",   "Spatial resolution", "Full",    "Positions of\nfishing events",
  "GFW",   "Time resolution",    "Full",    "Daily events"
) |>
  mutate(source    = factor(source, levels = names(data_source_colours)),
         dimension = factor(dimension, levels = rev(dims)),
         status    = factor(status, levels = c("Full", "Partial", "None")))

p_content <- ggplot(data_content, aes(x = source, y = dimension, fill = status)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = text), size = 2.4, lineheight = 0.9) +
  scale_fill_manual(values = c(Full = "#A8D5BA", Partial = "#F6D58E", None = "#E8A3A3"),
                    name = "Coverage") +
  scale_x_discrete(position = "top") +
  labs(subtitle = "Content and resolution", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x     = element_text(face = "bold", size = 10,
                                       colour = data_source_colours),
        panel.grid      = element_blank(),
        legend.position = "bottom")

p_time / p_content +
  plot_layout(heights = c(1, 2.3)) +
  plot_annotation(title = "Data sources used in this report",
                  theme = theme(plot.title = element_text(size = 12, face = "bold")))
