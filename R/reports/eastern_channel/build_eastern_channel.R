# ==============================================================================
# Build: Fishing in the eastern English Channel (ICES 27.7.d)
# ==============================================================================
# 1. fills in the cover page and footers (word_build/)
# 2. knits eastern_channel_report.Rmd with officedown into the MPFF template
# 3. sets the document to update all fields (TOC, cross-references) on opening
#
# Run with Source (Ctrl+Shift+S). All paths are relative to the repository
# root through here::here(), so the working directory does not matter.
#
# Per new version: change report_info$version (and the date row in the
# version history table of front_eastern_channel.docx, edited in Word).
# ==============================================================================

source(here::here("R", "paths.R"))
source(here::here("R", "report_docx_helpers.R"))

report_dir <- here::here("R", "reports", "eastern_channel")

# ---- Report details (cover page, citation, footers, file name) ---------------------
report_info <- list(
  version     = "1.3",
  title       = "Fishing in the eastern English Channel",
  subtitle    = "Trends in fishing effort, landings and spatial distribution of fleets in ICES division 27.7.d",
  short_title = "Fishing in the eastern English Channel",      # footer
  author      = "Martin Pastoors",
  client      = "Cornelis Vrolijk BV",
  report_no   = "2026-04",
  date        = date_english(Sys.Date())
)

report_info$citation <- paste0(
  "Pastoors, M.A. (", format(Sys.Date(), "%Y"), ") ",
  report_info$title, ". ", report_info$subtitle, ". ",
  "MPFF report ", report_info$report_no, ". ",
  "Commissioned by ", report_info$client, "."
)

# ---- Study area and analysis settings (passed to the Rmd as params) -----------------
report_params <- list(
  divisions      = "7.d",
  division_names = "eastern English Channel",
  focus_div      = "7.d",
  study_name     = "eastern English Channel"
)

# ---- Output file -----------------------------------------------------------------------
if (!dir.exists(report_out_dir)) dir.create(report_out_dir, recursive = TRUE)

output_file <- file.path(
  report_out_dir,
  paste0(format(Sys.Date(), "%Y%m%d"), "_Fishing_in_the_eastern_English_Channel_v",
         report_info$version, ".docx")
)

# ---- 1. Cover page and footers -----------------------------------------------------------
prepare_word_files(
  values             = report_info,
  reference_template = here::here("R", "word_template", "MPFF_report_reference.docx"),
  front_template     = file.path(report_dir, "front_eastern_channel.docx"),
  build_dir          = file.path(report_dir, "word_build")
)

# ---- 2. Knit ----------------------------------------------------------------------------------
check_docx_writable(output_file)

rmarkdown::render(
  input       = file.path(report_dir, "eastern_channel_report.Rmd"),
  output_file = basename(output_file),
  output_dir  = dirname(output_file),
  params      = report_params,
  envir       = new.env()
)

# ---- 3. Update TOC and cross-references when the document is opened -------------------------
docx_update_fields_on_open(output_file)

message("Done: ", output_file)
