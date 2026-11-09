# ==============================================================================
# Build the report in the MPFF Word template
# ==============================================================================
# 1. fills in the cover page and footers (word_template -> word_build)
# 2. knits the Rmd with officedown into the MPFF styles
# 3. sets the document to update all fields (TOC, cross-references) on opening
#
# Run this script (Source) instead of the Knit button. The Knit button also
# works once word_build/ exists, but then the cover and footers keep the values
# of the last build, and the fields must be updated by hand in Word
# (Ctrl+A, then F9).
#
# Needs: rmarkdown, officedown, officer, flextable, zip
#   install.packages(c("officedown", "officer", "flextable", "zip"))
# ==============================================================================

# ---- 0. set directory ---------------------------------------------------------
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source("report_docx_helpers.R")

rmd_file    <- "template_test.Rmd"
output_file <- "template_test.docx"

# ---- Report details (cover page, citation and footers) ----------------------------
report_info <- list(
  title       = "Fishing in the English Channel",
  subtitle    = "Trends in Fishing Effort, Landings and Spatial distributions between countries and fleets",
  short_title = "Fishing in the English Channel",      # footer
  author      = "Martin Pastoors",
  client      = "Cornelis Vrolijk BV",
  report_no   = "2026-03",
  date        = date_english(Sys.Date())
)

report_info$citation <- paste0(
  "Pastoors, M.A. (", format(Sys.Date(), "%Y"), ") ",
  report_info$title, ". ", report_info$subtitle, ". ",
  "MPFF report ", report_info$report_no, ". ",
  "Commissioned by ", report_info$client, "."
)

# ---- 1. Cover page and footers ---------------------------------------------------------
prepare_word_files(report_info,
                   template_dir = "word_template",
                   build_dir    = "word_build")

# ---- 2. Knit ------------------------------------------------------------------------------
check_docx_writable(output_file)

rmarkdown::render(rmd_file,
                  output_file = output_file,
                  envir       = new.env())

# ---- 3. Update TOC and cross-references when the document is opened -------------------------
docx_update_fields_on_open(output_file)

message("Done: ", normalizePath(output_file))
