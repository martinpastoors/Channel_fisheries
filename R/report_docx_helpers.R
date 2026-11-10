# ==============================================================================
# Helpers to knit an Rmd into the MPFF Word template
# ==============================================================================
# R/word_template/MPFF_report_reference.docx  (shared by all reports)
#     The MPFF report template (.dotx) converted to a .docx, used as
#     reference_docx: all styles, heading numbering, page layout and footers.
#     The footers contain the placeholders {{short_title}} and {{author}}.
# R/reports/<report>/front_<report>.docx  (one per report)
#     Cover page, citation and version history. Contains the placeholders
#     {{title}}, {{subtitle}}, {{author}}, {{date}}, {{client}},
#     {{report_no}} and {{citation}}. The version history table is edited
#     directly in this file in Word.
#
# prepare_word_files() fills in the placeholders and writes both files to the
# report's word_build/ folder, where the Rmd picks them up. Edit the templates in Word if the
# layout should change; keep each {{placeholder}} as one piece of text in one
# formatting (retype it in one go if Word splits it).
# ==============================================================================

# ---- Escape text for use inside Word XML ---------------------------------------
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

# ---- Zip a folder back into a .docx -------------------------------------------------
zip_docx_folder <- function(folder, output) {
  output <- normalizePath(output, mustWork = FALSE)
  if (file.exists(output)) file.remove(output)

  files <- list.files(folder, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip::zip(zipfile = output, files = files, root = folder, mode = "mirror")
  invisible(output)
}

# ---- Replace {{placeholders}} in the body, headers and footers of a .docx -----------
fill_docx_placeholders <- function(template, output, values) {

  if (!file.exists(template)) stop("Template not found: ", template, call. = FALSE)

  # Unzip into an empty temporary folder
  work_dir <- file.path(tempdir(), paste0("docx_", tools::file_path_sans_ext(basename(output))))
  unlink(work_dir, recursive = TRUE)
  dir.create(work_dir, recursive = TRUE)
  utils::unzip(template, exdir = work_dir)

  xml_files <- list.files(file.path(work_dir, "word"),
                          pattern    = "^(document|header[0-9]*|footer[0-9]*)\\.xml$",
                          full.names = TRUE)

  for (f in xml_files) {
    xml <- readChar(f, file.size(f), useBytes = TRUE)

    for (key in names(values)) {
      placeholder <- paste0("{{", key, "}}")
      new_text    <- xml_escape(enc2utf8(as.character(values[[key]])))
      xml <- gsub(placeholder, new_text, xml, fixed = TRUE, useBytes = TRUE)
    }

    # Warn about placeholders that were not filled in
    left_over <- unique(regmatches(xml, gregexpr("\\{\\{[a-z_]+\\}\\}", xml))[[1]])
    if (length(left_over) > 0) {
      warning(basename(template), " (", basename(f), "): no value for ",
              paste(left_over, collapse = ", "), call. = FALSE)
    }

    writeBin(charToRaw(xml), f)
  }

  zip_docx_folder(work_dir, output)
  unlink(work_dir, recursive = TRUE)
  message("Written: ", output)
  invisible(output)
}

# ---- Fill the reference template and the report's front page ------------------------
# reference_template : shared MPFF template (styles, footers), R/word_template/
# front_template     : this report's cover page and version history
# build_dir          : where the filled copies are written (not under Git)
prepare_word_files <- function(values, reference_template, front_template, build_dir) {

  if (!dir.exists(build_dir)) dir.create(build_dir, recursive = TRUE)

  reference <- fill_docx_placeholders(
    template = reference_template,
    output   = file.path(build_dir, "MPFF_report_reference_filled.docx"),
    values   = values)

  front <- fill_docx_placeholders(
    template = front_template,
    output   = file.path(build_dir, "MPFF_report_front_filled.docx"),
    values   = values)

  invisible(c(reference = reference, front = front))
}

# ---- Let Word update all fields (TOC, figure and table references) on opening ---------
# Pandoc does not keep this setting from the reference document, so it is added
# to the knitted file afterwards. Word then asks once whether to update the
# fields; answer Yes.
docx_update_fields_on_open <- function(docx) {

  work_dir <- file.path(tempdir(), "docx_update_fields")
  unlink(work_dir, recursive = TRUE)
  dir.create(work_dir, recursive = TRUE)
  utils::unzip(docx, exdir = work_dir)

  settings_file <- file.path(work_dir, "word", "settings.xml")
  settings <- readChar(settings_file, file.size(settings_file), useBytes = TRUE)

  if (!grepl("updateFields", settings, fixed = TRUE)) {
    settings <- sub("(<w:settings[^>]*>)", "\\1<w:updateFields w:val=\"true\"/>",
                    settings, useBytes = TRUE)
    writeBin(charToRaw(settings), settings_file)
  }

  zip_docx_folder(work_dir, docx)
  unlink(work_dir, recursive = TRUE)
  invisible(docx)
}

# ---- Date in English, independent of the Windows language setting ----------------------
date_english <- function(d = Sys.Date()) {
  paste(as.integer(format(d, "%d")),
        month.name[as.integer(format(d, "%m"))],
        format(d, "%Y"))
}

# ---- Stop early if the output .docx is open in Word -------------------------------------
check_docx_writable <- function(docx) {
  if (!file.exists(docx)) return(invisible(TRUE))
  writable <- tryCatch({
    con <- file(docx, open = "ab")
    close(con)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!writable) {
    stop("Output file is locked (probably open in Word): ", docx,
         "\nClose the document in Word and run again.", call. = FALSE)
  }
  invisible(TRUE)
}
