# ==============================================================================
# Paths on this computer — shared by all reports and build scripts
# ==============================================================================
# Only this file needs to change when the data move to another folder or
# another computer.

spatialdir <- "C:/WORK/4_Data/RDATA"                       # fao_sf, EMODnet wind farms, MPA
gfwdir     <- "C:/WORK/4_Data/GFW"                         # outputs of the GFW pipeline (gfw_s*.RData)
projdir    <- "C:/WORK/2_Projects/2026_01_CHANNEL_FISHERIES"   # AER, FDI, MMO data; output
litdir     <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/literature"  # stock_status.xlsx
inputdir   <- file.path(projdir, "2_Input")                  # AER, FDI and MMO data

# Knitted reports (outside Git, because they are large and change every build)
report_out_dir <- file.path(projdir, "4_Output")
