#' #  Reporting Step 1: Create (and maintain) report directory
#'
#' Install and update `R` packages required to run analyses and create report.
#' This step may also require us to copy any assets, custom content templates,
#' and other R scripts contained in the "Universal_Content" directory.
#'
#+ echo = TRUE, purl = TRUE, eval = FALSE
#   Set R working directory to the Documentation folder
# setwd("./Documentation")

# Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

# Install/update packages used in the report
source(file.path(universal.content.path, "Meta_Data", "Report_Packages.R"))

# It may be necessary to occasionally update Literasee package assets.
require(Literasee)
updateAssets(asset.type=c("css", "js", "pandoc", "rmd", "images"))

# setwd("..")
