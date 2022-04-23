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
universal.content.path <- file.path("")

# Install/update packages used in the report
source("../../../Universal_Content/Meta_Data/Report_Packages.R")

###   Load packages required for report setup
require(Literasee)

template.path <- file.path("../../../Custom_Content/assets/Child_RMD")
setupReportDirectory(custom.content.path = template.path)

# It may be necessary to occasionally update Literasee package assets.
# updateAssets(asset.type=c("css", "js", "pandoc", "rmd", "images"))

# copy additional assets from the Universal_Content directory
R.utils::copyDirectory(to = "assets/fonts",
         from = "../../../Universal_Content/rmarkdown/assets/fonts")

# setwd("..")
