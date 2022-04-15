#####
###   Produce Demonstration COVID Learning Loss Analysis Report using `bookdown` and `pagedreport`
#####

###   Set up your R working directory
setwd("./Documentation")

###   Load/Format/Subset Report Data
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")

###   Load required packages
require(Literasee)


#####
###    Render the report using `bookdown`
#####

# bookdown::render_book(".", "bookdown::gitbook")

#  Serve the site directory on a local host to see the results:
# servr::httw(dir = "site", watch = "site", port=4224)
# servr::daemon_stop()


#####
###    Render the report using `pagedown`
#####

##    Need to (hopefully temporarily!) remove the List of Figures and Tables from the Demonstration_COVID_Academic_Impact_Analysis.Rmd
##    Delete the following:
#     includes:
#       in_header: ../assets/js/meta_lof_js.html
# customjs: ../assets/js/lof.js
# lof: yes
# lof-title: Tables and Figures

rmarkdown::render(file.path("report", "Demonstration_COVID_Academic_Impact_Analysis.Rmd"))
pagedown::chrome_print(file.path("report", "Demonstration_COVID_Academic_Impact_Analysis.html"), wait=10, timeout=60)


###   Appendices

require(magrittr) # needed if just rendering appendices

###   Academic Impact
source("../../../Universal_Content/Learning_Loss_Analysis/Functions/covidImpactSubVisualization.R")
rmarkdown::render(file.path("report", "Academic_Impact_Analyses_APPENDIX_B.Rmd"))
pagedown::chrome_print(file.path("report", "Academic_Impact_Analyses_APPENDIX_B.html"), output = "report/Temp_ApdxB.pdf", wait=10, timeout=60)
unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...

###   Adjust academic impact plots produced internally OR include plots produced externally
##    Manually locate the pages to keep/remove/rotate
rpt.list <- list(
  FRONT = 1:3,      # frontmatter - title page, TOC, any intro, etc.
  ELA = 4,          # Any ELA specific section (text, tables, etc.)
  MATHEMATICS = 11, # Any math specific section (text, tables, etc.)
  BACK = NULL       # backmatter - discussion, conclusions, etc.
)

base.pdf <- "report/Temp_ApdxB.pdf"

##    Option 1 - create SVG files and add in length-wise, then rotate pages afterwords.
all.pages <- seq(qpdf::pdf_length(base.pdf))
pages.to.rotate <- setdiff(all.pages, unlist(rpt.list, use.names = FALSE))

qpdf::pdf_rotate_pages(
  input = base.pdf,
  output = "report/Academic_Impact_Analyses_APPENDIX_B.pdf", # Must rename
  pages = pages.to.rotate, angle = 90,
)

##    Option 2 - create a pdf with dummy pages which are stripped out and replaced with existing PDF files
#  Subset/strip base PDF file and then re-combine into final report
# if (!is.null(rpt.list$FRONT)) {
#   qpdf::pdf_subset(input = base.pdf, pages= rpt.list$FRONT, output = file.path(tempdir(), "FRONT.pdf"))
#   plot.files <- file.path(tempdir(), "FRONT.pdf")
# } else plot.files <- NULL
#
# for (ca in names(child.plot.list)) {
#   if (!is.null(rpt.list[[ca]])) {
#     tmp.file <- file.path(tempdir(), paste0("AI_", ca, "_.pdf"))
#     qpdf::pdf_subset(input = base.pdf, pages= rpt.list[[ca]], output = tmp.file)
#     plot.files <- c(plot.files, tmp.file)
#   }
#   plot.files <- c(plot.files, unlist(lapply(child.plot.list[[ca]], function(f) return(f[["files"]]))))
# }
#
# if (!is.null(rpt.list$BACK)) {
#   qpdf::pdf_subset(input = base.pdf, pages= rpt.list$BACK, output = file.path(tempdir(), "BACK.pdf"))
#   plot.files <- file.path(tempdir(), "BACK.pdf")
# }
#
# qpdf::pdf_combine(input = plot.files, output = "report/Academic_Impact_Analyses_APPENDIX_B.pdf") # set output to `base.pdf` if not renaming

file.remove(base.pdf)

##    Goodness of Fit
rmarkdown::render(file.path("report", "Goodness_of_Fit_APPENDIX_G.Rmd"))
pagedown::chrome_print(file.path("report", "Goodness_of_Fit_APPENDIX_G.html"), wait=10, timeout=60) # , options = list(pageRanges='1-10') # last (blank) page issue fixed in dev pagedown varsion
unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...
#
# ###  Copy report to the bookdown site for download links
# if (!file.exists(file.path("site", "downloads"))) dir.create(file.path("site", "downloads"))
# file.copy(file.path("report", "Demonstration_COVID_Academic_Impact_Analysis.pdf"), file.path("site", "downloads"), overwrite = TRUE)
# file.copy(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.pdf"), file.path("site", "downloads"), overwrite = TRUE)

setwd("..")
