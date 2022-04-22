#' ## Reporting Step 5: Produce report and appendices
#'
#' Now the fun begins! In this step we (finally!) generate reports using results,
#' assets and information created in the four previous steps.
#'
#' This step assumes the user is operating from different working directories at
#' specific points. We start at "*NCME_2022_Project/All_States/State_A/Documentation*"
#' to make sure the necessary data and results from steps 2 and 3 are loaded, and
#' then move to the "*Initial_Data_Analysis*" directory to both run the `R` code
#' and generate our first report (appendix B). With those data and results available,
#' we proceed with the report analyses and reporting (generated simultaneously by
#' `spin`ing and `eval`uating steps 2 through 4 (steps 1 and 5 can/should be done
#' separately). Finally we create an impact analysis appendix and then combine all
#' the main child .Rmd files into the final report.

#+ echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./Documentation")
##  Load `Literasee` package
require(Literasee)
##  Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")
##  Load existing `Report_Data` and `Report_Analyses` objects from steps 2 and 3.
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")
##  Create directories if needed for results
if(!dir.exists("../Documentation/assets/rmd/Custom_Content"))
  dir.create("../Documentation/assets/rmd/Custom_Content", recursive = TRUE)

#' ###  Initial data analysis and reporting
#'
#' One intent of this demonstration has been to show how reporting can be automated,
#' and a way this can be done is to combine the (final) data analysis and reporting
#' into a single step. The `R` code in the "*Initial_Data_Analysis*" directory
#' has been set up so that a report can be generated from it directly (here to be
#' used as an appendix to the final report).
#'
#' We will first construct a parent .Rmd file from pieces provided in our closet
#' full of skeletons. In order to `eval`uate the code while generating a report,
#' it seems to be necessary to work from the "*Initial_Data_Analysis*". We create
#' the documentation there and then move it to where it can be accessed more easily
#' in the final report generation.
#'
#' In this first step we will use a simple, clean template called "working paper".

#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Initial_Data_Analysis/")

writeLines(c("---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_sgp_ida_wp.yml"),
  "---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_sgp_ida.Rmd")),
  "tmp_file.Rmd")

##  Get the merged `params` created in step 4 and modify
params <- report.config$params
params$executive.summary <- FALSE

rmarkdown::render(input="tmp_file.Rmd",
        output_file="../Documentation/report/Appendix_SGP_Analysis_WP.html")
frm.tf <- file.remove("tmp_file.Rmd")

#' We have delayed satisfaction long enough! We will now complete our first draft
#' of the report along with the appendix we just generated. For this we will
#' continue to use the "working paper" template. Before we run the draft version,
#' however, we need to run steps 2 and 3 of the report.
#'
#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Documentation")
source("2_Report_Data.R")
source("3_Report_Analyses.R")

#' We will use a similar process to the one above to assemble a parent document
#' for the draft output. The draft is then rendered and saved in "*report*"
#' directory and converted to a pdf there using `pagedown::chrome_print`.

#+ echo = TRUE, purl = TRUE, eval = FALSE
writeLines(c("---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_wp.yml"),
# another way to get `params` without the list object. `rm(params)` first!
# readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_params.yml"),
  "---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_alt.Rmd")),
  "report/tmp_file.Rmd")

rmarkdown::render(input="report/tmp_file.Rmd",
        output_file="State_A_Academic_Impact_Analysis_WP.html")
frm.tf <- file.remove("report/tmp_file.Rmd")
pagedown::chrome_print("report/State_A_Academic_Impact_Analysis_WP.html")

#' ###  Alternate `pagedown` templates
#'
#' There are a handful of templates within the `pagedown` package one can use to
#' format reports, and the `pagedreport` package offers three packaged report
#' templates that one can use. We can try one out here with our skeletons.

#+ echo = TRUE, purl = TRUE, eval = FALSE
writeLines(c("---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_pgdrpt.yml"),
# readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_params.yml"),
  "---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_main_alt.Rmd")),
  "report/tmp_file.Rmd")

rmarkdown::render(input="report/tmp_file.Rmd",
        output_file="State_A_Academic_Impact_Analysis_PgdRpt.html")
frm.tf <- file.remove("report/tmp_file.Rmd")
pagedown::chrome_print("report/State_A_Academic_Impact_Analysis_PgdRpt.html")


# setwd("../Documentation/")
# res <- rmarkdown::render('report/Imapact_Report_Generation_APPENDIX_C.Rmd', quiet = TRUE)
# fcp.tf <- file.copy(res, "../Documentation/assets/rmd/Custom_Content/", overwrite = TRUE)
# if (fcp.tf) frm.tf <- file.remove(res)



#' ##  Render the report using `pagedown`/`nciea_report`
#'
#' In the final part of step 4, we generated five .Rmd scripts for generating
#' reports with the `nciea_report` template we use from the `Literasee` package
#' we maintain on GitHub. These are all renderable from the "*Documentation*"
#' directory, with the exception of the initial SGP analysis appendix. Again,
#' since it is rendering `R` code directly and not just being rendered as text
#' (i.e. `eval=TRUE` is assumed in the code as interpreted) we need to run the
#' analyses and reporting from the "*Initial_Data_Analysis*" directory. This breaks
#' many of the relative paths in the automatically generated script.
#'
#' Just as we did for the draft and `pagedreport` versions above, we will first
#' construct a Rmd parent and render it.
#'

#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Initial_Data_Analysis/")

writeLines(c("---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_sgp_ida_nciea.yml"),
  "---",
  readLines("../../../Universal_Content/rmarkdown/closet/skeleton_sgp_ida.Rmd")),
  "tmp_file.Rmd")

##  Get the merged `params` created in step 4
params <- report.config$params

rmarkdown::render(input="tmp_file.Rmd",
        output_file="../Documentation/report/Appendix_SGP_Analysis.html")
frm.tf <- file.remove("tmp_file.Rmd")

#' ##  Academic impact appendix
#'
#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Documentation/")
rm(params)
source("../../../Universal_Content/Functions/covidImpactSubVisualization.R")
rmarkdown::render("report/Academic_Imapact_Analysis_APPENDIX_A.Rmd")
pagedown::chrome_print(
            file.path("report", "Academic_Imapact_Analysis_APPENDIX_A.html"),
            output = "report/Temp_ApdxA.pdf", wait=10, timeout=60)
unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...

###   Adjust academic impact plots produced internally (from GROBS)
##    Manually locate the pages to keep/remove/rotate
rpt.list <- list(
  FRONT = 1:3,      # frontmatter - title page, TOC, any intro, etc.
  ELA = 4,          # Any ELA specific section (text, tables, etc.)
  MATHEMATICS = 11, # Any math specific section (text, tables, etc.)
  BACK = NULL       # backmatter - discussion, conclusions, etc.
)

base.pdf <- "report/Temp_ApdxA.pdf"

##    Option 1 - create SVG files and add in length-wise, then rotate pages afterwords.
all.pages <- seq(qpdf::pdf_length(base.pdf))
pages.to.rotate <- setdiff(all.pages, unlist(rpt.list, use.names = FALSE))

qpdf::pdf_rotate_pages(
  input = base.pdf,
  output = "report/Academic_Impact_Analyses_APPENDIX_A.pdf", # Must rename
  pages = pages.to.rotate, angle = 90,
)

file.remove(base.pdf)





##    Initial SGP Analysis
fcp.tf <- file.copy("assets/rmd/Custom_Content/Appendix_SGP_Analysis.Rmd", "../")
if (fcp.tf) fcp.tf <- file.copy("report/Initial_SGP_Analysis_APPENDIX_B.Rmd", "../Initial_Data_Analysis", overwrite = TRUE)
setwd("../Initial_Data_Analysis")
rmarkdown::render(file.path("Initial_SGP_Analysis_APPENDIX_B.Rmd"))
fcp.tf <- file.copy("Initial_SGP_Analysis_APPENDIX_B.html", "../Documentation/report", overwrite = TRUE)
# if(fcp.tf) # careful! make these skeletons and custom content templates first!
#   frm.tf <- file.remove(c("Appendix_SGP_Analysis.Rmd", "_bookdown.yml",
#                           "Initial_SGP_Analysis_APPENDIX_B.Rmd",
#                           "Initial_SGP_Analysis_APPENDIX_B.html"))
setwd("../Documentation")
pagedown::chrome_print(file.path("report", "Initial_SGP_Analysis_APPENDIX_B.html"), wait=10, timeout=60) # , options = list(pageRanges='1-10') # last (blank) page issue fixed in dev pagedown varsion


setwd("../Initial_Data_Analysis")
rmarkdown::render("APPENDIX_SGP_Analysis_NCIEA.Rmd")

rmarkdown::render("APPENDIX_SGP_Analysis_WP.Rmd")



rm(params)
rmarkdown::render(file.path("report", "State_A_Academic_Impact_Analysis.Rmd"))
pagedown::chrome_print(file.path("report", "State_A_Academic_Impact_Analysis.html"), wait=10, timeout=60)

pagedown::chrome_print("report/State_A_Academic_Impact_Analysis.html")
# pagedown::chrome_print("report/Appendix_A.html") # Done above with page rotation
pagedown::chrome_print("report/Appendix_SGP_Analysis.html")
pagedown::chrome_print("report/Appendix_C.html")
pagedown::chrome_print("report/Appendix_R.html")


###   Appendices

require(ymlthis) # needed if just rendering appendices


# bookdown::render_book(".", "bookdown::gitbook") # delete all appendices from yaml
#
# # Serve the site directory on a local host to see the results:
# servr::httw(dir = "site", watch = "site", port=4224)
# servr::daemon_stop()

# ###  Copy report to the bookdown site for download links
if (!file.exists(file.path("site", "downloads"))) dir.create(file.path("site", "downloads"))
file.copy(c(file.path("report", "State_A_Academic_Impact_Analysis.pdf"),
            file.path("report", "Academic_Imapact_Analysis_APPENDIX_A.pdf"),
            file.path("report", "Initial_SGP_Analysis_APPENDIX_B.pdf"),
            file.path("report", "Imapact_Report_Generation_APPENDIX_C.pdf"),
            file.path("report", "APPENDIX_R.pdf"))
          file.path("site", "downloads"), overwrite = TRUE)

setwd("..")
