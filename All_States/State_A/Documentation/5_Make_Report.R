#' #  Reporting Step 5: Produce report and appendices
#'
#' Now the fun begins! In this step we (finally!) generate reports using the
#' results from the analyses we ran and the information we gathered and organized
#' in the four previous steps.
#'
#' This step assumes the user is operating from different working directories
#' depending on the report we are generating. At this point in the demonstration,
#' we can consider this step a "choose your own adventure" story. On the one hand,
#' we can start at "*NCME_2022_Project/All_States/State_A/Documentation*" if all
#' the prior steps have been run together already and simply (re)load the necessary
#' data and results objects saved from steps 2 and 3 before moving on
#' to the "*Initial_Data_Analysis*" report generation. On the other hand, the code
#' is set up to where we could just start here without having run anything yet.
#' That is, the `R` code and Rmd files are set up to run the analysis code and
#' generate reports automatically afterwords.
#'
#' We begin by creating appendix B, which pertains to the initial SGP analyses.
#' With those data and results available in the `R` workspace, we proceed with
#' the report analyses and reporting (generated simultaneously by `spin`ing
#' and `eval`uating steps 2 through 4 (steps 1 and 5 can/should be done separately).
#' Finally we create an academic impact overview appendix and then combine all
#' the main report child .Rmd files into the final report(s).

#+ echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./Documentation")
##  Load `Literasee` package
require(Literasee)

##  Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")
closet <- "../../../Universal_Content/rmarkdown/closet/"

##  Load existing `Report_Data` and `Report_Analyses` objects from steps 2 and 3.
##  Or... Skip the data loading and proceed to the next step to re-run while reporting.
# if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
# if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")

##  Create directories if needed for results
if(!dir.exists("../Documentation/report"))
        dir.create("../Documentation/report", recursive = TRUE)

#' ###  Initial data analysis and reporting
#'
#' One intent of this demonstration has been to show how reporting can be automated,
#' and one way this can be done is to combine the (final) data analysis and reporting
#' into a single step. The `R` code in the "*Initial_Data_Analysis*" directory
#' has been set up so that a report can be generated from it directly (here to be
#' used as an appendix to the final report). See [this link](https://bookdown.org/yihui/rmarkdown-cookbook/spin.html)
#' for details on how to `spin` your goat hair `R` code into a `knit`'d report.
#'
#' We will first construct a parent .Rmd file from pieces provided in the closet
#' full of skeletons. In order to `eval`uate the code while generating a report,
#' it seems to be necessary to work from the "*Initial_Data_Analysis*". The
#' documentation itself is saved where it can be accessed easily in the final
#' report generation.
#'
#' We will first use a simple, clean template called ["working paper"](https://github.com/paulcbauer/Writing_a_reproducable_paper_in_pagedown).

#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Initial_Data_Analysis/")

writeLines(c("---", readLines(paste0(closet, "skeleton_sgp_ida_wp.yml")),
             "---", readLines(paste0(closet, "skeleton_sgp_ida.Rmd"))),
           "tmp_file.Rmd")

rmarkdown::render(input="tmp_file.Rmd",
    output_file="../Documentation/report/Appendix_SGP_Analysis_WP.html")
frm.tf <- file.remove("tmp_file.Rmd")


#' ###  Render again in `nciea_report` theme
#'
#' In the final part of step 4, we generated five .Rmd scripts for generating
#' reports with the `nciea_report` template from the `Literasee` package we
#' maintain on GitHub. These are all `render`able from the "*Documentation*"
#' directory, with the exception of the initial SGP analysis appendix. Again,
#' since it is rendering `R` code directly and not just being rendered as text
#' (i.e. `eval=TRUE` is assumed in the code as it is interpreted) we need to run
#' the analyses and reporting from the "*Initial_Data_Analysis*" directory. Doing
#' this breaks many of the relative paths in the automatically generated script.
#'
#' Just as we did for the draft version above, we will first construct a Rmd
#' parent and then render it.

#+ echo = TRUE, purl = TRUE, eval = FALSE
writeLines(c("---", readLines(paste0(closet, "skeleton_sgp_ida_nciea.yml")),
             "---", readLines(paste0(closet, "skeleton_sgp_ida.Rmd"))),
           "tmp_file.Rmd")

rmarkdown::render(input="tmp_file.Rmd",
        output_file="../Documentation/report/Appendix_SGP_Analysis.html")
frm.tf <- file.remove("tmp_file.Rmd")

#' ##  Academic impact analyses (and reporting)
#'
#' Now that we have the initial data analyses (re)run we can do another round of
#' simultaneous analysis and report generation. This time we will create the "C"
#' appendix, which is probably what you are reading right now. This again requires
#' us to render the report from a specific directory - this time the "*Documentation*"
#' directory (which is where we assumed all of the report data set up, analyses,
#' etc. where to be conducted).
#'
#' In the "Appendix_Impact_Report_Generation.Rmd", the `yaml` has been written to
#' create reports in both the "working paper" and "nciea report" themes. We will
#' run them concurrently and generate both formats from the same call to `render`.

#+ echo = TRUE, purl = TRUE, eval = FALSE
setwd("../Documentation/")

##  Copy Rmd to the "Documentation" directory to match paths in R code.
fcp.tf <- file.copy(
    "assets/rmd/Custom_Content/Appendix_Impact_Report_Generation.Rmd", ".")

rmarkdown::render(
    input = "Appendix_Impact_Report_Generation.Rmd",
    output_file = c("AIRG_WP.html", "Appendix_Impact_Report_Generation.html"),
    output_format = "all", output_dir = "report")

frm.tf <- file.remove("Appendix_Impact_Report_Generation.Rmd")

#' ## First draft of final report
#'
#' We have delayed satisfaction long enough! We now compile our first draft of
#' the final report along with the appendices we just generated. For this we will
#' continue to use the "working paper" template.
#'
#' We will use a similar process to the one above to assemble a parent document
#' for the draft output. The draft is then rendered and saved in "*report*"
#' directory and converted to a pdf there using `pagedown::chrome_print`.

#+ echo = TRUE, purl = TRUE, eval = FALSE
writeLines(c("---", readLines(paste0(closet, "skeleton_main_wp.yml")),
                  readLines(paste0(closet, "skeleton_main_params.yml")),
             "---", readLines(paste0(closet, "skeleton_main_alt.Rmd"))),
           "report/tmp_file.Rmd")

rmarkdown::render(input="report/tmp_file.Rmd",
                  output_file="State_A_Academic_Impact_Analysis_WP.html")
frm.tf <- file.remove("report/tmp_file.Rmd")
pagedown::chrome_print("report/State_A_Academic_Impact_Analysis_WP.html")

#' ##  Alternate `pagedown` templates
#'
#' There are a handful of templates within the `pagedown` package one can use to
#' format reports, and the `pagedreport` package offers three packaged report
#' templates that one can use. We can try one out here with our skeletons.

#+ echo = TRUE, purl = TRUE, eval = FALSE
writeLines(c("---", readLines(paste0(closet, "skeleton_main_pgdrpt.yml")),
              readLines(paste0(closet, "skeleton_main_params.yml")),
             "---", readLines(paste0(closet, "skeleton_main_alt.Rmd"))),
           "report/tmp_file.Rmd")

rmarkdown::render(input="report/tmp_file.Rmd",
        output_file="State_A_Academic_Impact_Analysis_PgdRpt.html")
frm.tf <- file.remove("report/tmp_file.Rmd")
pagedown::chrome_print("report/State_A_Academic_Impact_Analysis_PgdRpt.html")

#' This report looks pretty nice right out-of-the-box. There are some formatting
#' that would need to be cleaned up, and possibly some customization that could
#' be done without too little effort. One thing to point out is that appendices
#' would all need to be rendered separately or included as additional sections
#' at the end of the paper. A common complaint seen (or at least requested feature)
#' is that users can not add prefixes to page, figure or table numbers (e.g.,
#' "Figure A1.", Table B-2, etc.). These issues have been worked out in the `nciea`
#' template, and improvements/workarounds are underway in the "working paper".

#' ##  Academic impact overview appendix
#'
#' We will now turn to an appendix with detailed plots of the academic impact
#' in State A. These plots contain quite a bit of information and were formatted
#' originally to be 11"x17" pdfs. The code for this appendix takes the `R` graphical
#' objects and converts them to svg images (pdf and html do not play well together)
#' before adding them to the report in a portrait layout. Once rendered and printed
#' to a preliminary PDF report, those pages are then rotated 90 degrees to a
#' landscape layout using the `qpdf` package.
#'
#' Note that for this appendix we can use the automatically generated Rmd script
#' created in step 4 without modification.

#+ echo = TRUE, purl = TRUE, eval = FALSE
source("../../../Universal_Content/Functions/covidImpactSubVisualization.R")
rmarkdown::render("report/Academic_Impact_Overview_APPENDIX_A.Rmd")
pagedown::chrome_print("report/Academic_Impact_Overview_APPENDIX_A.html",
                       output = "report/Temp_ApdxA.pdf")
#  Need to remove - seems to mess up attempts to render the `bookdown` site ...
unlink(file.path("report", "_bookdown.yml"))

#' ###  Adjust academic impact plots produced internally (from GROBS)

#+ echo = TRUE, purl = TRUE, eval = FALSE
##  Manually locate the pages to rotate
rpt.list <- list(
  FRONT = 1:3,      # frontmatter - title page, TOC, any intro, etc.
  ELA = 4,          # Any ELA specific section (text, tables, etc.)
  MATHEMATICS = 11, # Any math specific section (text, tables, etc.)
  BACK = NULL       # backmatter - discussion, conclusions, etc.
)

base.pdf <- "report/Temp_ApdxA.pdf"

##  Rotate landscape pages.
all.pages <- seq(qpdf::pdf_length(base.pdf))
pages.to.rotate <- setdiff(all.pages, unlist(rpt.list, use.names = FALSE))

qpdf::pdf_rotate_pages(
  input = base.pdf,
  output = "report/Academic_Impact_Analyses_APPENDIX_A.pdf", # Must rename
  pages = pages.to.rotate, angle = 90,
)

frm.tf <- file.remove(base.pdf)

#' ##  Final report draft
#'
#' We now have all the pieces in place to run the final report. We do not need
#' to modify the parent script `createReportScripts` produced. This is now the
#' easy part! We then add in final appendix, which displays the R session information
#' (the user's system specifications, package versions, etc.).

#+ echo = TRUE, purl = TRUE, eval = FALSE
rmarkdown::render("report/State_A_Academic_Impact_Analysis.Rmd")

here <- getwd()
rmarkdown::render(
    "../../../Universal_Content/rmarkdown/Child_RMD/APPENDIX_R_NCIEA.Rmd",
    output_file = paste0(here, "/report/Appendix_Academic_Impact_R.html"))


#' ###  Produce PDF reports for the remaining files
#'
#+ echo = TRUE, purl = TRUE, eval = FALSE
pagedown::chrome_print("report/State_A_Academic_Impact_Analysis.html")
# pagedown::chrome_print("report/Appendix_A.html") # Done above with rotation
pagedown::chrome_print("report/Appendix_SGP_Analysis.html")
pagedown::chrome_print("report/Appendix_Impact_Report_Generation.html")
pagedown::chrome_print("report/Appendix_R.html")

#' ##  Creating a `bookdown` website
#'
#' It is also possible to create a `bookdown` website using the same Rmd child
#' documents as those used in the `pagedown` based reports. As part of the
#' `createReportScripts` function ran in `4_Make_Configs.R`, the required `yaml`
#' and `index.Rmd` files were created. Given the complexity of some of the appendix
#' generation, it is advised that these scripts be modified to remove those from
#' the `_bookdown.yml` file first. It may also be necessary to copy some assets
#' (such as figures, plots, css, etc.) from "*Universal_Content*" to the state
#' "*Documentation*" directory.
#'
#' Begin by copying the PDF reports to the "*site*" directory for download links
#' and then rendering the website.

#+ echo = TRUE, purl = TRUE, eval = FALSE
# if (!dir.exists(file.path("site", "downloads")))
#         dir.create(file.path("site", "downloads"), recursive = TRUE)
#
# file.copy(c(file.path("report", "State_A_Academic_Impact_Analysis.pdf"),
#             file.path("report", "Academic_Impact_Analyses_APPENDIX_A.pdf"),
#             file.path("report", "Appendix_SGP_Analysis.pdf"),
#             file.path("report", "Appendix_Impact_Report_Generation.pdf"),
#             file.path("report", "APPENDIX_R.pdf")),
#           file.path("site", "downloads"), overwrite = TRUE)
#
# bookdown::render_book(".", "bookdown::gitbook") # delete all appendices from yaml
#
# # Serve the site directory on a local host to see the results:
# servr::httw(dir = "site", watch = "site", port=4224)
# servr::daemon_stop()


#' ##  Bonus Report: The paper from this session
#'
#' The paper that accompanies this demonstration is built from the README.md
#' files sprinkled throughout the repository. Here's how I put it together:
#'
#+ echo = TRUE, purl = TRUE, eval = FALSE
rmarkdown::render("Flexible_Report_Generation.Rmd")

# setwd("..")
