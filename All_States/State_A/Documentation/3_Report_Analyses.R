#' ## Academic Impact Analysis Step 3: `Report_Analyses`
#'
#' In this step we run any external analyses using data from any/all elements of
#' the `Report_Data` object for the academic impact report and house the results
#' in a seperate object named `Report_Analyses`. This object will eventually be
#' passed to the rendered report. As with `Report_Data`, this can include
#' `State_Assessment`, `College_Entrance`, `ELP_Assessment` and (potentially
#' multiple) `Interim_Assessment` branches with multiple analysis results types
#' common to them all. For example, all assessments may have a `participation`
#' slot, but only one may have a `multiple_imputation` analysis.
#'
#' This step assumes the user is operating with their working directory set to
#' "*NCME_2022_Project/All_States/State_A/Documentation*".

#+ echo = TRUE, purl = TRUE
setwd("./Documentation")
##   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#  Load packages used in the Report Analyses (and install/update as necessary).
#  Here we assume updates have been made in Step 1, and that each report analysis
#  script has been run, but sourcing the "Report_Packages.R" may be necessary.

# source(file.path(universal.content.path, "Meta_Data", "Report_Packages.R"))

#' ### Load `Report_Data` and `Report_Analyses` objects
#'
#' Running analyses for the report assumes that formatted data is available from
#' the `2_Report_Data.R` script. All custom functions and/or `R` scripts use
#' `Report_Data` as the data source. Although this script may be sourced in its
#' entirety, it is also possible to load the object and only (re)run certain
#' sections. For that reason, we first load any existing `Report_Analyses` objects.

#+ echo = TRUE, purl = TRUE
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")

##  Load an existing `Report_Analyses` object or set up list
if (file.exists("../Data/Report_Analyses.Rdata")) {
  load("../Data/Report_Analyses.Rdata")
} else { #  or create a new one and run all analyses
  Report_Analyses <- list()
}

#' ### State assessment analyses
#'
#' For the statewide assessment in this demonstration report for State A, we will
#' conduct two primary analyses: missing data and academic impact. We will also
#' create a table that will be included in all state reports showing the percent
#' proficient trend over several years . Because State A was not set up to include
#' missing data, there is no multiple imputation analysis done here, and therefore
#' a similar table is not produced showing the estimated impact on percent proficient
#' from the missing data (via a multiple imputation analysis).

#+ echo = TRUE, purl = TRUE
###   Declare an assessment flavor
assessment <- "State_Assessment"

###   Missing data visualizations
source("../Report_Analyses/State_A_Missing_Data_2021.R")

###   Academic impact visualizations
source("../Report_Analyses/State_A_Academic_Impact_Visualization.R")

###   Percent Proficient
pct_proficient <- function(achievement_level) {
  tmp.table <- table(achievement_level)
  100*sum(tmp.table["Proficient"])/sum(tmp.table)
}

tmp <- Report_Data[[assessment]][
          YEAR %in% c("2018", "2019", "2021") &
          CONTENT_AREA %in% c("ELA", "MATHEMATICS"),
            .(PERCENT_PROFICIENT= round(pct_proficient(ACHIEVEMENT_ProfandAbove), 1)),
          keyby=c("YEAR", "CONTENT_AREA", "GRADE")]

Report_Analyses[["Summary_Tables"]][[assessment]][[
                 "Achievement"]][["Overall_PctProf"]] <-
                     dcast(tmp, CONTENT_AREA + GRADE ~ YEAR,
                           sep=".", drop=FALSE, value.var="PERCENT_PROFICIENT")

##  Imputation Difference Summaries
# source(file.path(universal.content.path, "Functions",
#                  "Percent_Prof_Imputations_Overall_ContentArea_by_Grade.R"))
# Report_Analyses[["Summary_Tables"]][[assessment]][[
#                  "Imputation"]][["CONTENT_AREA__GRADE"]] <-
#                      State_Assessment_Summaries

#' ###   Combine all data analyses into `Report_Analyses` and Save
#+ echo = TRUE, purl = TRUE
save(Report_Analyses, file = "../Data/Report_Analyses.Rdata")
# rm(params)
setwd("..")
