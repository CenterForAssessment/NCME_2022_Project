#####
###   Run any external analyses for any/all elements of the `Report_Data` object
###   for the Learning Loss Analysis Report and house the results in a seperate
###   `Report_Analyses` object that is also passed to the rendered report.
###   As with `Report_Data`, this can include `State_Assessment`, `College_Entrance`,
###   `ELP_Assessment` and (potentially multiple) `Interim_Assessment` branches
###   with multiple analysis results types common to them all.
###   For example, all assessments may have a `participation` slot, but only one
###   may have a `multiple_imputation` analysis.
#####

###   Set up your R working directory
setwd("./Documentation")

###   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load packages used in the Report Analyses (and install/update as necessary)
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Packages.R"))

###   Load formated Report_Data from `2_Report_Data.R`
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")

###   Either load an existing `Report_Analyses` object
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")
  ##  or create a new one and run all analyses ##
     # Report_Analyses <- list()

###   Either load an existing `params` object or create one in source file(s)
###   Load existing params or create by sourcing appropriate scripts
load("params_dry_run.rda")
# if (!exists("params")) {
#   ###   Load required packages and custom functions
#   require(SGP)
#   require(Literasee)
#   if (exists("assessment")) {tmp.assessment <- assessment; chg.assess <- TRUE} else chg.assess <- FALSE
#   setwd("..")
#   source("./Documentation/4_Make_Configs.R")
#   setwd("./Documentation")
#   params <- report.config$params
#   source(knitr::purl(file.path(universal.content.path, "Learning_Loss_Analysis", "Child_RMD", "params.Rmd"), quiet=TRUE))
#   file.remove("index.Rmd")
#   file.remove("_bookdown.yml")
#   file.remove("params.R")
#   file.remove("DEMO_COVID_Report_Configuration_MetaData.rda")
#   if (chg.assess) tmp.assessment -> assessment
# }

#####
###   State_Assessment
#####

###   Declare an assessment flavor (or loop around source(...) files)
assessment <- "State_Assessment"

###   Missing data visualizations
"Report_Analyses/DEMO_COVID_Missing_Data_2021.R"


###   (School Level) Summary Tables
require(cfaTools)
Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["SCHOOL_NUMBER"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="DEMO_COVID",
          current_year=tail(params[["years"]][[assessment]], 1),
          prior_year=tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=params[["GL_subjects"]][[assessment]],
          all_grades=params[["grades"]][[assessment]],
          sgp_grades=params[["sgp.grades"]][[assessment]],
          aggregation_group="SCHOOL_NUMBER")[["SCHOOL_NUMBER"]]

Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["SCHOOL_NUMBER_by_GRADE"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="DEMO_COVID",
          current_year=tail(params[["years"]][[assessment]], 1),
          prior_year=tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=params[["GL_subjects"]][[assessment]],
          all_grades=params[["grades"]][[assessment]],
          sgp_grades=params[["sgp.grades"]][[assessment]],
          aggregation_group=c("SCHOOL_NUMBER", "GRADE"))[["SCHOOL_NUMBER_by_GRADE"]]

###   (District Level) Summary Tables
Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["DISTRICT_NUMBER"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="DEMO_COVID",
          current_year=tail(params[["years"]][[assessment]], 1),
          prior_year=tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=params[["GL_subjects"]][[assessment]],
          all_grades=params[["grades"]][[assessment]],
          sgp_grades=params[["sgp.grades"]][[assessment]],
          aggregation_group="DISTRICT_NUMBER")[["DISTRICT_NUMBER"]]

Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["DISTRICT_NUMBER_by_GRADE"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="DEMO_COVID",
          current_year=tail(params[["years"]][[assessment]], 1),
          prior_year=tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=params[["GL_subjects"]][[assessment]],
          all_grades=params[["grades"]][[assessment]],
          sgp_grades=params[["sgp.grades"]][[assessment]],
          aggregation_group=c("DISTRICT_NUMBER", "GRADE"))[["DISTRICT_NUMBER_by_GRADE"]]

#
###   Percent Proficient
percent_proficient <- function(achievement_level) {
  tmp.table <- table(achievement_level)
  100*sum(tmp.table["Proficient"])/sum(tmp.table)
}

tmp <- Report_Data[[assessment]][YEAR %in% c("2018", "2019", "2021") & CONTENT_AREA %in% c("ELA", "MATHEMATICS"), list(
                                        PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_ProfandAbove)),
                                        # PARTICIPATED=sum(!is.na(ACHIEVEMENT_LEVEL)), TOTAL = .N),
                                      keyby=c("YEAR", "CONTENT_AREA", "GRADE")]

Report_Analyses[["Summary_Tables"]][[assessment]][["Achievement"]][["Overall_PctProf"]] <-
    dcast(tmp, CONTENT_AREA + GRADE ~ YEAR, sep=".", drop=FALSE, value.var="PERCENT_PROFICIENT")


##    Imputation Difference Summaries

##  From Report_Analyses/GA_Imputation_Overall_ContentArea_by_Grade.R - could be sourced and saved as well...
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Percent_Prof_Imputations_Overall_ContentArea_by_Grade.R"))
Report_Analyses[["Summary_Tables"]][[assessment]][["Imputation"]][["CONTENT_AREA__GRADE"]] <- State_Assessment_Summaries

###   Quantile Shift Analyses
#
# load("params_dry_run.rda")
# beeswarm.plots <- TRUE
# hsf.analysis <- TRUE
# between.inst.ges <- TRUE
#
# for (assessment in c("State_Assessment", "ELP_Assessment")) {
#     source("Report_Analyses/DEMO_COVID_Quantile_Shift_Effects_2021.R")
# }

#####
###   College_Entrance
#####



#####
###   ELP_Assessment
#####



#####
###   Interim_Assessment
#####



#####
###   Combine all data analyses into `Report_Analyses` and Save
#####

###  Do we need this monster?  Not seeing it used anywhere, so not keeping it...
# Report_Analyses[["participation"]][[assessment]][["onelevel_logistic"]] <- NULL

save(Report_Analyses, file = "../Data/Report_Analyses.Rdata")
rm(params)
setwd("..")
