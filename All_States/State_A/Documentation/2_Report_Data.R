#+ include = FALSE, purl = FALSE
################################################################################
####                                                                        ####
####             Academic Impact Analysis - Step 2) Report Data             ####
####                                                                        ####
################################################################################

#' ## Reporting Step 2: `Report_Data`

#' In the data preparation and cleaning step of the the academic impact analysis,
#' we create a `Report_Data` object with data (sub)sets from various available
#' sources (including the statewide, English language proficiency, college entrance
#' and/or interim assessments). We format, alter, augment the available/desired
#' data and store it in a single named list object (`Report_Data`) that can be
#' passed to 1) standardized and customized analyses and 2) the report generating
#' code/process. In some cases this object can also be used used to create the
#' report `params`.
#'
#' This step assumes the user is operating with their working directory set to
#' "*NCME_2022_Project/All_States/State_A/Documentation*".

#+ echo = TRUE, purl = TRUE
# setwd("./Documentation")

#' ### Load packages and custom functions.
#'
#' The `SGP` package is required for the data analysis with simulated data.
#'
#+ echo = TRUE, purl = TRUE
require(SGP)
require(data.table)
require(cfaTools)

#' ###   State_Assessment
#'
#' In this data simulation and reporting exercise we are using `State_A_SGP`
#' directly from the "Initial_Analysis" step. Typically we would need to load
#' data from external sources at this stage (e.g. 'State_A_SGP.Rdata' that would
#' have been saved/output in the "*Initial_Data_Analysis/State_A_Baseline_SGP_Analyses.R*"
#' script.
#'
#' Here we simply copy the data from the `SGP` object and name it `State_A_Data_LONG`.


#+ echo = TRUE, purl = TRUE
rm(State_A_Data_LONG)
State_A_Data_LONG <- copy(State_A_SGP@Data)

#' Data loading is typically followed by an initial subsetting to select only the
#' report-relevant information (years, content areas and grades to be reported,
#' variables used in report analyses, student/organization/other exclusion
#' criteria, etc.). This process has already been carried out in the
#' "*Initial_Data_Analysis/State_A_Data_LONG.R*" script.
#'
#' ###   Create lagged variables
#'
#' Lagged scale and standardized score variables (and their associated achievement
#' levels) are required for our academic impact analyses. Here we first create a
#' standardized scale score variable that uses 2019 means and standard deviations
#' to create a referenced standardization. This prevents us from "washing out"
#' impact in 2021 that would happen if we standardized by year. We also standardize
#' the scores by content area and grade level.
#'
#' The `SGP` analyses already include some version of these lagged and/or standardized
#' variables, however, these are only included for students for whom growth was
#' calculated. This means that students with missing prior scores (and potentially
#' others such as students with repeated/accelerated grade progressions) would
#' not have these data in some cases. The following code chunk creates these
#' variables.
#'
#+ echo = TRUE, purl = TRUE
##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
State_A_Data_LONG[, SCALE_SCORE_STANDARDIZED :=
                      Z(.SD, "SCALE_SCORE", reference.year = "2019"),
                  by = list(CONTENT_AREA, GRADE),
                  .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]

#   Need to run this again here to get SCALE_SCORE_PRIOR_*YEAR
#   Seems to get deleted in abcSGP - names are too close! Something to look into.

# getShiftedValues DOES NOT add in 2020 (YEAR completely missing from data)
# We need to add in this information with a small data.table - `missing_data`
missing_data <- data.table(YEAR = "2020",
                           GRADE = c(3:8, 3:8),
                           CONTENT_AREA = c(rep("ELA", 6),
                                            rep("MATHEMATICS", 6)))
State_A_Data_LONG <- rbindlist(list(State_A_Data_LONG, missing_data), fill=TRUE)

shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(State_A_Data_LONG, shift.key)

getShiftedValues(State_A_Data_LONG,
                 shift_amount = c(1L, 2L),
                 shift_variable = c("SCALE_SCORE",
                                    "SCALE_SCORE_STANDARDIZED",
                                    "ACHIEVEMENT_LEVEL"))
# Check initial agreement between `SGP` and `shift` generated lagged variables:
# table(State_A_Data_LONG[YEAR==current.year,
#            ACHIEVEMENT_LEVEL_PRIOR, ACHIEVEMENT_LEVEL_LAG_2], exclude=NULL)

#  Clean up - remove 2020 dummy data and rename according to old conventions
State_A_Data_LONG <- State_A_Data_LONG[YEAR != '2020']
setnames(State_A_Data_LONG, gsub("LAG_1", "PRIOR_1YEAR", names(State_A_Data_LONG)))
setnames(State_A_Data_LONG, gsub("LAG_2", "PRIOR_2YEAR", names(State_A_Data_LONG)))

##    Fix 2021 Lags for Grades 3 & 4 - repeaters:
# table(State_A_Data_LONG[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(State_A_Data_LONG[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"),
                    SCALE_SCORE_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"),
                    SCALE_SCORE_STANDARDIZED_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"),
                    ACHIEVEMENT_LEVEL_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016",
                    SCALE_SCORE_PRIOR_1YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016",
                    SCALE_SCORE_STANDARDIZED_PRIOR_1YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016",
                    ACHIEVEMENT_LEVEL_PRIOR_1YEAR := NA]

#' ###   Create other variables
#'
#' Through the course of analyzing academic impact, we may find there are additional
#' variables that need to be created often (i.e. used in multiple analyses or in
#' both an analysis and the report generation). In these cases it is best to create
#' those variables here so that the process is standardized across all use cases.
#'
#' For example, we may need a variable that converts the `ACHIEVEMENT_LEVEL` and/or
#' `ACHIEVEMENT_LEVEL_PRIOR` variable into a dichotomous "Proficient/Not Proficient"
#' variable. The following example creates those variables such that they are
#' consistent not only in "State_A" data for use across analyses/reports, but also
#' consistent across states so that variables with the same naming conventions
#' could be used in the same `R` code or custom functions.
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, ACHIEVEMENT_ProfandAbove := fcase(
  ACHIEVEMENT_LEVEL %in% c("Partially Proficient", "Unsatisfactory"), "Not Proficient",
  ACHIEVEMENT_LEVEL %in% c("Advanced", "Proficient"), "Proficient")]

State_A_Data_LONG[YEAR == "2021", ACHIEVEMENT_LEVEL_PRIOR := ACHIEVEMENT_LEVEL_PRIOR_2YEAR]
State_A_Data_LONG[YEAR != "2021", ACHIEVEMENT_LEVEL_PRIOR := ACHIEVEMENT_LEVEL_PRIOR_1YEAR]
State_A_Data_LONG[, PRIOR_ACHIEVEMENT_ProfandAbove := fcase(
  ACHIEVEMENT_LEVEL_PRIOR %in% c("Partially Proficient", "Unsatisfactory"), "Not Proficient",
  ACHIEVEMENT_LEVEL_PRIOR %in% c("Advanced", "Proficient"), "Proficient")]

#' ###   Remove redundant/replaced variables
#'
#' In this data preparation and formatting step, we have created some variables
#' that are now redundant or only needed in the variable creation process. We now
#' do one last data cleaning step to make sure we are only saving the variables
#' that we will need/want.
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, c("SCALE_SCORE_PRIOR",
                      "SCALE_SCORE_PRIOR_STANDARDIZED",
                      "ACHIEVEMENT_LEVEL_PRIOR",
                      "SGP_NORM_GROUP_BASELINE",
                      "SGP_NORM_GROUP_BASELINE_SCALE_SCORES") := NULL]

#' In this exercise, we are only using a simulated data source that mimics a
#' statewide assessment (i.e. one given annually for accountability purposes).
#' The process outlined above could also be applied to ELP, college entrance,
#' interim or any other data source a state has. If the report analysis and
#' reporting code is set up to accommodate these multiple data sources, then
#' identical reporting can be done for each of these sources as well.

#+ include = FALSE, purl = FALSE
#####
###   College_Entrance
#####


#####
###   ELP_Assessment
#####


#####
###   Interim_Assessment
#####


#' ###   Combine all data and save
#'
#' Finally we will combine all the data sources that will be used in the report
#' analyses and reporting. Note that, regardless of the state/consortium/organization
#' from which the data comes, we name the data object `Report_Data`. This allows
#' us to flexibly and automatically apply `R` code and functions to this data
#' source in a standardized way. In this way, `Report_Data` is a required parameter
#' for this exercise.
#'
#+ echo = TRUE, purl = TRUE
Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(State_A_Data_LONG); rm(State_A_SGP); rm(State_A_Data_LONG)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

# setwd("..")
