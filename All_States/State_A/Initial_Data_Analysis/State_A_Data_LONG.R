#+ include = FALSE, purl = FALSE
################################################################################
####                                                                        ####
####                   Data Cleaning and Prep for State A                   ####
####                                                                        ####
################################################################################

#' ### Load packages and custom functions.
#'
#' The following `R` packages are required for the data cleaning and impact simulation.
#'
#+ echo = TRUE, purl = TRUE
require(SGP)
require(SGPdata)
require(data.table)
require(cfaTools)
require(quantreg)
require(splines)

# This R script includes at least two custom functions that
# may be included in future R packages.
source("../../../Universal_Content/Functions/addImpact.R")

# Set a random seed for reproducibility of simulated data
set.seed(3693)

#' ### General data setup and cleaning.
#'
#' For these simulation analyses we will be using the *`sgpData_LONG_COVID`* data
#' from the [`SGPData`](https://github.com/CenterForAssessment/SGPdata) package.
#' It includes 7 years of annual assessment data in two content areas (ELA and
#' Mathematics). The data comes with a "built-in" impact in 2021 related to the
#' pandemic as well as an unperturbed version - *`SCALE_SCORE_without_COVID_IMPACT`*.
#' Here we will simulate a different impact scenario for "State_A" and remove both
#' the unperturbed and original versions.
#'
#+ echo = TRUE, purl = TRUE
# First load and rename/remove SCALE_SCORE* variables included in the data
State_A_Data_LONG <- copy(SGPdata::sgpData_LONG_COVID)[YEAR < 2022]
State_A_Data_LONG[, SCALE_SCORE := NULL]
setnames(State_A_Data_LONG, "SCALE_SCORE_without_COVID_IMPACT", "SCALE_SCORE")

#' #### Add in prior scale score variable for simulating COVID impact
#'
#' The functions that are used to simulate COVID impact require two variables that
#' are currently not in the data: a prior score and an "impact percentile".
#'
#+ echo = TRUE, purl = TRUE
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
                 shift_variable = c("SCALE_SCORE"))

#  Clean up - remove 2020 dummy data and rename according to required conventions
State_A_Data_LONG <- State_A_Data_LONG[YEAR != '2020']
setnames(State_A_Data_LONG, gsub("LAG_1", "PRIOR_1YEAR", names(State_A_Data_LONG)))
setnames(State_A_Data_LONG, gsub("LAG_2", "PRIOR_2YEAR", names(State_A_Data_LONG)))

# Fix 2021 Lags for Grades 3 & 4 - repeaters:
# table(State_A_Data_LONG[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(State_A_Data_LONG[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"),
                    SCALE_SCORE_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016", SCALE_SCORE_PRIOR_1YEAR := NA]

#' ### Simulate COVID Impact by content area, grade and prior score decile.
#'
#' For students in grades 5 - 8, we use observed prior scores (`SCALE_SCORE_PRIOR_2YEAR`)
#' in simulating their 2021 COVID impact. If their prior score is missing we will
#' substitute their 2021 observed score. For 3rd and 4th grade students (no priors
#' available) we will first simulate a correlated prior score from which to then
#' simulate COVID impact.
#'
#' #### Create a simulated prior scale score
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, PRIOR_SCORE_for_IMPACT := simulatePriorScore(.SD),
        by = c("YEAR", "CONTENT_AREA", "GRADE"),
        .SDcols = c("YEAR", "CONTENT_AREA", "GRADE",
                    "SCALE_SCORE", "SCALE_SCORE_PRIOR_2YEAR")]

State_A_Data_LONG[YEAR == "2021" & is.na(PRIOR_SCORE_for_IMPACT),
                    PRIOR_SCORE_for_IMPACT := SCALE_SCORE]

#' #### Calculate the students' prior score deciles.
#'
#' We will assign (typical) impact level by content area, grade and the students'
#' prior score deciles. The function `addImpact` is flexible enough to allow for
#' any assignment criteria. E.g., rather than (or in addition to) prior score,
#' impact could be modeled as a function of school membership, demographic
#' characteristic(s), be completely random, etc.
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, PRIOR_SCORE_DECILE :=
                as.integer(cut(PRIOR_SCORE_for_IMPACT,
                  breaks=quantile(PRIOR_SCORE_for_IMPACT,
                  probs=seq(0,1, by=0.1), na.rm=TRUE), include.lowest=TRUE)),
              keyby=c("YEAR", "CONTENT_AREA", "GRADE")]


#' #### Specify the level of simulated impact by content area and grade.
#'
#' We will also allow impact to vary by grade and content area. In most states we
#' worked with in 2021 (and from other reports), students were more impacted in
#' mathematics than ELA. We add that pattern in here as well. Something not
#' necessarily consistently observed, but interesting to simulate, is that impact
#' levels increase from lowest to highest from the lowest to highest prior score deciles.
#' We will want to investigate whether this pattern is detected in our impact analyses.
#'
#' The `addImpact` function requires a variable that gives an integer level value
#' for the typical (true) impact for a student/group. In the list below we choose
#' 10 random uniform values between two boundaries and then sort them from lowest
#' to highest to correspond with the prior score deciles.
#'
#+ echo = TRUE, purl = TRUE
  qtile.impact <- list(
    ELA = list(
      "3" = rev(sort(round(runif(10, 25, 40), 0))),
      "4" = rev(sort(round(runif(10, 25, 40), 0))),
      "5" = rev(sort(round(runif(10, 35, 45), 0))),
      "6" = rev(sort(round(runif(10, 35, 45), 0))),
      "7" = rev(sort(round(runif(10, 35, 48), 0))),
      "8" = rev(sort(round(runif(10, 35, 48), 0)))),
    MATHEMATICS = list(
      "3" = rev(sort(round(runif(10, 20, 35), 0))),
      "4" = rev(sort(round(runif(10, 20, 35), 0))),
      "5" = rev(sort(round(runif(10, 30, 40), 0))),
      "6" = rev(sort(round(runif(10, 30, 40), 0))),
      "7" = rev(sort(round(runif(10, 30, 45), 0))),
      "8" = rev(sort(round(runif(10, 30, 45), 0))))
  )

#' From the `qtile.impact` list we create a `data.table` object that we will be
#' merged in with LONG data at the student level.
#'
#+ echo = TRUE, purl = TRUE
quantile_impact <- data.table(CONTENT_AREA = rep(names(qtile.impact), each=6*10),
                              GRADE = rep(rep(as.character(3:8), each=10), 2),
                              PRIOR_SCORE_DECILE = rep(1:10, 12),
                              IMPACT_PERCENTILE = unlist(qtile.impact))

## Verify that we created a `1` in each cell of this table:
#  table(quantile_impact[, .(PRIOR_SCORE_DECILE, CONTENT_AREA), GRADE])

setkey(quantile_impact, CONTENT_AREA, GRADE, PRIOR_SCORE_DECILE)
setkey(State_A_Data_LONG, CONTENT_AREA, GRADE, PRIOR_SCORE_DECILE)

State_A_Data_LONG <- quantile_impact[State_A_Data_LONG]
# should only have impact percentiles in 2021:
# table(State_A_Data_LONG[, YEAR, !is.na(IMPACT_PERCENTILE)])


#' #### Create new version of SCALE_SCORE with impact added
#'
#' We now have all the data elements needed to simulate random impact as specified.
#' Although not necessary, we will create this variable separately from the unperturbed
#' `SCALE_SCORE` variable (rather than overwriting it). This will let us do some
#' checks and comparisons against the original variable.
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, SCALE_SCORE_IMPACTED := SCALE_SCORE]
State_A_Data_LONG[YEAR == "2021",
             SCALE_SCORE_IMPACTED :=
                addImpact(.SD), by = list(CONTENT_AREA, GRADE),
                .SDcols = c("CONTENT_AREA", "GRADE",
                            "SCALE_SCORE", "PRIOR_SCORE_for_IMPACT",
                            "IMPACT_PERCENTILE")]

#' The new impacted variable may have values that are outside of the range of lowest/highest
#' observed/obtainable scores. We will pull in those values based on meta-data for the
#' LOSS/HOSS at the content area/grade level stored in `SGP::SGPstateData`.
#'
#+ echo = TRUE, purl = TRUE
for (content_area.iter in c("ELA", "MATHEMATICS")) {
  for (grade.iter in as.character(3:8)) {
    tmp.loss.hoss <-
      SGPstateData[["DEMO_COVID"]][["Achievement"]][["Knots_Boundaries"]][[
                     content_area.iter]][[paste0("loss.hoss_", grade.iter)]]
    State_A_Data_LONG[CONTENT_AREA == content_area.iter &
                                  GRADE == grade.iter &
                                  SCALE_SCORE_IMPACTED < tmp.loss.hoss[1],
                                    SCALE_SCORE_IMPACTED:=tmp.loss.hoss[1]]
    State_A_Data_LONG[CONTENT_AREA == content_area.iter &
                                  GRADE == grade.iter &
                                  SCALE_SCORE_IMPACTED > tmp.loss.hoss[2],
                                    SCALE_SCORE_IMPACTED:=tmp.loss.hoss[2]]
  }
}

#' ####  Quick visual checks on the new variable
#+ echo = TRUE, purl = TRUE
hist(State_A_Data_LONG[YEAR=='2021' & CONTENT_AREA == "ELA",
                         SCALE_SCORE_IMPACTED - SCALE_SCORE],
            main = "ELA", xlab = "Impact minus Unperturbed")
hist(State_A_Data_LONG[YEAR=='2021' & CONTENT_AREA == "MATHEMATICS",
                         SCALE_SCORE_IMPACTED - SCALE_SCORE],
            main = "MATHEMATICS", xlab = "Impact minus Unperturbed")

#' ### Final cleaning of the data
#'
#' With the scale score having simulated impact, we will now remove extraneous
#' variables and rename `SCALE_SCORE_IMPACTED` to conform to conventions used in
#' the `SGP` package. We also re-create the `ACHIEVEMENT_LEVEL` variable based
#' on the altered (for 2021 only) scale score.
#'
#+ echo = TRUE, purl = TRUE
State_A_Data_LONG[, SCALE_SCORE := NULL]
State_A_Data_LONG[, ACHIEVEMENT_LEVEL := NULL]
setnames(State_A_Data_LONG, "SCALE_SCORE_IMPACTED", "SCALE_SCORE")

State_A_Data_LONG <-
    SGP:::getAchievementLevel(State_A_Data_LONG, state = "DEMO_COVID")

## Verify impact on achievement relative to prior years
#  round(prop.table(table(State_A_Data_LONG[,
#              .(YEAR, CONTENT_AREA), ACHIEVEMENT_LEVEL]), 2)*100, 1)

# Remove variables used in simulating impact that are no longer needed.
State_A_Data_LONG[, c("PRIOR_SCORE_for_IMPACT",
                      "PRIOR_SCORE_DECILE",
                      "IMPACT_PERCENTILE") := NULL]

#' ### Summary and notes
#'
#'   * One thing that changed from the original (simulated) COVID impact is in how
#'     we simulated impact for 3rd and 4th graders. Originally we used their current
#'     scores as priors in the 5th grade regression model. Now we first simulate prior
#'     scores for them (adding in correlated, heteroskedastic random error to their
#'     current scores) with another function - `simulatePriorScore`. Then calculate
#'     their prior score decile based on actual/simulated priors.
#'     - This allows for a more consistent application of the COVID impact simulation
#'     - But also adds in a "double dose" of simulation (and error/uncertainty).
#'     - Not sure what impact this will have...
#'   * We specify the amount of impact by content area and grade. "State A" is simulated
#'     with increasing impact by decile and more impact in math overall (varying
#'     by grade). This is one nice feature about the `addImpact` function - we can
#'     basically determine simulated impact (or muted recovery going forward) in a
#'     pretty flexible way. If we wanted to assign impact by subgroup or school or any
#'     other way, we just need create a temporary variable that assigns a student/group
#'     an impact value in a variable (here called `IMPACT_PERCENTILE`).
#'   * We do NOT save the final data object here as usual before running baseline
#'     SGP analyses. This is done simply to save hard-drive and Github repo space.
