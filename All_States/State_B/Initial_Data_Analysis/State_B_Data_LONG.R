################################################################################
####                                                                        ####
####                   Data Cleaning and Prep for State A                   ####
####                                                                        ####
################################################################################


### Load packages
require(SGP)
require(SGPdata)
require(data.table)
require(cfaTools)
require(quantreg)
require(splines)

source("../../../Universal_Content/Functions/addImpact.R")
### Parameters
# decile.impact <- c(0.05, -0.05, -0.1, -0.15, -0.2, -0.25, -0.2, -0.15, -0.1, -0.05)

set.seed(3693)

State_A_Data_LONG <- copy(SGPdata::sgpData_LONG_COVID)[YEAR < 2022]
State_A_Data_LONG[, SCALE_SCORE := NULL]
setnames(State_A_Data_LONG, "SCALE_SCORE_without_COVID_IMPACT", "SCALE_SCORE")


# State_A_Data_LONG[, as.list(round(summary(SCALE_SCORE_STANDARDIZED), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]


# x <- SGPstateData[["DEMO_COVID"]][["Achievement"]][["Cutscores"]][["ELA"]][["GRADE_7"]]
# y <- State_A_Data_LONG[YEAR == "2019" & CONTENT_AREA == "ELA" & GRADE == "7", SCALE_SCORE]
# (x - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
#
# yy <- State_A_Data_LONG[YEAR == "2019" & CONTENT_AREA == "ELA" & GRADE == "7", SCALE_SCORE_STANDARDIZED]
#
# new.scale <- (yy * roundInt(sd(y, na.rm = TRUE), 50)) + roundInt(mean(y, na.rm = TRUE), 100)
#
# table(State_A_Data_LONG[YEAR == "2019" & CONTENT_AREA == "ELA" & GRADE == "7", SCALE_SCORE_STANDARDIZED >= -0.3601071, SCALE_SCORE >= 500])
# table(State_A_Data_LONG[YEAR == "2019" & CONTENT_AREA == "ELA" & GRADE == "7", SCALE_SCORE_STANDARDIZED >= -0.3601071, ACHIEVEMENT_LEVEL])


# getShiftedValues DOES NOT add in 2020 (YEAR completely missing from data) - add in with missing_data
missing_data <- data.table(GRADE = c(3:8, 3:8), CONTENT_AREA = c(rep("ELA", 6), rep("MATHEMATICS", 6)), YEAR = "2020")
State_A_Data_LONG <- rbindlist(list(State_A_Data_LONG, missing_data), fill=TRUE)

shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(State_A_Data_LONG, shift.key)

getShiftedValues(State_A_Data_LONG, shift_amount = c(1L, 2L), shift_variable = c("SCALE_SCORE"))

#  Clean up - remove 2020 dummy data and rename according to old conventions
State_A_Data_LONG <- State_A_Data_LONG[YEAR != '2020']
setnames(State_A_Data_LONG, gsub("LAG_1", "PRIOR_1YEAR", names(State_A_Data_LONG)))
setnames(State_A_Data_LONG, gsub("LAG_2", "PRIOR_2YEAR", names(State_A_Data_LONG)))

##    Fix 2021 Lags for Grades 3 & 4 - repeaters:
# table(State_A_Data_LONG[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(State_A_Data_LONG[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"), SCALE_SCORE_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016", SCALE_SCORE_PRIOR_1YEAR := NA]


State_A_Data_LONG[, PRIOR_SCORE_for_IMPACT := simulatePriorScore(.SD),
        by = c("YEAR", "CONTENT_AREA", "GRADE"),
        .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE", "SCALE_SCORE_PRIOR_2YEAR")]

State_A_Data_LONG[YEAR == "2021" & is.na(PRIOR_SCORE_for_IMPACT), PRIOR_SCORE_for_IMPACT := SCALE_SCORE]

State_A_Data_LONG[, PRIOR_SCORE_DECILE :=
                as.integer(cut(PRIOR_SCORE_for_IMPACT,
                  breaks=quantile(PRIOR_SCORE_for_IMPACT,
                  probs=seq(0,1, by=0.1), na.rm=TRUE), include.lowest=TRUE)),
              keyby=c("YEAR", "CONTENT_AREA", "GRADE")]

# prior.decile.tbl <- table(State_A_Data_LONG[,
#                             .(PRIOR_SCORE_DECILE, CONTENT_AREA), GRADE])

# rbindlist(list(
#       dcast(data.table(prior.decile.tbl[,,1]), GRADE ~ PRIOR_SCORE_DECILE),
#       dcast(data.table(prior.decile.tbl[,,2]), GRADE ~ PRIOR_SCORE_DECILE)))

# State_A_Data_LONG[, SCALE_SCORE_PRIOR := SCALE_SCORE_PRIOR_1YEAR]
# State_A_Data_LONG[YEAR == "2021", SCALE_SCORE_PRIOR := SCALE_SCORE_PRIOR_2YEAR]

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

# qtile.impact <- list(
#   ELA = list(
#     "3" = sort(round(runif(10, 25, 40), 0)),
#     "4" = sort(round(runif(10, 25, 40), 0)),
#     "5" = sort(round(runif(10, 35, 45), 0)),
#     "6" = sort(round(runif(10, 35, 45), 0)),
#     "7" = sort(round(runif(10, 35, 48), 0)),
#     "8" = sort(round(runif(10, 35, 48), 0))),
#   MATHEMATICS = list(
#     "3" = sort(round(runif(10, 20, 35), 0)),
#     "4" = sort(round(runif(10, 20, 35), 0)),
#     "5" = sort(round(runif(10, 30, 40), 0)),
#     "6" = sort(round(runif(10, 30, 40), 0)),
#     "7" = sort(round(runif(10, 30, 45), 0)),
#     "8" = sort(round(runif(10, 30, 45), 0)))
# )

quantile_impact <- data.table(CONTENT_AREA = rep(names(qtile.impact), each=6*10),
                              GRADE = rep(rep(as.character(3:8), each=10), 2),
                              PRIOR_SCORE_DECILE = rep(1:10, 12),
                              IMPACT_PERCENTILE = unlist(qtile.impact))

# table(quantile_impact[, .(PRIOR_SCORE_DECILE, CONTENT_AREA), GRADE])

setkey(quantile_impact, CONTENT_AREA, GRADE, PRIOR_SCORE_DECILE)
setkey(State_A_Data_LONG, CONTENT_AREA, GRADE, PRIOR_SCORE_DECILE)

State_A_Data_LONG <- quantile_impact[State_A_Data_LONG]
# table(State_A_Data_LONG[, YEAR, is.na(IMPACT_PERCENTILE)])
# data <- copy(State_A_Data_LONG)


###   Create new version of SCALE_SCORE with impact added in as specified
State_A_Data_LONG[, SCALE_SCORE_IMPACTED := SCALE_SCORE]
State_A_Data_LONG[YEAR == "2021",
             SCALE_SCORE_IMPACTED :=
                addImpact(.SD), by = list(CONTENT_AREA, GRADE),
                .SDcols = c("CONTENT_AREA", "GRADE",
                            "SCALE_SCORE", "PRIOR_SCORE_for_IMPACT",
                            "PRIOR_SCORE_DECILE", "IMPACT_PERCENTILE")]

###   Pull in values between loss/hoss
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

# hist(State_A_Data_LONG[YEAR=='2021' & CONTENT_AREA == "ELA", SCALE_SCORE_IMPACTED - SCALE_SCORE])
# hist(State_A_Data_LONG[YEAR=='2021' & CONTENT_AREA == "MATHEMATICS", SCALE_SCORE_IMPACTED - SCALE_SCORE])
# hist(tst[YEAR=='2021' & CONTENT_AREA == "MATHEMATICS", SCALE_SCORE - SCALE_SCORE_without_COVID_IMPACT], breaks = 30)

State_A_Data_LONG[, SCALE_SCORE := NULL]
State_A_Data_LONG[, ACHIEVEMENT_LEVEL := NULL]
setnames(State_A_Data_LONG, "SCALE_SCORE_IMPACTED", "SCALE_SCORE")

State_A_Data_LONG <- SGP:::getAchievementLevel(State_A_Data_LONG, state = "DEMO_COVID")
# round(prop.table(table(State_A_Data_LONG[, .(YEAR, CONTENT_AREA), ACHIEVEMENT_LEVEL]), 2)*100, 1)

###   Remove variables used in simulating impact and no longer needed.
State_A_Data_LONG[, c("PRIOR_SCORE_for_IMPACT",
                      "PRIOR_SCORE_DECILE",
                      "IMPACT_PERCENTILE") := NULL]
