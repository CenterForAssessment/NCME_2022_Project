#####
###   Create the `Report_Data` object with data-(sub)sets for the Learning Loss
###   Analysis Report. House all data in one object that is passed to the report
###   (and in some cases used to create the report `params`). Create, format,
###   alter, augment one or more raw data sets.
#####


###   Set R working directory to the Documentation folder
# setwd("./Documentation")

###   Load required packages
require(SGP)
require(data.table)
require(cfaTools)

#####
###   State_Assessment
#####

###   Using `State_A_SGP` directly from "Initial_Analysis"

rm(State_A_Data_LONG)
State_A_Data_LONG <- copy(State_A_SGP@Data)


###   Create Lagged Scale Score (ACTUAL and STANDARDIZED) and achievement level variables that include missing scores (and others potentially)
##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
State_A_Data_LONG[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = "2019"),
    by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]

#   Need to run this again here to get SCALE_SCORE_PRIOR_*YEAR
#   Seems to get deleted in abcSGP - names are too close! Something to look into.

#   getShiftedValues DOES NOT add in 2020 (YEAR completely missing from data) - add in with missing_data
missing_data <- data.table(GRADE = c(3:8, 3:8), CONTENT_AREA = c(rep("ELA", 6), rep("MATHEMATICS", 6)), YEAR = "2020")
State_A_Data_LONG <- rbindlist(list(State_A_Data_LONG, missing_data), fill=TRUE)

shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(State_A_Data_LONG, shift.key)

getShiftedValues(State_A_Data_LONG, shift_amount = c(1L, 2L),
                 shift_variable = c("SCALE_SCORE", "SCALE_SCORE_STANDARDIZED", "ACHIEVEMENT_LEVEL"))
# table(State_A_Data_LONG[YEAR==current.year, ACHIEVEMENT_LEVEL_PRIOR, ACHIEVEMENT_LEVEL_LAG_2], exclude=NULL)

#  Clean up - remove 2020 dummy data and rename according to old conventions
State_A_Data_LONG <- State_A_Data_LONG[YEAR != '2020']
setnames(State_A_Data_LONG, gsub("LAG_1", "PRIOR_1YEAR", names(State_A_Data_LONG)))
setnames(State_A_Data_LONG, gsub("LAG_2", "PRIOR_2YEAR", names(State_A_Data_LONG)))

##    Fix 2021 Lags for Grades 3 & 4 - repeaters:
# table(State_A_Data_LONG[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(State_A_Data_LONG[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"), SCALE_SCORE_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"), SCALE_SCORE_STANDARDIZED_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE %in% c(3, 4) | YEAR %in% c("2016", "2017"), ACHIEVEMENT_LEVEL_PRIOR_2YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016", SCALE_SCORE_PRIOR_1YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016", SCALE_SCORE_STANDARDIZED_PRIOR_1YEAR := NA]
State_A_Data_LONG[GRADE == 3 | YEAR == "2016", ACHIEVEMENT_LEVEL_PRIOR_1YEAR := NA]

###   From Nathan's
State_A_Data_LONG[, ACHIEVEMENT_ProfandAbove := fcase(
  ACHIEVEMENT_LEVEL %in% c("Partially Proficient", "Unsatisfactory"), "Not Proficient",
  ACHIEVEMENT_LEVEL %in% c("Advanced", "Proficient"), "Proficient")]

State_A_Data_LONG[YEAR == "2021", ACHIEVEMENT_LEVEL_PRIOR := ACHIEVEMENT_LEVEL_PRIOR_2YEAR]
State_A_Data_LONG[YEAR != "2021", ACHIEVEMENT_LEVEL_PRIOR := ACHIEVEMENT_LEVEL_PRIOR_1YEAR]
State_A_Data_LONG[, PRIOR_ACHIEVEMENT_ProfandAbove := fcase(
  ACHIEVEMENT_LEVEL_PRIOR %in% c("Partially Proficient", "Unsatisfactory"), "Not Proficient",
  ACHIEVEMENT_LEVEL_PRIOR %in% c("Advanced", "Proficient"), "Proficient")]

###   Remove redundant/replaced variables
State_A_Data_LONG[, c("SCALE_SCORE_PRIOR",
                      "SCALE_SCORE_PRIOR_STANDARDIZED",
                      "ACHIEVEMENT_LEVEL_PRIOR",
                      "SGP_NORM_GROUP_BASELINE",
                      "SGP_NORM_GROUP_BASELINE_SCALE_SCORES") := NULL]

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
###   Combine all data sources into `Report_Data` and Save
#####

Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(State_A_Data_LONG); rm(State_A_SGP); rm(State_A_Data_LONG)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
