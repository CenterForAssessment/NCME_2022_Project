##############################################################################
###
###   State A Impact Visualization
###   General Setup (assumes running from ./Documentation directory)
###
##############################################################################

##    Load packages
require(data.table)
require(SGP)
require(splines)
require(grid)
require(quantreg)
require(hexbin)

##    Source covidImpactVisualization function
source("../../../Universal_Content/Functions/covidImpactVisualization.R")
source("../../../Universal_Content/Functions/covidImpactOverviewVisualization.R")

##    Source covidImpactVisualization Utility functions
source("../../../Universal_Content/Functions/covidImpactVisualization_UTILITIES.R")

##    Load formated Report_Data and Imputed Data
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
# if (!exists("State_A_SGP_Data_Imputed")) load("../Data/Imputation/State_A_SGP_Data_Imputed.rda")

##     NULL out portion of Academic_Impact_Overview
if (file.exists("../Data/Academic_Impact_Overview.Rdata")) {
    load("../Data/Academic_Impact_Overview.Rdata")
    Academic_Impact_Overview[['State_Assessment']] <- NULL
    save(Academic_Impact_Overview, file="../Data/Academic_Impact_Overview.Rdata")
}

##    Define parameters
parameters <- list()
parameters[['assessment_type']] <- "State_Assessment"
parameters[['include.imputations']] <- FALSE
parameters[['universal.content.path']] <- file.path("..", "..", "..", "Universal_Content")

parameters[['state_abb']] <- "DEMO_COVID"
parameters[['assessment_abb']] <- "State_A"
parameters[['current_year']] <- "2021"
parameters[['prior_year']] <- "2019"
parameters[['prior_year_base']] <- "2017"
parameters[['content_area']] <- c("ELA", "MATHEMATICS")
parameters[['content_area_label']][["ELA"]] <- "ELA"
parameters[['content_area_label']][["MATHEMATICS"]] <- "Mathematics"
parameters[['academic_impact_label']][["ELA"]] <- "ELA"
parameters[['academic_impact_label']][["MATHEMATICS"]] <- "Mathematics"
parameters[['achievement_levels']] <- SGPstateData[[parameters[['state_abb']]]][['Achievement']][['Levels']][['Labels']]
parameters[['achievement_levels_proficient']] <- SGPstateData[[parameters[['state_abb']]]][['Achievement']][['Levels']][['Proficient']]
parameters[['current_grade']] <- c("3", "4", "5", "6", "7", "8")
parameters[['prior_grade']] <- c(NA, NA, "3", "4", "5", "6")

# Report_Data[[parameters[['assessment_type']]]][!is.na(SCALE_SCORE) & YEAR %in% 2019:2021, as.list(summary(SCALE_SCORE)), keyby=c("GRADE", "YEAR")]
parameters[["ELA"]][['x_axis_range']] <- c(400, 650)
parameters[["ELA"]][['x_axis_ticks']] <- c(400, 450, 500, 550, 600, 650)
parameters[["ELA"]][['y_axis_range']] <- c(400, 650)
parameters[["ELA"]][['y_axis_ticks']] <- c(400, 450, 500, 550, 600, 650)
parameters[["ELA"]][['academic_impact_axis_range_BL']] <- c(-35, 35)
parameters[["ELA"]][['academic_impact_axis_ticks_BL']] <- seq(-35, 35, by=5)
parameters[["ELA"]][['academic_impact_axis_range_BR']] <- c(-35, 35)
parameters[["ELA"]][['academic_impact_axis_ticks_BR']] <- seq(-35, 35, by=5)
parameters[["ELA"]][['x_axis_range_cohort_comparison']] <- c(0, 100)
parameters[["ELA"]][['x_axis_range_cohort_comparison_ticks']] <- seq(0, 100, by=10)
parameters[["ELA"]][['x_axis_range_cohort_comparison_scale_score']] <- parameters[["ELA"]][['x_axis_range']]
parameters[["ELA"]][['x_axis_range_cohort_comparison_scale_score_ticks']] <- c(400, 500, 600, 650)
parameters[["MATHEMATICS"]][['x_axis_range']] <- c(400, 650)
parameters[["MATHEMATICS"]][['x_axis_ticks']] <- c(400, 450, 500, 550, 600, 650)
parameters[["MATHEMATICS"]][['y_axis_range']] <- c(400, 650)
parameters[["MATHEMATICS"]][['y_axis_ticks']] <- c(400, 450, 500, 550, 600, 650)
parameters[["MATHEMATICS"]][['academic_impact_axis_range_BL']] <- c(-35, 35)
parameters[["MATHEMATICS"]][['academic_impact_axis_ticks_BL']] <- seq(-35, 35, by=5)
parameters[["MATHEMATICS"]][['academic_impact_axis_range_BR']] <- c(-35, 35)
parameters[["MATHEMATICS"]][['academic_impact_axis_ticks_BR']] <- seq(-35, 35, by=5)
parameters[["MATHEMATICS"]][['x_axis_range_cohort_comparison']] <- c(0, 100)
parameters[["MATHEMATICS"]][['x_axis_range_cohort_comparison_ticks']] <- seq(0, 100, by=10)
parameters[["MATHEMATICS"]][['x_axis_range_cohort_comparison_scale_score']] <- parameters[["MATHEMATICS"]][['x_axis_range']]
parameters[["MATHEMATICS"]][['x_axis_range_cohort_comparison_scale_score_ticks']] <- c(400, 500, 600, 650)
parameters[['graphic_format']][['Mode']] <- "Print"##"Presentation"
if (parameters[['graphic_format']][['Mode']]=="Print") {
     parameters[['graphic_format']][['colors_background']] <- rgb(0.985, 0.985, 1.0)
     parameters[['graphic_format']][['colors_border']] <- "grey20"
     parameters[['graphic_format']][['colors_font']] <- c("grey20", rgb(0.985, 0.985, 1.0))
}
if (parameters[['graphic_format']][['Mode']]=="Presentation") {
     parameters[['graphic_format']][['colors_background']] <- rgb(0.48, 0.48, 0.52)
     parameters[['graphic_format']][['colors_border']] <- rgb(0.985, 0.985, 1.0)
     parameters[['graphic_format']][['colors_font']] <- c(rgb(0.985, 0.985, 1.0), rgb(0.48, 0.48, 0.52))
}
parameters[['graphic_format']][['fig_width']] <- 17
parameters[['graphic_format']][['fig_height']] <- 11
parameters[['graphic_format']][['file.path']] <- file.path("assets", "Rplots", "Impact", parameters[['assessment_type']], "CONDITIONAL_STATUS")
parameters[['proficient_cutscore_level']] <- 2
parameters[["scale_score_round_digits"]] <- 0

##   Plot Directory
if (!dir.exists(parameters[['graphic_format']][['file.path']])) dir.create(parameters[['graphic_format']][['file.path']], recursive=TRUE)

##   Merge imputed data
if (parameters[['include.imputations']]) {
    long_data <- mergeImputedData(Report_Data[[parameters[['assessment_type']]]], State_A_SGP_Data_Imputed, parameters)
} else {
    long_data <- Report_Data[[parameters[['assessment_type']]]]
}

###############################################################################
### Create Academic Impact Visualization GRADE x CONTENT_AREA
###############################################################################
for (content_area.iter in parameters[['content_area']]) {
  for (grade.iter in seq_along(parameters[['current_grade']])) {
    tmp_data <- getData(
                    long_data=long_data,
                    current_year=parameters[['current_year']],
                    prior_year=parameters[['prior_year']],
                    content_area=content_area.iter,
                    current_grade=parameters[['current_grade']][grade.iter],
                    prior_grade=parameters[['prior_grade']][grade.iter],
                    parameters=parameters)

    covidImpactVisualization(
                    tmp_data=tmp_data,
                    content_area=content_area.iter,
                    current_grade=parameters[['current_grade']][grade.iter],
                    prior_grade=parameters[['prior_grade']][grade.iter],
                    parameters=parameters,
                    student_group=NULL,
                    output_type="PDF",
                    save_grobs = TRUE)

    addStatusGrowthSummariestoOveriew(tmp_data, parameters[['assessment_type']], content_area.iter, grade.iter)
    }
}

### Create Academic Impact Overview Visualization
load("../Data/Academic_Impact_Overview.Rdata")

covidImpactOverviewVisualization(
                 Academic_Impact_Overview=Academic_Impact_Overview,
                 parameters=parameters,
                 output_type="PDF",
                 academic_impact_metric="Hybrid",  ## Hybrid or Status
                 academic_impact_groups="ALL_STUDENTS") # c("ALL_STUDENTS", student_groups[['TITLE_LABEL']])


###   Create Academic Impact Catalog
makeCatalog()
