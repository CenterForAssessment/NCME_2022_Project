#+ ida-sgp, include = FALSE, purl = FALSE
################################################################################
####                                                                        ####
####             Create 2019 and 2021 Baseline SGPs for State A             ####
####                                                                        ####
################################################################################

#' ## Create 2019 and 2021 Baseline SGPs for State A
#'
#' The baseline SGP analysis section of the appendix assumes the user is operating
#' with their working directory set to "*NCME_2022_Project/All_States/State_A/Initial_Data_Analysis*".

#+ ida-sgp-wd, echo = TRUE, purl = TRUE
# setwd("./Initial_Data_Analysis")

#' ### Load packages and custom functions.
#'
#' The `SGP` package is required for the data analysis with simulated data.
#'
#+ ida-sgp-pkg, echo = TRUE, purl = TRUE
require(SGP)

#' ### Load baseline matrices for Demonstration Covid
#'
#' These coefficient matrices were created using the same *`sgpData_LONG_COVID`*
#' data set that our simulated impact data comes from. Baselines were established
#' with data from the 2019 cohort.
#'
#+ ida-sgp-bslnmtrx, echo = TRUE, purl = TRUE
load("../../../Universal_Content/Data/Baseline_Matrices/DEMO_COVID_Baseline_Matrices-SingleCohort.Rdata")

#' ### Modify SGPstateData
#'
#' In these simulation analyses, we want to simulate varyious levels and patterns
#' of academic impact using the same base data set. We will call them by different
#' names, but ultimately they come from the "Demonstration_COVID" (abbreviated
#' "DEMO_COVID") data and will use the same meta-data stored in the `SGPstateData`
#' object. This meta-data is required to use various functions in the `SGP` package
#' as well as some custom functions that have been created to analyze academic impact.
#'
#+ ida-sgp-meta, echo = TRUE, purl = TRUE
SGPstateData[["State_A"]] <- SGPstateData[["DEMO_COVID"]]
SGPstateData[["State_A"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- DEMO_COVID_Baseline_Matrices
SGPstateData[["State_A"]][["Growth"]][["Levels"]] <-
SGPstateData[["State_A"]][["Growth"]][["Cutscores"]] <-
SGPstateData[["State_A"]][["SGP_Configuration"]][["percentile.cuts"]] <- NULL

#' ### Load and combine SGP config scripts
#'
#' Because we are incorporating the skipped year (2020) of testing, we need to
#' manually specify which SGP progressions we want to run. That is, which unique
#' year-by-grade-by-content area combinations will define each cohort of students
#' included in an analysis.
#'
#' NOTE TO SELF - read in and print out the progressions as a part of this appendix (in PARENT document)
#'
#+ ida-sgp-config, echo = TRUE, purl = TRUE
source("SGP_CONFIG/BASELINE_SGP/ELA.R")
source("SGP_CONFIG/BASELINE_SGP/MATHEMATICS.R")

baseline.config <- c(ELA_2021.config,
                     MATHEMATICS_2021.config,
                     ELA_2019.config,
                     MATHEMATICS_2019.config)

#' ### Calculate baseline SGPs
#'
#' We use the `abcSGP` function from the `SGP` package to create 2019 and 2021
#' baseline referenced SGPs. We need the 2019 values for comparisons with 2021
#' results. We expect the typical growth for 2019 to be near 50 (the baselines)
#' were created using their data), but expect typical growth to be lower for 2021.
#'
#+ ida-sgp-abcsgp, echo = TRUE, message = FALSE, purl = TRUE
State_A_SGP <- abcSGP(sgp_object = State_A_Data_LONG,
                      state = "State_A",
                    	steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                      sgp.config = baseline.config,
                      sgp.percentiles = FALSE,
                    	sgp.projections = FALSE,
                    	sgp.projections.lagged = FALSE,
                    	sgp.percentiles.baseline = TRUE,
                    	sgp.projections.baseline = FALSE,
                    	sgp.projections.lagged.baseline=FALSE,
                      simulate.sgps=FALSE)

#' Since we will only be using these results for creating reports, we will remove
#' all extraneous (pdf) versions of the model fit plots (created in the "Goodness_of_Fit"
#' directory at completion of the call to `abcSGP` above). We also only want the
#' results stored in the `State_A_SGP@Data` portion of the object, so we do not
#' save the object in this step either. Instead we will subset that out and modify
#' the data in the next section before finally saving in a named list (`Report_Data`).
#'
#'
#+ ida-sgp-trimrepo, echo = FALSE, message = FALSE, purl = TRUE
all.files <- list.files("Goodness_of_Fit", recursive = TRUE, full.names = TRUE)
flrm.tf <- file.remove(grep(".pdf|.Rdata", all.files, value = TRUE))
unlk.tf <- unlink(grep("Decile_Tables", list.dirs(), value=TRUE), recursive = TRUE)
## reset working directory to State_A
#  setwd("..")

#' ### Summary and notes
#'
#'   * This was a very minimal SGP analysis. Only baseline SGPs were created and
#'     we did not produce any (cohort or baseline referenced) SGP targets. The
#'     goal of this step was simply to create Baseline SGPs and merge them into
#'     the longitudinal data so that we can proceed to the next step.
#'     - Baseline SGP Goodness of Fit plots were produced, which serve as good
#'       diagnostics for the quality of the simulated impact from the data prep step.
