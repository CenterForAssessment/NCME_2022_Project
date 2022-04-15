################################################################################
####                                                                        ####
####             Create 2019 and 2021 Baseline SGPs for State A             ####
####                                                                        ####
################################################################################


###   Load packages
require(SGP)

###   Load baseline matrices for Demonstration Covid
load("../../../Universal_Content/Data/Baseline_Matrices/DEMO_COVID_Baseline_Matrices-SingleCohort.Rdata")

###   Modify SGPstateData
SGPstateData[["State_A"]] <- SGPstateData[["DEMO_COVID"]]
SGPstateData[["State_A"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- DEMO_COVID_Baseline_Matrices
SGPstateData[["State_A"]][["Growth"]][["Levels"]] <-
SGPstateData[["State_A"]][["Growth"]][["Cutscores"]] <-
SGPstateData[["State_A"]][["SGP_Configuration"]][["percentile.cuts"]] <- NULL

###   Load and combine SGP config scripts

source("SGP_CONFIG/BASELINE_SGP/ELA.R")
source("SGP_CONFIG/BASELINE_SGP/MATHEMATICS.R")

baseline.config <- c(ELA_2021.config,
                     MATHEMATICS_2021.config,
                     ELA_2019.config,
                     MATHEMATICS_2019.config)

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
