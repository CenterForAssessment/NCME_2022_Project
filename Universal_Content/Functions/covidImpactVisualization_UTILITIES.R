#####
###  covidImpactVisualization Utility functions
#####

percent_proficient <- function(variable, achievement_levels, proficient_achievement_levels) {
    tmp.table <- table(variable)
    round(100*sum(tmp.table[achievement_levels[proficient_achievement_levels=="Proficient"]], na.rm=TRUE)/sum(tmp.table[achievement_levels], na.rm=TRUE), digits=1)
}

participation_rate <- function(numerator, denominator) {
    round(100*(sum(!is.na(numerator))/length(denominator)), digits=1)
}

getCutscores <- function(state_abb, content_area, year) {
    tmp_names <- names(SGPstateData[[state_abb]][['Achievement']][['Cutscores']])
    if (paste(content_area, year, sep=".") %in% tmp_names) {
        cutscore_name <- paste(content_area, year, sep=".")
    } else {
        tmp_names <- sort(c(grep(paste0("^",content_area, "$"), tmp_names, value=TRUE), paste(content_area, year, sep="."), grep(paste0("^",content_area, "\\."), tmp_names, value=TRUE)))
        cutscore_name <- tmp_names[which(paste(content_area, year, sep=".")==tmp_names)-1]
    }
    return(SGPstateData[[state_abb]][['Achievement']][['Cutscores']][[cutscore_name]])
} ### END getCutScores

my_quantile <- function(my_data, probs, na.rm=TRUE) {
    if (length(tmp_quantile <- quantile(my_data, probs=probs, na.rm=na.rm))==length(unique(length(tmp_quantile)))) {
        return(tmp_quantile)
    } else {
        quantile(my_data+rnorm(length(my_data), sd=0.001), probs=probs, na.rm=na.rm)
    }
}

getData <- function(long_data, current_year, prior_year, content_area, current_grade, prior_grade, student_group=NULL, parameters) {
    ### Checks on parameters
    current.ss.to.use <- ifelse(parameters[['include.imputations']], "MEAN_SCALE_SCORE_IMPUTED", "SCALE_SCORE")
    content.area.label <- ifelse(is.null(parameters[['content_area_label']]), capwords(content_area), gsub(" |-", "_", parameters[['content_area_label']][[content_area]]))

    if (!is.null(parameters[['prior_score_variable']])) {
        long_data[, ACADEMIC_IMPACT_SCALE_SCORE_PRIOR := get(parameters[['prior_score_variable']])]
    } else long_data[, ACADEMIC_IMPACT_SCALE_SCORE_PRIOR := SCALE_SCORE_PRIOR_2YEAR]

    ## List object to hold data
    tmp_data <- list()

    ## Create current and prior data sets
    setkeyv(long_data, c("CONTENT_AREA", "GRADE", "YEAR"))
    if (!is.null(student_group)) {
        setkeyv(long_data, c("CONTENT_AREA", "GRADE", "YEAR", student_group[['STUDENT_GROUP']]))
        tmp_data[['current_data_no_subset']] <- long_data[list(content_area, current_grade, current_year)]
        tmp_data[['prior_data_no_subset']] <- long_data[list(content_area, current_grade, prior_year)]
        tmp_data[['current_data']] <- long_data[list(content_area, current_grade, current_year, student_group[['STUDENT_GROUP_LABEL']])]
        tmp_data[['prior_data']] <- long_data[list(content_area, current_grade, prior_year, student_group[['STUDENT_GROUP_LABEL']])]
    } else {
        setkeyv(long_data, c("CONTENT_AREA", "GRADE", "YEAR"))
        tmp_data[['current_data']] <- long_data[list(content_area, current_grade, current_year)]
        tmp_data[['prior_data']] <- long_data[list(content_area, current_grade, prior_year)]
    }

    ## Create Cutscores
    tmp_data[['achievement_cutscores_prior']] <- getCutscores(parameters[['state_abb']], content_area, prior_year)
    tmp_data[['achievement_cutscores_current']] <- getCutscores(parameters[['state_abb']], content_area, current_year)

    ### Create file_path
    if (!is.null(student_group)) {
        tmp_data[['file_path']] <- file.path(parameters[['graphic_format']][['file.path']], paste0(student_group[['DIRECTORY_LABEL']], "_by_CONTENT_AREA_by_GRADE"))
        tmp_data[['file_name']] <- paste0("Academic_Impact_", content.area.label, "_Grade_", current_grade, "_", student_group[['FILE_LABEL']], ".pdf")
    } else {
        tmp_data[['file_path']] <- file.path(parameters[['graphic_format']][['file.path']], "CONTENT_AREA_by_GRADE")
        tmp_data[['file_name']] <- paste0("Academic_Impact_", content.area.label, "_Grade_", current_grade, ".pdf")
    }

    ## Create percent proficient summary statistics
    tmp_data[['percent_proficient_prior']] <- percent_proficient(variable=tmp_data[['prior_data']][['ACHIEVEMENT_LEVEL']], parameters[['achievement_levels']], parameters[['achievement_levels_proficient']])
    tmp_data[['percent_proficient_current']] <- percent_proficient(variable=tmp_data[['current_data']][['ACHIEVEMENT_LEVEL']], parameters[['achievement_levels']], parameters[['achievement_levels_proficient']])

    ## Create knots/boundaries/breaks
    if (!is.na(prior_grade)){
        tmp_data[['knots']] <- my_quantile(c(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], tmp_data[['prior_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']]), probs=c(1, 3, 5, 7, 9)/10)
        tmp_data[['boundaries']] <- extendrange(c(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], tmp_data[['prior_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']]))
    }

    ## Create DECILE, QUINTILE, and PERCENTILE cuts of current SCALE_SCORE for current_data and prior_data
    ### DECILE
    tmp_data[['decile_breaks_current_data']] <-  my_quantile(tmp_data[['current_data']][['SCALE_SCORE']], probs=0:10/10)
    if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['decile_breaks_current_data_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][['SCALE_SCORE']], probs=0:10/10)
    if (parameters[['include.imputations']]) tmp_data[['decile_breaks_current_data_imputed']] <-  my_quantile(tmp_data[['current_data']][[current.ss.to.use]], probs=0:10/10)
    if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['decile_breaks_current_data_imputed_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][[current.ss.to.use]], probs=0:10/10)
    tmp_data[['decile_breaks_prior_data']] <-  my_quantile(tmp_data[['prior_data']][['SCALE_SCORE']], probs=0:10/10)
    if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['decile_breaks_prior_data_no_subset']] <- my_quantile(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], probs=0:10/10)
    ### QUINTILE
    tmp_data[['quintile_breaks_current_data']] <-  my_quantile(tmp_data[['current_data']][['SCALE_SCORE']], probs=c(0,2,4,6,8,10)/10)
    if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['quintile_breaks_current_data_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][['SCALE_SCORE']], probs=c(0,2,4,6,8,10)/10)
    tmp_data[['quintile_breaks_prior_data']] <-  my_quantile(tmp_data[['prior_data']][['SCALE_SCORE']], probs=c(0,2,4,6,8,10)/10)
    if (!is.null(tmp_data[['prior_data_no_subset']])) tmp_data[['quintile_breaks_prior_data_no_subset']] <- my_quantile(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], probs=c(0,2,4,6,8,10)/10)
    ### PERCENTILE
    tmp_data[['percentile_breaks_current_data']] <-  my_quantile(tmp_data[['current_data']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
    if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['percentile_breaks_current_data_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
    if (parameters[['include.imputations']]) tmp_data[['percentile_breaks_current_data_imputed']] <-  my_quantile(tmp_data[['current_data']][[current.ss.to.use]], probs=seq(0.005, 0.995, length=100))
    tmp_data[['percentile_breaks_prior_data']] <-  my_quantile(tmp_data[['prior_data']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
    if (!is.null(tmp_data[['prior_data_no_subset']])) tmp_data[['percentile_breaks_prior_data_no_subset']] <- my_quantile(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))


    if (!is.na(prior_grade)){
        tmp_data[['decile_breaks_scale_score_prior_current_data']] <-  my_quantile(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=0:10/10)
        if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['decile_breaks_scale_score_prior_no_subset_current_data']] <-  my_quantile(tmp_data[['current_data_no_subset']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=0:10/10)
        tmp_data[['decile_breaks_scale_score_prior_prior_data']] <-  my_quantile(tmp_data[['prior_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=0:10/10)
        if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['decile_breaks_scale_score_prior_no_subset_prior_data']] <-  my_quantile(tmp_data[['prior_data_no_subset']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=0:10/10)
        tmp_data[['quintile_breaks_scale_score_prior_current_data']] <-  my_quantile(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=c(0,2,4,6,8,10)/10)
        if (!is.null(tmp_data[['current_data_no_subset']])) tmp_data[['quintile_breaks_scale_score_prior_no_subset_current_data']] <-  my_quantile(tmp_data[['current_data_no_subset']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=c(0,2,4,6,8,10)/10)
        tmp_data[['quintile_breaks_scale_score_prior_prior_data']] <-  my_quantile(tmp_data[['prior_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=c(0,2,4,6,8,10)/10)
        if (!is.null(tmp_data[['prior_data_no_subset']])) tmp_data[['quintile_breaks_scale_score_prior_no_subset_prior_data']] <-  my_quantile(tmp_data[['prior_data_no_subset']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=c(0,2,4,6,8,10)/10)
    }

    ## Add SCALE_SCORE_PRIOR_QUINTILES variable to current_data and prior_data sets
    if (!is.na(prior_grade)) {
        if (!is.null(tmp_data[['current_data_no_subset']])) {
            tmp_data[['current_data']][,SCALE_SCORE_PRIOR_QUINTILES:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_no_subset_current_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['prior_data']][,SCALE_SCORE_PRIOR_QUINTILES:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_no_subset_prior_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['current_data']][,SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_current_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['prior_data']][,SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_prior_data']], labels=1:5, include.lowest=TRUE)]
        } else {
            tmp_data[['current_data']][,SCALE_SCORE_PRIOR_QUINTILES:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_current_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['prior_data']][,SCALE_SCORE_PRIOR_QUINTILES:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_prior_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['current_data']][,SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_current_data']], labels=1:5, include.lowest=TRUE)]
            tmp_data[['prior_data']][,SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS:=cut(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, breaks=tmp_data[['quintile_breaks_scale_score_prior_prior_data']], labels=1:5, include.lowest=TRUE)]
        }
        if (parameters[['include.imputations']]) {
          tmp_data[['current_data_summaries_quintiles_subgroup_cuts']] <- tmp_data[['current_data']][,
                                                          list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                              MEDIAN_SGP_BASELINE_IMPUTED=median(MEAN_SGP_BASELINE_IMPUTED, na.rm=TRUE),
                                                              COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE)),
                                                              COUNT_GROWTH_IMPUTED=sum(!is.na(MEAN_SGP_BASELINE_IMPUTED))),
                                                          keyby="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"][.(SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"]
          tmp_data[['current_data_summaries_quintiles']] <- tmp_data[['current_data']][,
                                                          list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                              MEDIAN_SGP_BASELINE_IMPUTED=median(MEAN_SGP_BASELINE_IMPUTED, na.rm=TRUE),
                                                              COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE)),
                                                              COUNT_GROWTH_IMPUTED=sum(!is.na(MEAN_SGP_BASELINE_IMPUTED))),
                                                          keyby="SCALE_SCORE_PRIOR_QUINTILES"][.(SCALE_SCORE_PRIOR_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES"]
        } else {
          tmp_data[['current_data_summaries_quintiles_subgroup_cuts']] <- tmp_data[['current_data']][,
                                                          list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                              COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                          keyby="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"][.(SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"]
          tmp_data[['current_data_summaries_quintiles']] <- tmp_data[['current_data']][,
                                                          list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                              COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                          keyby="SCALE_SCORE_PRIOR_QUINTILES"][.(SCALE_SCORE_PRIOR_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES"]
        }

        tmp_data[['prior_data_summaries_quintiles_subgroup_cuts']] <- tmp_data[['prior_data']][,
                                                        list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"][.(SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES_SUBGROUP_CUTS"]
        tmp_data[['prior_data_summaries_quintiles']] <- tmp_data[['prior_data']][,
                                                        list(MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_PRIOR_QUINTILES"][.(SCALE_SCORE_PRIOR_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_PRIOR_QUINTILES"]

        ### Create QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS from quintiles for all students
        tmp_data[['quintile_group_cuts_and_percentages_growth']] <- data.table(
            QUINTILE_LEVEL=seq(0,5),
            QUINTILE_PERCENTAGES_SUBGROUP_SUBGROUP_CUTS=seq(0, 100, by=20)
        )
        if (!is.null(student_group)) {
            tmp_data[['quintile_group_cuts_and_percentages_growth']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=round(100*ecdf(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']])(tmp_data[['quintile_breaks_scale_score_prior_no_subset_current_data']]), digits=c(0,rep(2,4),0))]
        } else {
            tmp_data[['quintile_group_cuts_and_percentages_growth']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)]
        }

        ### Create percentile cuts for ACADEMIC_IMPACT_SCALE_SCORE_PRIOR and SCALE_SCORE for status plotting
        if (!is.null(tmp_data[['current_data_no_subset']])) {
            tmp_data[['plotting_domain_growth_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=seq(0.005, 0.995, length=100))
            tmp_data[['plotting_domain_growth']] <- my_quantile(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=seq(0.005, 0.995, length=100))
            tmp_data[['plotting_range_growth_no_subset']] <- my_quantile(tmp_data[['current_data_no_subset']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
            tmp_data[['plotting_range_growth']] <- my_quantile(tmp_data[['current_data']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
        } else {
            tmp_data[['plotting_domain_growth']] <- my_quantile(tmp_data[['current_data']][['ACADEMIC_IMPACT_SCALE_SCORE_PRIOR']], probs=seq(0.005, 0.995, length=100))
            tmp_data[['plotting_range_growth']] <- my_quantile(tmp_data[['current_data']][['SCALE_SCORE']], probs=seq(0.005, 0.995, length=100))
        }

        tmp_data[['plotting_domain_sequence_growth']] <- tmp_data[['plotting_domain_growth']][3:97]
    } ### END !is.na(prior_grade)

    ### Add SCALE_SCORE_QUINTILES and SCALE_SCORE_QUINTILES_SUBGROUP for aggregations
    if (!is.null(tmp_data[['prior_data_no_subset']])) {
        tmp_data[['current_data']][,SCALE_SCORE_QUINTILES:=cut(get(current.ss.to.use), breaks=tmp_data[['quintile_breaks_current_data_no_subset']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['prior_data']][,SCALE_SCORE_QUINTILES:=cut(SCALE_SCORE, breaks=tmp_data[['quintile_breaks_prior_data_no_subset']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['current_data']][,SCALE_SCORE_QUINTILES_SUBGROUP:=cut(get(current.ss.to.use), breaks=tmp_data[['quintile_breaks_current_data']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['prior_data']][,SCALE_SCORE_QUINTILES_SUBGROUP:=cut(SCALE_SCORE, breaks=tmp_data[['quintile_breaks_prior_data']], labels=1:5, include.lowest=TRUE)]
    } else {
        tmp_data[['current_data']][,SCALE_SCORE_QUINTILES:=cut(get(current.ss.to.use), breaks=tmp_data[['quintile_breaks_current_data']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['prior_data']][,SCALE_SCORE_QUINTILES:=cut(SCALE_SCORE, breaks=tmp_data[['quintile_breaks_prior_data']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['current_data']][,SCALE_SCORE_QUINTILES_SUBGROUP:=cut(get(current.ss.to.use), breaks=tmp_data[['quintile_breaks_current_data']], labels=1:5, include.lowest=TRUE)]
        tmp_data[['prior_data']][,SCALE_SCORE_QUINTILES_SUBGROUP:=cut(SCALE_SCORE, breaks=tmp_data[['quintile_breaks_prior_data']], labels=1:5, include.lowest=TRUE)]
    }

    if (parameters[['include.imputations']]) {
      tmp_data[['current_data_summaries_current_quintiles']] <- tmp_data[['current_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_IMPUTED=mean(MEAN_SCALE_SCORE_IMPUTED, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED=mean(MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_STATUS_IMPUTED=sum(!is.na(MEAN_SCALE_SCORE_IMPUTED)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE)),
                                                            COUNT_GROWTH_IMPUTED=sum(!is.na(MEAN_SGP_BASELINE_IMPUTED))),
                                                        keyby="SCALE_SCORE_QUINTILES"][.(SCALE_SCORE_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_QUINTILES"]
      tmp_data[['current_data_summaries_current_quintiles_subgroup']] <- tmp_data[['current_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_IMPUTED=mean(MEAN_SCALE_SCORE_IMPUTED, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED=mean(MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_STATUS_IMPUTED=sum(!is.na(MEAN_SCALE_SCORE_IMPUTED)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE)),
                                                            COUNT_GROWTH_IMPUTED=sum(!is.na(MEAN_SGP_BASELINE_IMPUTED))),
                                                        keyby="SCALE_SCORE_QUINTILES_SUBGROUP"][.(SCALE_SCORE_QUINTILES_SUBGROUP=as.factor(1:5)), on="SCALE_SCORE_QUINTILES_SUBGROUP"]
      tmp_data[['prior_data_summaries_current_quintiles']] <- tmp_data[['prior_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES"][.(SCALE_SCORE_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_QUINTILES"]
      tmp_data[['prior_data_summaries_current_quintiles_subgroup']] <- tmp_data[['prior_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES_SUBGROUP"][.(SCALE_SCORE_QUINTILES_SUBGROUP=as.factor(1:5)), on="SCALE_SCORE_QUINTILES_SUBGROUP"]
    } else {
      tmp_data[['current_data_summaries_current_quintiles']] <- tmp_data[['current_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES"][.(SCALE_SCORE_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_QUINTILES"]
      tmp_data[['current_data_summaries_current_quintiles_subgroup']] <- tmp_data[['current_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES_SUBGROUP"][.(SCALE_SCORE_QUINTILES_SUBGROUP=as.factor(1:5)), on="SCALE_SCORE_QUINTILES_SUBGROUP"]
      tmp_data[['prior_data_summaries_current_quintiles']] <- tmp_data[['prior_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES"][.(SCALE_SCORE_QUINTILES=as.factor(1:5)), on="SCALE_SCORE_QUINTILES"]
      tmp_data[['prior_data_summaries_current_quintiles_subgroup']] <- tmp_data[['prior_data']][,
                                                        list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE),
                                                            MEAN_SCALE_SCORE_STANDARDIZED=mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                                                            COUNT_STATUS_OBSERVED=sum(!is.na(SCALE_SCORE)),
                                                            COUNT_GROWTH_OBSERVED=sum(!is.na(SGP_BASELINE))),
                                                        keyby="SCALE_SCORE_QUINTILES_SUBGROUP"][.(SCALE_SCORE_QUINTILES_SUBGROUP=as.factor(1:5)), on="SCALE_SCORE_QUINTILES_SUBGROUP"]
    }

    ### CREATE QUINTILE PERCENTAGES (both ALL_STUDENTS and SUBGROUP) based upon all student quintile cuts
    tmp_data[['quintile_group_cuts_and_percentages_status_current']] <- data.table(
        QUINTILE_LEVEL=seq(0,5),
        QUINTILE_PERCENTAGES_SUBGROUP_SUBGROUP_CUTS=seq(0, 100, by=20)
    )

    tmp_data[['quintile_group_cuts_and_percentages_status_prior']] <- data.table(
        QUINTILE_LEVEL=seq(0,5),
        QUINTILE_PERCENTAGES_SUBGROUP_SUBGROUP_CUTS=seq(0, 100, by=20)
    )
    if (!is.null(student_group)) {
        tmp_data[['quintile_group_cuts_and_percentages_status_current']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=round(100*ecdf(tmp_data[['current_data']][['SCALE_SCORE']])(tmp_data[['quintile_breaks_current_data_no_subset']]), digits=c(0,rep(2,4),0))]
        if (parameters[['include.imputations']]) {
            tmp_data[['quintile_group_cuts_and_percentages_status_current']][,QUINTILE_PERCENTAGES_SUBGROUP_IMPUTED_OVERALL_CUTS:=round(100*ecdf(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']])(tmp_data[['quintile_breaks_current_data_no_subset']]), digits=c(0,rep(2,4),0))]
        }
        tmp_data[['quintile_group_cuts_and_percentages_status_prior']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=round(100*ecdf(tmp_data[['prior_data']][['SCALE_SCORE']])(tmp_data[['quintile_breaks_prior_data_no_subset']]), digits=c(0,rep(2,4),0))]
    } else {
        tmp_data[['quintile_group_cuts_and_percentages_status_current']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)]
        if (parameters[['include.imputations']]) {
            tmp_data[['quintile_group_cuts_and_percentages_status_current']][,QUINTILE_PERCENTAGES_SUBGROUP_IMPUTED_OVERALL_CUTS:=c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)]
        }
        tmp_data[['quintile_group_cuts_and_percentages_status_prior']][,QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS:=c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)]
    }

    ### Run spline/linear regressions and produce estimated values from them
    if (!is.na(prior_grade)) {
        tmp_data[['current_data_lm']] <- lm(SCALE_SCORE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['current_data']])
        tmp_data[['current_data_sgp_lm']] <- lm(SGP_BASELINE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['current_data']])
        tmp_data[['current_data_sgp_rq']] <- rq(SGP_BASELINE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['current_data']], method="fn")
        tmp_data[['current_data_lm_fitted_values']] <- predict(tmp_data[['current_data_lm']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['current_data_sgp_lm_fitted_values']] <- predict(tmp_data[['current_data_sgp_lm']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['current_data_sgp_rq_fitted_values']] <- predict(tmp_data[['current_data_sgp_rq']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['prior_data_lm']] <- lm(SCALE_SCORE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['prior_data']])
        tmp_data[['prior_data_sgp_lm']] <- lm(SGP_BASELINE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['prior_data']])
        tmp_data[['prior_data_sgp_rq']] <- rq(SGP_BASELINE ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['prior_data']], method="fn")
        tmp_data[['prior_data_lm_fitted_values']] <- predict(tmp_data[['prior_data_lm']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['prior_data_sgp_lm_fitted_values']] <- predict(tmp_data[['prior_data_sgp_lm']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['prior_data_sgp_rq_fitted_values']] <- predict(tmp_data[['prior_data_sgp_rq']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        tmp_data[['conditional_status_change']] <- tmp_data[['current_data_lm_fitted_values']] - tmp_data[['prior_data_lm_fitted_values']]
        tmp_data[['sgp_change']] <- tmp_data[['current_data_sgp_rq_fitted_values']] - tmp_data[['prior_data_sgp_rq_fitted_values']]
        # "FAIR" TREND       tmp_data[['prior_data_lm_line']] <- lm(SCALE_SCORE ~ ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, data=tmp_data[['prior_data']])
        # "FAIR" TREND       tmp_data[['prior_data_lm_line_fitted_values']] <- predict(tmp_data[['prior_data_lm_line']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
        if (parameters[['include.imputations']]) {
          tmp_data[['current_data_lm_imputed']] <- lm(MEAN_SCALE_SCORE_IMPUTED ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['current_data']])
          tmp_data[['current_data_lm_imputed_fitted_values']] <- predict(tmp_data[['current_data_lm_imputed']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR=tmp_data[['plotting_domain_sequence_growth']]))
          tmp_data[['current_data_sgp_imputed_rq']] <- rq(MEAN_SGP_BASELINE_IMPUTED ~ bs(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR, knots=tmp_data[['knots']], Boundary.knots=tmp_data[['boundaries']]), data=tmp_data[['current_data']], method="fn")
          tmp_data[['current_data_sgp_imputed_rq_fitted_values']] <- predict(tmp_data[['current_data_sgp_imputed_rq']], data.frame(ACADEMIC_IMPACT_SCALE_SCORE_PRIOR = tmp_data[['plotting_domain_sequence_growth']]))
          tmp_data[['conditional_status_change_imputed']] <- tmp_data[['current_data_lm_imputed_fitted_values']] - tmp_data[['prior_data_lm_fitted_values']]
          tmp_data[['sgp_change_imputed']] <- tmp_data[['current_data_sgp_imputed_rq_fitted_values']] - tmp_data[['prior_data_sgp_rq_fitted_values']]
        }
    }

    ### Create PERCENTILE table for status change calculations
    tmp_data[['prior_and_current_percentiles']] <- data.table(
                                                             PERCENTILES=100*seq(0.005, 0.995, length=100),
                                                             QUINTILES=rep(1:5, each=20),
                                                             CURRENT_DATA_CURRENT_SCORE_PERCENTILE=tmp_data[['percentile_breaks_current_data']],
                                                             PRIOR_DATA_CURRENT_SCORE_PERCENTILE=tmp_data[['percentile_breaks_prior_data']]
                                                            )

    if (parameters[['include.imputations']]) {
            tmp_data[['prior_and_current_percentiles']][,CURRENT_DATA_CURRENT_SCORE_IMPUTED_PERCENTILE:=tmp_data[['percentile_breaks_current_data_imputed']]]
    }

    if (!is.null(tmp_data[['current_data_no_subset']])) {
        tmp_data[['prior_and_current_percentiles']][,CURRENT_DATA_CURRENT_SCORE_PERCENTILE_NO_SUBSET:=tmp_data[['percentile_breaks_current_data_no_subset']]]
        tmp_data[['prior_and_current_percentiles']][,PRIOR_DATA_CURRENT_SCORE_PERCENTILE_NO_SUBSET:=tmp_data[['percentile_breaks_prior_data_no_subset']]]
    }

    ## Add in PERCENTILES and QUINTILES based upon Overall Distribution
    if (!is.null(student_group)) {
        subgroup_status_quantiles <- ecdf(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']])(tmp_data[['prior_and_current_percentiles']][['PRIOR_DATA_CURRENT_SCORE_PERCENTILE']])
        subgroup_status_quintiles <- cut(subgroup_status_quantiles, breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=1:5, include.lowest=TRUE)
        tmp_data[['prior_and_current_percentiles']][,PERCENTILES_OVERALL:=round(100*subgroup_status_quantiles, digits=2)]
        tmp_data[['prior_and_current_percentiles']][,QUINTILES_OVERALL:=as.integer(subgroup_status_quintiles)]
    }

    tmp_data[['current_data_percentile_cuts_current_score_subgroup_splinefun']] <- splinefun(tmp_data[['prior_and_current_percentiles']][['PERCENTILES']], tmp_data[['prior_and_current_percentiles']][['CURRENT_DATA_CURRENT_SCORE_PERCENTILE']], method="monoH.FC")
    tmp_data[['prior_data_percentile_cuts_current_score_subgroup_splinefun']] <- splinefun(tmp_data[['prior_and_current_percentiles']][['PERCENTILES']], tmp_data[['prior_and_current_percentiles']][['PRIOR_DATA_CURRENT_SCORE_PERCENTILE']], method="monoH.FC")
    if (parameters[['include.imputations']]) {
        tmp_data[['current_data_percentile_cuts_current_score_imputed_subgroup_splinefun']] <- splinefun(tmp_data[['prior_and_current_percentiles']][['PERCENTILES']], tmp_data[['prior_and_current_percentiles']][['CURRENT_DATA_CURRENT_SCORE_IMPUTED_PERCENTILE']], method="monoH.FC")
    }
    if (!is.null(student_group)) {
        tmp_data[['current_data_percentile_cuts_current_score_overall_splinefun']] <- splinefun(
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['PERCENTILES_OVERALL']],
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['CURRENT_DATA_CURRENT_SCORE_PERCENTILE']],
                                                            method="monoH.FC")
        tmp_data[['prior_data_percentile_cuts_current_score_overall_splinefun']] <- splinefun(
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['PERCENTILES_OVERALL']],
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['PRIOR_DATA_CURRENT_SCORE_PERCENTILE']],
                                                            method="monoH.FC")
        if (parameters[['include.imputations']]) {
            tmp_data[['current_data_percentile_cuts_current_score_imputed_overall_splinefun']] <- splinefun(
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['PERCENTILES_OVERALL']],
                                                            unique(tmp_data[['prior_and_current_percentiles']], by="PERCENTILES_OVERALL")[['CURRENT_DATA_CURRENT_SCORE_IMPUTED_PERCENTILE']],
                                                            method="monoH.FC")
        }
    }

    ### Status PERCENTILE and QUINTILE change calculations
    tmp_data[['status_percentile_change_subgroup']] <- tmp_data[['current_data_percentile_cuts_current_score_subgroup_splinefun']](seq(3, 97)) - tmp_data[['prior_data_percentile_cuts_current_score_subgroup_splinefun']](seq(3, 97))
    if (parameters[['include.imputations']]){
        tmp_data[['status_percentile_change_imputed_subgroup']] <- tmp_data[['current_data_percentile_cuts_current_score_imputed_subgroup_splinefun']](seq(3, 97)) - tmp_data[['prior_data_percentile_cuts_current_score_subgroup_splinefun']](seq(3, 97))
    }
    if (!is.null(student_group)) {
        tmp_data[['status_percentile_change_overall']] <- tmp_data[['current_data_percentile_cuts_current_score_overall_splinefun']](seq(3, 97)) - tmp_data[['prior_data_percentile_cuts_current_score_overall_splinefun']](seq(3, 97))
        if (parameters[['include.imputations']]) {
            tmp_data[['status_percentile_change_imputed_overall']] <- tmp_data[['current_data_percentile_cuts_current_score_imputed_overall_splinefun']](seq(3, 97)) - tmp_data[['prior_data_percentile_cuts_current_score_overall_splinefun']](seq(3, 97))
        }
    }


    tmp_data[['status_quintile_change_subgroup']] <- (tmp_data[['prior_and_current_percentiles']][,list(QUINTILE_CHANGE=mean(CURRENT_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE)), keyby="QUINTILES"][.(QUINTILES=1:5), on="QUINTILES"] -
                                                        tmp_data[['prior_and_current_percentiles']][,mean(PRIOR_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE), keyby="QUINTILES"][.(QUINTILES=1:5), on="QUINTILES"])[,QUINTILES:=1:5]
    if (parameters[['include.imputations']]) {
        tmp_data[['status_quintile_change_imputed_subgroup']] <- (tmp_data[['prior_and_current_percentiles']][,list(QUINTILE_CHANGE=mean(CURRENT_DATA_CURRENT_SCORE_IMPUTED_PERCENTILE, na.rm=TRUE)), keyby="QUINTILES"][.(QUINTILES=1:5), on="QUINTILES"] -
                                                                    tmp_data[['prior_and_current_percentiles']][,mean(PRIOR_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE), keyby="QUINTILES"][.(QUINTILES=1:5), on="QUINTILES"])[,QUINTILES:=1:5]
    }
    if (!is.null(student_group)) {
        tmp_data[['status_quintile_change_overall']] <- (tmp_data[['prior_and_current_percentiles']][,list(QUINTILE_CHANGE=mean(CURRENT_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE)), keyby="QUINTILES_OVERALL"][.(QUINTILES_OVERALL=1:5), on="QUINTILES_OVERALL"] -
                                                            tmp_data[['prior_and_current_percentiles']][,mean(PRIOR_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE), keyby="QUINTILES_OVERALL"][.(QUINTILES_OVERALL=1:5), on="QUINTILES_OVERALL"])[,QUINTILES_OVERALL:=1:5]
        if (parameters[['include.imputations']]) {
            tmp_data[['status_quintile_change_imputed_overall']] <- (tmp_data[['prior_and_current_percentiles']][,list(QUINTILE_CHANGE=mean(CURRENT_DATA_CURRENT_SCORE_IMPUTED_PERCENTILE, na.rm=TRUE)), keyby="QUINTILES_OVERALL"][.(QUINTILES_OVERALL=1:5), on="QUINTILES_OVERALL"] -
                                                                        tmp_data[['prior_and_current_percentiles']][,mean(PRIOR_DATA_CURRENT_SCORE_PERCENTILE, na.rm=TRUE), keyby="QUINTILES_OVERALL"][.(QUINTILES_OVERALL=1:5), on="QUINTILES_OVERALL"])[,QUINTILES_OVERALL:=1:5]
        }
    }

    if (!is.null(tmp_data[['prior_data_no_subset']])) {
        tmp_data[['status_percentile_change_standardized_subgroup']] <- tmp_data[['status_percentile_change_subgroup']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        if (!is.null(student_group)) tmp_data[['status_percentile_change_standardized_overall']] <- tmp_data[['status_percentile_change_overall']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        tmp_data[['prior_data_current_score_ecdf']] <- ecdf(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']])
    } else {
        tmp_data[['status_percentile_change_standardized_subgroup']] <- tmp_data[['status_percentile_change_subgroup']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        if (!is.null(student_group)) tmp_data[['status_percentile_change_standardized_overall']] <- tmp_data[['status_percentile_change_overall']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        tmp_data[['prior_data_current_score_ecdf']] <- ecdf(tmp_data[['prior_data']][['SCALE_SCORE']])
    }

    tmp_unique_percentiles <- unique(tmp_data[['prior_and_current_percentiles']], by="PRIOR_DATA_CURRENT_SCORE_PERCENTILE")
    tmp_unique_percentiles_names <- head(unique(tmp_unique_percentiles[['PERCENTILES']])+0.5, -1)
    tmp_data[['prior_data']][,SCALE_SCORE_PERCENTILES:=cut(SCALE_SCORE, breaks=tmp_unique_percentiles[['PRIOR_DATA_CURRENT_SCORE_PERCENTILE']], labels=tmp_unique_percentiles_names, include.lowest=TRUE)]
    tmp_data[['current_data']][,SCALE_SCORE_PERCENTILES:=cut(SCALE_SCORE, breaks=tmp_unique_percentiles[['PRIOR_DATA_CURRENT_SCORE_PERCENTILE']], labels=tmp_unique_percentiles_names, include.lowest=TRUE)]

    ### Clean Up
    long_data[, ACADEMIC_IMPACT_SCALE_SCORE_PRIOR := NULL]

    ### Return data object
    return(tmp_data)
} ### END getData

mergeImputedData <- function(original_data, imputed_data, parameters) {
    ### Checks on parameters
    if (!is.null(parameters[['prior_score_variable']])) {
      prior_score_variable <- parameters[['prior_score_variable']]
    } else prior_score_variable <- "SCALE_SCORE_PRIOR_2YEAR"

    imputation_data <- imputed_data[GRADE %in% parameters[['current_grade']] & CONTENT_AREA %in% parameters[['content_area']]]

    ### Create LONG data file based upon 30 imputations
    meas.list <- vector(mode = "list", length = 2)
    meas.list[["SCALE_SCORE_IMPUTED"]] <- grep("SCALE_SCORE_IMPUTED", names(imputation_data), value = TRUE)
    meas.list[["SGP_BASELINE_IMPUTED"]] <- grep("SGP_BASELINE_IMPUTED", names(imputation_data), value = TRUE)

    id.vars <- c("ID", "CONTENT_AREA", "GRADE", prior_score_variable) # , "SCALE_SCORE_OBSERVED", "SGP_BASELINE_OBSERVED")
    tmp.vars <- c(id.vars, meas.list[["SCALE_SCORE_IMPUTED"]], meas.list[["SGP_BASELINE_IMPUTED"]])
    tmp_wide <- imputation_data[, ..tmp.vars]
    Imputed_Data_LONG <- melt(tmp_wide, id = id.vars, variable.name = "IMP", measure=meas.list[lengths(meas.list) != 0])
    Imputed_Data_LONG[, VALID_CASE := "VALID_CASE"][, YEAR := parameters[['current_year']]]

    # setnames(Imputed_Data_LONG, c("SCALE_SCORE_OBSERVED", "SGP_BASELINE_OBSERVED"), c("SCALE_SCORE", "SGP_BASELINE"))

    Imputed_Data_LONG[, MEAN_SCALE_SCORE_IMPUTED := mean(SCALE_SCORE_IMPUTED), keyby = c("ID", "CONTENT_AREA", "GRADE")]
    Imputed_Data_LONG[, MEAN_SGP_BASELINE_IMPUTED := mean(SGP_BASELINE_IMPUTED), keyby = c("ID", "CONTENT_AREA", "GRADE")]
    Imputed_Data_AVG <- Imputed_Data_LONG[IMP == "1"] # "SCALE_SCORE_IMPUTED_1"]
    Imputed_Data_AVG[,MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED:=as.data.table(scale(MEAN_SCALE_SCORE_IMPUTED))[['V1']]]
    Imputed_Data_AVG[, c("IMP", "SCALE_SCORE_IMPUTED", "SGP_BASELINE_IMPUTED", prior_score_variable) := NULL]

    setkeyv(Report_Data[[parameters[['assessment_type']]]], c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE"))
    setkeyv(Imputed_Data_AVG, c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE"))

    tmp_data <- Imputed_Data_AVG[Report_Data[[parameters[['assessment_type']]]]] # merge in MEAN_SCALE_SCORE_IMPUTED, MEAN_SGP_BASELINE_IMPUTED
    return(tmp_data)
} ### END mergeImputedData

getAcademicImpactInfo <- function(academic_impact_value, growth_or_status="GROWTH", academic_impact_type=NULL, small_n=FALSE, Quintile_Group_Percentages=NA) {
    impactColorGradient <- colorRampPalette(
                                c(
                                    rgb(0.89803922, 0.04843137, 0.89921569), ### Magenta for -100
                                    rgb(0.53333333, 0.03137255, 0.03137255), ### Blood Red for Severe/Large Cut -25
                                    rgb(0.800000, 0.254902, 0.000000), ### Orange for Large/Moderate Cut -15
                                    rgb(0.8000000, 0.5843137, 0.0000000), ### Orange-Yellow for Moderate/Modest Cut -5
                                    rgb(0.5392157, 0.7598039, 0.1960784), ### Green-Yellow for Modest/Improvement Cut 5
                                    rgb(0.4596078, 0.6596078, 0.8596078) ### Blue for Improvement 100
                                )
                            )

    if (is.na(academic_impact_value)) {
        return(list(Label="NO DATA", cex=NA, Label_Color=NA, Academic_Impact_Value=NA, Color=NA, Quintile_Group_Percentages=NA))
    }

    if (is.na(small_n) || small_n==TRUE) {
        return(list(Label="Small N", cex=0.65, Label_Color="black", Academic_Impact_Value=as.numeric(NA), Color=rgb(1.0, 1.0, 1.0), Quintile_Group_Percentages=Quintile_Group_Percentages))
    } else {
        tmp_colors <- impactColorGradient(100)
        if (growth_or_status=="GROWTH") {
            transformed_academic_impact_value_fun <- approxfun(x=c(-100, -40, -25, -15, -5, 5, 20, 100), y=c(1, 1, 20, 40, 60, 80, 100, 100))
            if (is.na(academic_impact_value)) {
                return(list(Label="No Cases", cex=0.55, Label_Color="black", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=rgb(1, 1, 1), Quintile_Group_Percentages=Quintile_Group_Percentages))
            } else {
                if (academic_impact_value <= -25) return(list(Label="Severe", cex=0.55, Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -25 & academic_impact_value <= -15) return(list(Label="Large", cex=0.55, Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -15 & academic_impact_value <= -5) return(list(Label="Moderate", cex=0.55, Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -5 & academic_impact_value <= 5) return(list(Label="Modest to None", cex=0.45, Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > 5) return(list(Label="Improvement", cex=0.55, Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
            }
        }
        if (growth_or_status=="STATUS") {
            transformed_academic_impact_value_fun <- approxfun(x=c(-2.0, -0.55, -0.4, -0.25, -0.1, 0.05, 0.2, 2.0), y=c(1, 1, 20, 40, 60, 80, 100, 100))
            if (is.na(academic_impact_value)) {
                return(list(Label="No Cases", cex=0.55,  Label_Color="black", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=rgb(1, 1, 1), Quintile_Group_Percentages=Quintile_Group_Percentages))
            } else {
                if (academic_impact_value <= -0.4) return(list(Label="Severe", cex=0.55,  Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -0.4 & academic_impact_value <= -0.25) return(list(Label="Large", cex=0.55,  Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -0.25 & academic_impact_value <= -0.1) return(list(Label="Moderate", cex=0.55,  Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > -0.1 & academic_impact_value <= 0.05) return(list(Label="Modest to None", cex=0.45,  Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
                if (academic_impact_value > 0.05) return(list(Label="Improvement", cex=0.55,  Label_Color="white", Academic_Impact_Value=academic_impact_value, Academic_Impact_Type=academic_impact_type, Color=tmp_colors[round(transformed_academic_impact_value_fun(academic_impact_value))], Quintile_Group_Percentages=Quintile_Group_Percentages))
            }
        }
    }
} ### END getAcademicImpactInfo

addStatusGrowthSummariestoOveriew <- function(tmp_data, assessment, content_area.iter, grade.iter) {
    load("../Data/Academic_Impact_Overview.Rdata")
    tmp.list <- list()
    tmp.list[['current_data_sgp_lm_fitted_values']] <- tmp_data[['current_data_sgp_lm_fitted_values']]
    tmp.list[['prior_data_sgp_lm_fitted_values']] <- tmp_data[['prior_data_sgp_lm_fitted_values']]
    tmp.list[['current_data_sgp_rq_fitted_values']] <- tmp_data[['current_data_sgp_rq_fitted_values']]
    tmp.list[['prior_data_sgp_rq_fitted_values']] <- tmp_data[['prior_data_sgp_rq_fitted_values']]
    tmp.list[['status_percentile_change']] <- tmp_data[['status_percentile_change']]
    tmp.list[['status_percentile_change_standardized']] <- tmp_data[['status_percentile_change_standardized']]

    Academic_Impact_Overview[[assessment]][['ALL_STUDENTS']][['Growth_Status_Overview']][[content_area.iter]][[parameters[['current_grade']][grade.iter]]] <- tmp.list
    save(Academic_Impact_Overview, file="../Data/Academic_Impact_Overview.Rdata")
} ### END addStatusGrowthSummariestoOveriew

strtail <- function(s, n=1){
    if (n < 0)
        substring(s, 1 - n)
    else substring(s, nchar(s) - n + 1)
}


getDistrictIDs <- function(
    data,
    var.name = "DISTRICT_NUMBER",
    min.size = 250,
    smallest.grade.size=50,
    ids.to.exclude = NA,
    parameters
  ) {
    Base_Counts_CURRENT <- data[YEAR == parameters[["current_year"]] & !get(var.name) %in% ids.to.exclude, .(
                                      COUNT_STATUS = sum(!is.na(SCALE_SCORE))),
                                    keyby = c("CONTENT_AREA", "GRADE", var.name)][COUNT_STATUS >= smallest.grade.size][,MEDIAN_COHORT_COUNT:=as.numeric(median(COUNT_STATUS)), by = var.name][MEDIAN_COHORT_COUNT >= min.size]

    Base_Counts_PRIOR <- data[YEAR == parameters[["prior_year"]] & !get(var.name) %in% ids.to.exclude, .(
                                      COUNT_STATUS = sum(!is.na(SCALE_SCORE))),
                                    keyby = c("CONTENT_AREA", "GRADE", var.name)][COUNT_STATUS >= smallest.grade.size][,MEDIAN_COHORT_COUNT:=as.numeric(median(COUNT_STATUS)), by = var.name][MEDIAN_COHORT_COUNT >= min.size]

    District_Subset <- merge(Base_Counts_PRIOR[,1:3], Base_Counts_CURRENT[,1:3])[,eval(var.name):=as.character(get(var.name))]
    setkeyv(District_Subset, c(var.name, "CONTENT_AREA", "GRADE"))

    return(District_Subset)
}

filterAcademicImpactOverview <- function(Academic_Impact_Overview,
                                            assessment_type,
                                            status_grades,
                                            growth_grades,
                                            academic_impact_metric="Hybrid",
                                            use_imputations,
                                            academic_impact_groups) {

    ### Utility function
    getGrowthGrades <- function(growth_grades, content_area) {
        if (is.list(growth_grades)) {
                return(growth_grades[[content_area]])
        } else {
                return(growth_grades)
        }
    }

    Academic_Impact_Overview_REDUCED <- Academic_Impact_Overview

    ### FILTER/REMOVE groups not in academic_impact_groups
    Academic_Impact_Overview_REDUCED[[assessment_type]][setdiff(names(Academic_Impact_Overview[[assessment_type]]), academic_impact_groups)] <- NULL

    ### REARRANGE meta-data based upon type and use_imputations arguments
    for (academic_impact_group_iter in academic_impact_groups) {
        content_areas <- names(Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][['Overall']])
        for (content_area_iter in content_areas) {
            grades <- names(Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][['Overall']][[content_area_iter]])
            for (grade_iter in grades) {
                for (achievement_group in c("Overall", "Q1", "Q2", "Q3", "Q4", "Q5")) {
                    if (academic_impact_metric=="Hybrid") {
                        growth_or_status_grade <- ifelse(grade_iter %in% getGrowthGrades(growth_grades, content_area_iter), "Growth", "Status")
                        if (use_imputations) {
                            Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]] <-
                                Academic_Impact_Overview[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]][[growth_or_status_grade]][['Imputed']]
                        } else {
                            Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]] <-
                                Academic_Impact_Overview[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]][[growth_or_status_grade]][['Observed']]
                        }
                    } ### END Hybrid

                    if (academic_impact_metric=="Status") {
                        if (use_imputations) {
                            Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]] <-
                                Academic_Impact_Overview[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]][['Status']][['Imputed']]
                        } else {
                            Academic_Impact_Overview_REDUCED[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]] <-
                                Academic_Impact_Overview[[assessment_type]][[academic_impact_group_iter]][[achievement_group]][[content_area_iter]][[grade_iter]][['Status']][['Observed']]
                        }
                    } ### END Status
                } ## END achievement_group loop
            } ## END grade loop
        } ## END content_area loop
    } ## END academic_impact_group loop

    return(Academic_Impact_Overview_REDUCED)
} ### END filterAcademicImpactOverview

getMetaData <- function(tmp_meta_data, Quintile_Cuts=NULL) {
    meta_data_list <- list()
    meta_data_list[['CONTENT_AREA']] <- rep(names(tmp_meta_data), sapply(tmp_meta_data, length))
    meta_data_list[['GRADE']] <- as.character(unlist(sapply(tmp_meta_data, names)))
    meta_data_list[['Color']] <- as.character(unlist(tmp_meta_data)[setdiff(grep(paste(c(Quintile_Cuts, "Color"), collapse="."), names(unlist(tmp_meta_data))), grep(paste(c(Quintile_Cuts, "Label_Color"), collapse="."), names(unlist(tmp_meta_data))))])
    meta_data_list[['Label']] <- as.character(unlist(tmp_meta_data)[setdiff(grep(paste(c(Quintile_Cuts, "Label"), collapse="."), names(unlist(tmp_meta_data))), grep(paste(c(Quintile_Cuts, "Label_Color"), collapse="."), names(unlist(tmp_meta_data))))])
    meta_data_list[['cex']] <- as.numeric(unlist(tmp_meta_data)[grep(paste(c(Quintile_Cuts, "cex"), collapse="."), names(unlist(tmp_meta_data)))])
    meta_data_list[['Label_Color']] <- as.character(unlist(tmp_meta_data)[grep(paste(c(Quintile_Cuts, "Label_Color"), collapse="."), names(unlist(tmp_meta_data)))])
    if (is.null(Quintile_Cuts)){
        meta_data_list[['Quintile_Group_Percentages']] <- rep(NA, length(meta_data_list[['CONTENT_AREA']]))
    } else {
        meta_data_list[['Quintile_Group_Percentages']] <- unlist(unlist(unlist(tmp_meta_data, recursive=FALSE), recursive=FALSE), recursive=FALSE)[grep(paste(Quintile_Cuts, "Quintile_Group_Percentages", sep="."), names(unlist(unlist(unlist(tmp_meta_data, recursive=FALSE), recursive=FALSE), recursive=FALSE)))]
    }
    meta_data_dt <- as.data.table(meta_data_list)
    setkey(meta_data_dt, CONTENT_AREA, GRADE)
    return(meta_data_dt)
} ### END getMetaData

getQuintileBoxInfo <- function(meta_data) {
    total_rectangle_width <- 0.94; box_sep_constant <- rep(0.01, 4)
    quintile_group_percentages_diff <- lapply(meta_data[['Quintile_Group_Percentages']], function(x) diff(x)/diff(x)[1])
    box_widths <- lapply(quintile_group_percentages_diff, function(x) x*(total_rectangle_width - 0.01*(length(x[x!=0])-1))/sum(x))
    box_seps <- lapply(box_widths, function(x) box_sep_constant*as.numeric((abs(head(x, -1)) - abs(diff(x)))!=0))
    meta_data[,BOX_WIDTH_Q1:=unlist(lapply(box_widths, '[', 1))]
    meta_data[,BOX_WIDTH_Q2:=unlist(lapply(box_widths, '[', 2))]
    meta_data[,BOX_WIDTH_Q3:=unlist(lapply(box_widths, '[', 3))]
    meta_data[,BOX_WIDTH_Q4:=unlist(lapply(box_widths, '[', 4))]
    meta_data[,BOX_WIDTH_Q5:=unlist(lapply(box_widths, '[', 5))]
    meta_data[,X_COOR_Q1:=X_COOR - total_rectangle_width/2 + BOX_WIDTH_Q1/2]
    meta_data[,X_COOR_Q2:=X_COOR - total_rectangle_width/2 + BOX_WIDTH_Q1 + unlist(lapply(box_seps, '[', 1)) + BOX_WIDTH_Q2/2]
    meta_data[,X_COOR_Q3:=X_COOR - total_rectangle_width/2 + BOX_WIDTH_Q1 + unlist(lapply(box_seps, '[', 1)) + BOX_WIDTH_Q2 + unlist(lapply(box_seps, '[', 2)) + BOX_WIDTH_Q3/2]
    meta_data[,X_COOR_Q4:=X_COOR - total_rectangle_width/2 + BOX_WIDTH_Q1 + unlist(lapply(box_seps, '[', 1)) + BOX_WIDTH_Q2 + unlist(lapply(box_seps, '[', 2)) + BOX_WIDTH_Q3 + unlist(lapply(box_seps, '[', 3)) + BOX_WIDTH_Q4/2]
    meta_data[,X_COOR_Q5:=X_COOR - total_rectangle_width/2 + BOX_WIDTH_Q1 + unlist(lapply(box_seps, '[', 1)) + BOX_WIDTH_Q2 + unlist(lapply(box_seps, '[', 2)) + BOX_WIDTH_Q3 + unlist(lapply(box_seps, '[', 3)) + BOX_WIDTH_Q4 + unlist(lapply(box_seps, '[', 4)) + BOX_WIDTH_Q5/2]

    return(meta_data)
} ### END getQuintileBoxInfo

getAcademicImpactValues <- function(
                            tmp_data=tmp_data,
                            parameters=parameters,
                            student_group=student_group,
                            prior_grade=prior_grade) {

        academicImpactValues <- list()

        ###############################
        #### Status Impact Overall ####
        ###############################
        if (is.null(student_group)) {
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
              tmp_z_score_imputed_current_data <- (mean(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            }
            tmp_z_score_prior_data <- 0
        } else {
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
              tmp_z_score_imputed_current_data <- (mean(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            }
            tmp_z_score_prior_data <- (mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        academicImpactValues[['overall_academic_impact_status']] <- tmp_z_score_current_data - tmp_z_score_prior_data
        academicImpactValues[['overall_academic_impact_N_status']] <- min(sum(!is.na(tmp_data[['current_data']][['SCALE_SCORE']])), sum(!is.na(tmp_data[['prior_data']][['SCALE_SCORE']])), na.rm=TRUE)

        if (parameters[['include.imputations']]) {
            academicImpactValues[['overall_academic_impact_imputed_status']] <- tmp_z_score_imputed_current_data - tmp_z_score_prior_data
            academicImpactValues[['overall_academic_impact_imputed_N_status']] <- min(sum(!is.na(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']])), sum(!is.na(tmp_data[['prior_data']][['SCALE_SCORE']])), na.rm=TRUE)
        } else {
            academicImpactValues[['overall_academic_impact_imputed_status']] <- academicImpactValues[['overall_academic_impact_imputed_N_status']] <- NA
        }
        academicImpactValues[['overall_quintile_group_percentages_status']] <- c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)

        #################################################################################################
        #### Status Impact Quintile (Overall Cuts, NOT necessarily 20, 20, 20, 20, 20 in each group) ####
        #################################################################################################
        if (is.null(student_group)) {
            tmp_z_score_quintile_change <- tmp_data[['status_quintile_change_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                tmp_z_score_imputed_quintile_change <- tmp_data[['status_quintile_change_imputed_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            }
        } else {
            tmp_z_score_quintile_change <- tmp_data[['status_quintile_change_overall']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                tmp_z_score_imputed_quintile_change <- tmp_data[['status_quintile_change_imputed_overall']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            }
        }

        academicImpactValues[['quintile_academic_impact_status_overall_cuts']] <- tmp_z_score_quintile_change
        academicImpactValues[['quintile_academic_impact_N_status_overall_cuts']] <- pmin(tmp_data[['prior_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], tmp_data[['current_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']])
        if (parameters[['include.imputations']]) {
            academicImpactValues[['quintile_academic_impact_imputed_status_overall_cuts']] <- tmp_z_score_imputed_quintile_change
            academicImpactValues[['quintile_academic_impact_imputed_N_status_overall_cuts']] <- pmin(tmp_data[['prior_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], tmp_data[['current_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']])
        } else {
            academicImpactValues[['quintile_academic_impact_imputed_status_overall_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_status_overall_cuts']] <- rep(NA, 5)
        }
        academicImpactValues[['quintile_group_percentages_status_overall_cuts']] <- tmp_data[['quintile_group_cuts_and_percentages_status_prior']][['QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS']]

        ################################################
        #### Status Impact Quintile (Subgroup Cuts) ####
        ################################################
        if (is.null(student_group)) {
            tmp_z_score_quintile_change <- tmp_data[['status_quintile_change_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                tmp_z_score_imputed_quintile_change <- tmp_data[['status_quintile_change_imputed_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            }
        } else {
            tmp_z_score_quintile_change <- tmp_data[['status_quintile_change_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                tmp_z_score_imputed_quintile_change <- tmp_data[['status_quintile_change_imputed_subgroup']][['QUINTILE_CHANGE']]/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            }
        }

        academicImpactValues[['quintile_academic_impact_status_subgroup_cuts']] <- tmp_z_score_quintile_change
        academicImpactValues[['quintile_academic_impact_N_status_subgroup_cuts']] <- pmin(tmp_data[['prior_data_summaries_current_quintiles_subgroup']][['COUNT_STATUS_OBSERVED']], tmp_data[['current_data_summaries_current_quintiles_subgroup']][['COUNT_STATUS_OBSERVED']])
        if (parameters[['include.imputations']]) {
            academicImpactValues[['quintile_academic_impact_imputed_status_subgroup_cuts']] <- tmp_z_score_imputed_quintile_change
            academicImpactValues[['quintile_academic_impact_imputed_N_status_subgroup_cuts']] <- pmin(tmp_data[['prior_data_summaries_current_quintiles_subgroup']][['COUNT_STATUS_OBSERVED']], tmp_data[['current_data_summaries_current_quintiles_subgroup']][['COUNT_STATUS_OBSERVED']])
        } else {
            academicImpactValues[['quintile_academic_impact_imputed_status_subgroup_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_status_subgroup_cuts']] <- rep(NA, 5)
        }
        academicImpactValues[['quintile_group_percentages_status_subgroup_cuts']] <- tmp_data[['quintile_group_cuts_and_percentages_status_prior']][['QUINTILE_PERCENTAGES_SUBGROUP_SUBGROUP_CUTS']]

        ###############################
        #### Growth Impact Overall ####
        ###############################
        if (!is.na(prior_grade)) {
            academicImpactValues[['overall_academic_impact_growth']] <- median(tmp_data[['current_data']][['SGP_BASELINE']], na.rm=TRUE) - median(tmp_data[['prior_data']][['SGP_BASELINE']], na.rm=TRUE)
            academicImpactValues[['overall_academic_impact_N_growth']] <- min(sum(!is.na(tmp_data[['current_data']][['SGP_BASELINE']])), sum(!is.na(tmp_data[['prior_data']][['SGP_BASELINE']])), na.rm=TRUE)

            if (parameters[['include.imputations']]) {
                academicImpactValues[['overall_academic_impact_imputed_growth']] <- median(tmp_data[['current_data']][['MEAN_SGP_BASELINE_IMPUTED']], na.rm=TRUE) - median(tmp_data[['prior_data']][['SGP_BASELINE']], na.rm=TRUE)
                academicImpactValues[['overall_academic_impact_imputed_N_growth']] <- min(sum(!is.na(tmp_data[['current_data']][['MEAN_SGP_BASELINE_IMPUTED']])), sum(!is.na(tmp_data[['prior_data']][['SGP_BASELINE']])), na.rm=TRUE)
            } else {
                academicImpactValues[['overall_academic_impact_imputed_growth']] <- academicImpactValues[['overall_academic_impact_imputed_N_growth']] <- NA
            }
        } else {
            academicImpactValues[['overall_academic_impact_growth']] <- academicImpactValues[['overall_academic_impact_N_growth']] <-
                academicImpactValues[['overall_academic_impact_imputed_growth']] <- academicImpactValues[['overall_academic_impact_imputed_N_growth']] <- NA
        }
        academicImpactValues[['overall_quintile_group_percentages_growth']] <- c(0.0, 20.0, 40.0, 60.0, 80.0, 100.0)

        ###############################################
        #### Growth Impact Quintile (Overall Cuts) ####
        ###############################################
        if (!is.na(prior_grade)) {
            academicImpactValues[['quintile_academic_impact_growth_overall_cuts']] <- tmp_data[['current_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']] - tmp_data[['prior_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']]
            academicImpactValues[['quintile_academic_impact_N_growth_overall_cuts']] <- pmin(tmp_data[['current_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], tmp_data[['prior_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                academicImpactValues[['quintile_academic_impact_imputed_growth_overall_cuts']] <- tmp_data[['current_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE_IMPUTED']] - tmp_data[['prior_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']]
                academicImpactValues[['quintile_academic_impact_imputed_N_growth_overall_cuts']] <- pmin(tmp_data[['current_data_summaries_quintiles']][['COUNT_GROWTH_IMPUTED']], tmp_data[['prior_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE)
            } else {
                academicImpactValues[['quintile_academic_impact_imputed_growth_overall_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_growth_overall_cuts']] <- rep(NA, 5)
            }
        } else {
            academicImpactValues[['quintile_academic_impact_growth_overall_cuts']] <- academicImpactValues[['quintile_academic_impact_N_growth_overall_cuts']] <-
                academicImpactValues[['quintile_academic_impact_imputed_growth_overall_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_growth_overall_cuts']] <- rep(NA, 5)
        }
        academicImpactValues[['quintile_group_percentages_growth_overall_cuts']] <- tmp_data[['quintile_group_cuts_and_percentages_growth']][['QUINTILE_PERCENTAGES_SUBGROUP_OVERALL_CUTS']]

        ################################################
        #### Growth Impact Quintile (Subgroup Cuts) ####
        ################################################
        if (!is.na(prior_grade)) {
            academicImpactValues[['quintile_academic_impact_growth_subgroup_cuts']] <- tmp_data[['current_data_summaries_quintiles_subgroup_cuts']][['MEDIAN_SGP_BASELINE']] - tmp_data[['prior_data_summaries_quintiles_subgroup_cuts']][['MEDIAN_SGP_BASELINE']]
            academicImpactValues[['quintile_academic_impact_N_growth_subgroup_cuts']] <- pmin(tmp_data[['current_data_summaries_quintiles_subgroup_cuts']][['COUNT_GROWTH_OBSERVED']], tmp_data[['prior_data_summaries_quintiles_subgroup_cuts']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE)
            if (parameters[['include.imputations']]) {
                academicImpactValues[['quintile_academic_impact_imputed_growth_subgroup_cuts']] <- tmp_data[['current_data_summaries_quintiles_subgroup_cuts']][['MEDIAN_SGP_BASELINE_IMPUTED']] - tmp_data[['prior_data_summaries_quintiles_subgroup_cuts']][['MEDIAN_SGP_BASELINE']]
                academicImpactValues[['quintile_academic_impact_imputed_N_growth_subgroup_cuts']] <- pmin(tmp_data[['current_data_summaries_quintiles_subgroup_cuts']][['COUNT_GROWTH_IMPUTED']], tmp_data[['prior_data_summaries_quintiles_subgroup_cuts']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE)
            } else {
                academicImpactValues[['quintile_academic_impact_imputed_growth_subgroup_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_growth_subgroup_cuts']] <- rep(NA, 5)
            }
        } else {
            academicImpactValues[['quintile_academic_impact_growth_subgroup_cuts']] <- academicImpactValues[['quintile_academic_impact_N_growth_subgroup_cuts']] <-
                academicImpactValues[['quintile_academic_impact_imputed_growth_subgroup_cuts']] <- academicImpactValues[['quintile_academic_impact_imputed_N_growth_subgroup_cuts']] <- rep(NA, 5)
        }
        academicImpactValues[['quintile_group_percentages_growth_subgroup_cuts']] <- tmp_data[['quintile_group_cuts_and_percentages_growth']][['QUINTILE_PERCENTAGES_SUBGROUP_SUBGROUP_CUTS']]

        #### Return data
        return(academicImpactValues)
} ### END getAcademicImpactValues

combinePDF <- function(
  plots_path = "assets/Rplots/Impact/State_Assessment/CONDITIONAL_STATUS/CONTENT_AREA_by_GRADE",
  output_name = "Academic_Impact_CATALOG.pdf", # should be NULL if using content_area and/or grades
  output_dir = NULL,
  content_area = NULL,
  grades = NULL
  ) {
    if (is.null(content_area) & is.null(grades) & length(plots_path)==1L) {
      all.plots <- grep("[.]pdf", list.files(plots_path, full.names = TRUE), value = TRUE)
      all.plots <- all.plots[!grepl("CATALOGUE|CATALOG", toupper(basename(all.plots)))] # filter out any existing CATALOG files # grep("CATALOGUE|CATALOG", all.plots, invert = TRUE, value = TRUE)
      if (any(grepl("Grade_[10-12]", basename(all.plots)))) {
        tdt <- data.table(CA = gsub(".*?Academic_Impact_([[:alpha:]]+)_Grade.*", "\\1", basename(all.plots)),
                           G = as.numeric(gsub(".*?_([[:digit:]]+).*", "\\1", basename(all.plots))),
                          ORD = 1:length(all.plots), key=c("CA", "G"))
        all.plots <- all.plots[tdt$ORD]
      }
      if (is.null(output_dir)) output_dir <- plots_path
      qpdf::pdf_combine(input = all.plots, output = file.path(output_dir, output_name))
    }

    if (is.null(content_area) & is.null(grades) & length(plots_path)>1L) {
      #  get plots in order of `plots_path`
      all.plots <- grep("[.]pdf", unlist(lapply(plots_path, function(f) { #list.files(f, full.names = TRUE)), use.names = FALSE), value = TRUE)
          tfiles <- list.files(f, full.names = TRUE)
          tfiles <- tfiles[!grepl("CATALOGUE|CATALOG", toupper(basename(tfiles)))]
          if (any(grepl("Grade_[10-12]", tfiles))) {
            tdt <- data.table(CA = gsub(".*?Academic_Impact_([[:alpha:]]+)_Grade.*", "\\1", basename(tfiles)),
                               G = as.numeric(gsub(".*?_([[:digit:]]+).*", "\\1", basename(tfiles))),
                              ORD = 1:length(tfiles), key=c("CA", "G"))
            tfiles <- tfiles[tdt$ORD]
          }
          tfiles
        }), use.names = FALSE), value = TRUE)

      all.plots <- all.plots[!grepl("CATALOGUE|CATALOG", toupper(basename(all.plots)))] # filter out any existing CATALOG files
      if (is.null(output_dir)) {
        fpaths <- strsplit(plots_path, .Platform$file.sep)
        paircomps <- combn(length(fpaths), 2, simplify=FALSE)
        path.nest <- sapply(paircomps, function(k) match(fpaths[[k[1]]], fpaths[[k[2]]]), simplify = "array")
        for(idx in seq(nrow(path.nest))) {
          if(!any(is.na(path.nest[idx,]))) next else {idx <- idx - 1L; break}
        }
        output_dir <- paste(fpaths[[1]][1:idx], collapse=.Platform$file.sep)
      }
      qpdf::pdf_combine(input = all.plots, output = file.path(output_dir, output_name))
    }

    if (!is.null(content_area) & is.null(grades) & length(plots_path)==1L) {
      all.plots <- gsub(".pdf", "", grep("[.]pdf", list.files(plots_path), value = TRUE))
      for (ca in content_area) {
        subj.plots <- grep(capwords(ca), all.plots, value = TRUE)
        subj.plots <- file.path(plots_path, paste0(subj.plots, ".pdf"))
        tmp_output_dir <- ifelse(is.null(output_dir), plots_path, output_dir)
        qpdf::pdf_combine(input = subj.plots, output = file.path(tmp_output_dir, paste0("Academic_Impact_", capwords(ca), "_CATALOG.pdf")))
      }
    }

    if (!is.null(content_area) & !is.null(grades) & length(plots_path)>1L) {
      #  get plots in order of `plots_path`
      all.plots <- gsub(".pdf", "", grep("[.]pdf", unlist(lapply(plots_path, function(f) list.files(f, full.names = TRUE)), use.names = FALSE), value = TRUE))
      all.plots <- grep("CATALOGUE|CATALOG", all.plots, invert = TRUE, value = TRUE) # filter out any existing CATALOG files
      for (ca in content_area) {
        for(grd in grades) {
          tmp.plots <- grep(capwords(ca), all.plots, value = TRUE)
          tmp.plots <- sort(grep(grd, tmp.plots, value = TRUE))
          tmp.plots <- paste0(tmp.plots, ".pdf")
          tmp_output_dir <- ifelse(is.null(output_dir), plots_path, output_dir)
          qpdf::pdf_combine(input = tmp.plots, output = file.path(tmp_output_dir, paste0("Academic_Impact_", capwords(ca), "_Grade_", grd, "_CATALOG.pdf")))
        }
      }
    }
  }  ###  END combinePDF

makeCatalog <- function(
  base_path = parameters[['graphic_format']][['file.path']],
  included_groups = if(exists("student_groups")) student_groups$DIRECTORY_LABEL else NA
  ) {
    if (any(!is.na(included_groups))) {
      plot.dirs <- list.dirs(base_path, full.names=FALSE, recursive = FALSE)
      plot.dirs <- grep(paste(unique(included_groups), collapse="|"), plot.dirs, value=TRUE)
      plot.dirs <- file.path(base_path, c("CONTENT_AREA_by_GRADE",
                    plot.dirs[unlist(lapply(unique(included_groups), function(f) agrep(f, plot.dirs)))]))
    } else {
      plot.dirs <- file.path(base_path, "CONTENT_AREA_by_GRADE")
    }
    combinePDF(plots_path = plot.dirs, output_dir = base_path)
}  ###  END makeCatalog


getQuantiles <- function(vals, q = seq(0,1, 0.01)) {
  tmp.pts <- sort(unique(vals))
  denom <- length(tmp.pts)
  if (denom > 1) {
    if (denom > 9) {
      tmp.cuts <- quantile(vals, probs = q, na.rm = TRUE, names = FALSE)
      tmp.qntl <- (findInterval(vals, tmp.cuts, rightmost.closed = TRUE)-1)/length(q)
    } else {
      if (denom > 2) {
        tmp.qntl <- as.numeric(as.character(factor(vals, levels = tmp.pts, labels = c(0, round(((2:(denom-1))/9)*(9/(denom+1)), 1), 0.9))))
      } else tmp.qntl <- as.numeric(as.character(factor(vals, levels = tmp.pts, labels = c(0, 0.5)))) # denom == 2
      # tmp.qntl <- findInterval(vals, tmp.pts, rightmost.closed = TRUE)-1 # also works, but not spaced out
    }
    return(tmp.qntl)
  } else {
    return(1)
  }
}  ###  END getQuantiles

roundUp <- function(x, to=5) to*(x%/%to + as.logical(x%%to))
