##############################################################################
###
### covidImpactVisualization function
###
##############################################################################

covidImpactVisualization <- function(
    tmp_data,
    content_area,
    current_grade,
    prior_grade,
    student_group,
    parameters,
    output_type="PDF",
    save_grobs = FALSE
) {

### Checks on parameters
if (!is.null(parameters[['prior_score_variable']])) {
    prior_score_variable <- parameters[['prior_score_variable']]
} else prior_score_variable <- "SCALE_SCORE_PRIOR_2YEAR"

content_area_label <- ifelse(is.null(parameters[['content_area_label']]), capwords(content_area), parameters[['content_area_label']][[content_area]])
academic_impact_label <- ifelse(is.null(parameters[['academic_impact_label']]), capwords(content_area), parameters[['academic_impact_label']][[content_area]])

small_n_group_size <- 50

##   Plot Directory
if (!dir.exists(tmp_data[['file_path']])) dir.create(tmp_data[['file_path']], recursive=TRUE)

##   Academic_Impact_Overview file
if (!file.exists("../Data/Academic_Impact_Overview.Rdata")) Academic_Impact_Overview <- list() else load("../Data/Academic_Impact_Overview.Rdata")
if (is.null(student_group)) {
    Academic_Impact_Overview_Group <- "ALL_STUDENTS"; Splinefun_Label <- "subgroup"
} else {
    Academic_Impact_Overview_Group <- student_group[['TITLE_LABEL']]; Splinefun_Label <- "overall"
}

### Create grid graphic
    ### Layout and viewports
    figure_vp <- viewport(name = "figure_vp",
               layout = grid.layout(9, 9,
                  widths = unit(c(0.05, 0.3, 0.04, 0.01, 0.06, 0.3, 0.04, 0.01, 0.19)*parameters[['graphic_format']][['fig_width']], rep("inches", 9)),
                  heights = unit(c(0.09, 0.025, 0.065, 0.315, 0.05, 0.02, 0.11, 0.25, 0.075)*parameters[['graphic_format']][['fig_height']], rep("inches", 9))),
               gp=gpar(cex=parameters[['graphic_format']][['fig_width']]/17))

    title_vp <- viewport(name="title_vp",
               layout.pos.row=1, layout.pos.col=1:9,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    subtitle_left_vp <- viewport(name="subtitle_left_vp",
               layout.pos.row=2, layout.pos.col=1:3,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    subtitle_middle_vp <- viewport(name="subtitle_middle_vp",
               layout.pos.row=2, layout.pos.col=5:7,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    subtitle_right_vp <- viewport(name="subtitle_right_vp",
               layout.pos.row=2, layout.pos.col=9,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    # Now created with hexViewport below
    # top_left_figure_vp <- viewport(name="top_left_figure_vp",
    #            layout.pos.row=4, layout.pos.col=2,
    #            xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
    #            yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
    #            gp=gpar(fill="transparent"))

    top_right_figure_vp <- viewport(name="top_right_figure_vp",
               layout.pos.row=4, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
               gp=gpar(fill="transparent"))

    top_left_figure_top_axis_vp <- viewport(name="top_left_figure_top_axis_vp",
               layout.pos.row=3, layout.pos.col=2,
               xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    top_right_figure_top_axis_vp <- viewport(name="top_right_figure_top_axis_vp",
               layout.pos.row=3, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    top_left_figure_left_axis_vp <- viewport(name="top_left_figure_left_axis_vp",
               layout.pos.row=4, layout.pos.col=1,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
               gp=gpar(fill="transparent"))

    top_right_figure_left_axis_vp <- viewport(name="top_right_figure_left_axis_vp",
               layout.pos.row=4, layout.pos.col=5,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
               gp=gpar(fill="transparent"))

    top_left_figure_right_axis_vp <- viewport(name="top_left_figure_right_axis_vp",
               layout.pos.row=4, layout.pos.col=3,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
               gp=gpar(fill="transparent"))

    top_right_figure_right_axis_vp <- viewport(name="top_right_figure_right_axis_vp",
               layout.pos.row=4, layout.pos.col=7,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
               gp=gpar(fill="transparent"))

    top_left_figure_bottom_axis_vp <- viewport(name="top_left_figure_bottom_axis_vp",
               layout.pos.row=5, layout.pos.col=2,
               xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    top_right_figure_bottom_axis_vp <- viewport(name="top_right_figure_bottom_axis_vp",
               layout.pos.row=5, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    left_right_figure_sep_vp <- viewport(name="left_right_figure_sep_vp",
               layout.pos.row=3:9, layout.pos.col=4,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    bottom_left_figure_vp <- viewport(name="bottom_left_figure_vp",
               layout.pos.row=8, layout.pos.col=2,
               xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BL']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_right_figure_vp <- viewport(name="bottom_right_figure_vp",
               layout.pos.row=8, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BR']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_left_figure_top_axis_vp <- viewport(name="bottom_left_figure_top_axis_vp",
               layout.pos.row=7, layout.pos.col=2,
               xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    bottom_right_figure_top_axis_vp <- viewport(name="bottom_right_figure_top_axis_vp",
               layout.pos.row=7, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    bottom_left_figure_left_axis_vp <- viewport(name="bottom_left_figure_left_axis_vp",
               layout.pos.row=8, layout.pos.col=1,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BL']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_right_figure_left_axis_vp <- viewport(name="bottom_right_figure_left_axis_vp",
               layout.pos.row=8, layout.pos.col=5,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BR']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_left_figure_right_axis_vp <- viewport(name="bottom_left_figure_right_axis_vp",
               layout.pos.row=8, layout.pos.col=3,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BL']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_right_figure_right_axis_vp <- viewport(name="bottom_right_figure_right_axis_vp",
               layout.pos.row=8, layout.pos.col=7,
               xscale=c(0,1),
               yscale=extendrange(parameters[[content_area]][['academic_impact_axis_range_BR']], f=0.01),
               gp=gpar(fill="transparent"))

    bottom_left_figure_bottom_axis_vp <- viewport(name="bottom_left_figure_bottom_axis_vp",
               layout.pos.row=9, layout.pos.col=2,
               xscale=extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    bottom_right_figure_bottom_axis_vp <- viewport(name="bottom_right_figure_bottom_axis_vp",
               layout.pos.row=9, layout.pos.col=6,
               xscale=extendrange(parameters[[content_area]][['x_axis_range_cohort_comparison']], f=0.01),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    right_overview_figure_sep_vp <- viewport(name="right_overview_figure_sep_vp",
               layout.pos.row=3:9, layout.pos.col=8,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    overview_vp <- viewport(name="overview_vp",
                layout.pos.row=3:9, layout.pos.col=9,
                xscale=c(0,1),
                yscale=c(0,1),
                gp=gpar(fill="transparent"))

    growth_left_vp <- viewport(name="growth_left_vp",
                layout.pos.row=2:8, layout.pos.col=1:3,
                xscale=c(0,1),
                yscale=c(0,1),
                gp=gpar(fill="transparent"))

    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(tmp_data[['file_path']], tmp_data[['file_name']]),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']],
            bg=parameters[['graphic_format']][['colors_background']])
        main.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=3.0)
        sub.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontfamily="Helvetica-Narrow", cex=1.7)
    }
    if (output_type=="PNG") {
        png(file=file.path(tmp_data[['file_path']], gsub(".pdf", ".png", tmp_data[['file_name']])),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']],
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
        main.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica", cex=2.5)
        sub.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica", cex=1.35)
    }
    if (output_type=="SVG") {
        svglite::svglite(filename = file.path(tmp_data[['file_path']], gsub(".pdf", ".svgz", tmp_data[['file_name']])),
            pointsize = 12,
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']])
        main.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica", cex=2.5)
        sub.gpar <- gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica", cex=1.35)
    }

    ### figure_vp
    pushViewport(figure_vp)

    ### title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.02, y=0.5, paste(getStateAbbreviation(parameters[['state_abb']], type="LONG"), strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=main.gpar, just="left", default.units="native")
        grid.text(x=0.96, y=0.65, paste(parameters[['assessment_abb']], content_area_label, "Grade", current_grade), gp=sub.gpar, just="right", default.units="native")
        if (!is.null(student_group)) {
            grid.text(x=0.96, y=0.35, paste0(student_group[['TITLE_LABEL']]), gp=sub.gpar, just="right", default.units="native")
        } else {
            grid.text(x=0.96, y=0.35, "All Students", gp=sub.gpar, just="right", default.units="native")
        }
    popViewport()

    ### subtitle_left_vp
    if (!is.na(prior_grade)){
        pushViewport(subtitle_left_vp)
            grid.roundrect(width=unit(0.975, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
            grid.text(x=0.5, y=0.5, paste("Growth: Grade", prior_grade, strtail(parameters[['prior_year']], 4), "to Grade", current_grade, strtail(parameters[['current_year']], 4)), gp=gpar(cex=1.1), just="center", default.units="native")
        popViewport()
    }

    ### subtitle_middle_vp
    pushViewport(subtitle_middle_vp)
        grid.roundrect(width=unit(0.975, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.5, y=0.5, paste("Status: Grade", current_grade, strtail(parameters[['prior_year']], 4), "to Grade", current_grade, strtail(parameters[['current_year']], 4)), gp=gpar(cex=1.1), just="center", default.units="native")
    popViewport()

    ### subtitle_right_vp
    pushViewport(subtitle_right_vp)
        grid.roundrect(width=unit(0.975, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.5, y=0.5, "Summary", gp=gpar(cex=1.1), just="center", default.units="native")
    popViewport()


    ### overview_vp
    pushViewport(overview_vp)
        base_academic_impact_y_coor <- 0.95
        base_participation_y_coor <- 0.78
        base_key_y_coor <- 0.5
        grid.text(x=0.025, y=base_academic_impact_y_coor, "Academic Impact: Overall and by Quintile", gp=gpar(cex=1), just="left")

        ###############################################
        ### Academic Impact Overview: STATUS & GROWTH
        ###############################################
        academicImpactValues <- getAcademicImpactValues(tmp_data, parameters, student_group, prior_grade)

        ### OVERALL IMPACT ###
        ## STATUS IMPACT
        overall_academic_impact_info_STATUS_OBSERVED <- getAcademicImpactInfo(academicImpactValues[['overall_academic_impact_status']],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Observed",
                                                                            small_n=(academicImpactValues[['overall_academic_impact_N_status']] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['overall_quintile_group_percentages_status']])
        overall_academic_impact_info_STATUS_IMPUTED <- getAcademicImpactInfo(academicImpactValues[['overall_academic_impact_imputed_status']],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Imputed",
                                                                            small_n=(academicImpactValues[['overall_academic_impact_imputed_N_status']] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['overall_quintile_group_percentages_status']])

        ## GROWTH IMPACT
        overall_academic_impact_info_GROWTH_OBSERVED <- getAcademicImpactInfo(academicImpactValues[['overall_academic_impact_growth']],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Observed",
                                                                            small_n=(academicImpactValues[['overall_academic_impact_N_growth']] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['overall_quintile_group_percentages_growth']])
        overall_academic_impact_info_GROWTH_IMPUTED <- getAcademicImpactInfo(academicImpactValues[['overall_academic_impact_imputed_growth']],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Imputed",
                                                                            small_n=(academicImpactValues[['overall_academic_impact_imputed_N_growth']] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['overall_quintile_group_percentages_growth']])

        ### EMBED RESULT in Academic_Impact_Overview Object
        Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][['Overall']][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Observed']] <- overall_academic_impact_info_STATUS_OBSERVED
        Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][['Overall']][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Observed']] <- overall_academic_impact_info_GROWTH_OBSERVED
        Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][['Overall']][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Imputed']] <- overall_academic_impact_info_STATUS_IMPUTED
        Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][['Overall']][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Imputed']] <- overall_academic_impact_info_GROWTH_IMPUTED

        ### PLOT OVERALL ACADEMIC IMPACT RESULT
        ## Pick appropriate Academic Info (growth versus status/observered versus imputed)
        if (!is.na(prior_grade)) {
            if (parameters[['include.imputations']]) {
                overall_academic_impact_info <- overall_academic_impact_info_GROWTH_IMPUTED
                status_polygon_fill <- paste0(overall_academic_impact_info_STATUS_IMPUTED[["Color"]], 80) # Use for single, "overall" polygon coloring and legend
            } else {
                overall_academic_impact_info <- overall_academic_impact_info_GROWTH_OBSERVED
                status_polygon_fill <- paste0(overall_academic_impact_info_STATUS_OBSERVED[["Color"]], 80)
            }
            growth_polygon_fill <- paste0(overall_academic_impact_info[["Color"]], 80)
            if (growth_polygon_fill == "NA80") growth_polygon_fill <- "#FFFFFF80"
        } else {
            if (parameters[['include.imputations']]) {
                overall_academic_impact_info <- overall_academic_impact_info_STATUS_IMPUTED
            } else {
                overall_academic_impact_info <- overall_academic_impact_info_STATUS_OBSERVED
            }
            status_polygon_fill <- paste0(overall_academic_impact_info[["Color"]], 80)
        }
        if (status_polygon_fill == "NA80") status_polygon_fill <- "#FFFFFF80"

        overall_academic_impact_y_coor <- base_academic_impact_y_coor - 0.0425
        grid.rect(x=0.5, y=overall_academic_impact_y_coor, width=unit(0.95, "npc"), height=unit(0.05, "npc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.rect(x=0.5, y=overall_academic_impact_y_coor - 0.0125, width=unit(0.95, "npc"), height=unit(0.025, "npc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], fill=overall_academic_impact_info[['Color']], lwd=1.4))
        grid.text(x=0.5, y=overall_academic_impact_y_coor - 0.0125, overall_academic_impact_info[['Label']], gp=gpar(cex=1, col=overall_academic_impact_info[['Label_Color']]), just="center", default.units="native")
        grid.text(x=0.5, y=overall_academic_impact_y_coor + 0.0125, "Overall", gp=gpar(cex=1, col="black"), just="center", default.units="native")

        ### QUINTILE IMPACT ###
        ## STATUS IMPACT
        quintile_box_sep <- .01; quintile_box_width <- (.95 - 4*quintile_box_sep)/5; first_box_center_coor <- .025 + quintile_box_width/2
        quintile_x_coors <- c(first_box_center_coor, first_box_center_coor + (quintile_box_width + quintile_box_sep) * 1:4)
        quintile_academic_impact_y_coor <- base_academic_impact_y_coor - 0.095
        grid.rect(x=quintile_x_coors, y=quintile_academic_impact_y_coor, width=quintile_box_width, height=0.04)
        for (quintile_iter in 1:5) {
            ### QUINTILE IMPACT ###
            ## STATUS IMPACT (Based upon OVERALL QUINTILES CUTS)
            quintile_academic_impact_info_STATUS_OBSERVED_OVERALL_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_status_overall_cuts']][quintile_iter],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Observed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_N_status_overall_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_status_overall_cuts']])
            quintile_academic_impact_info_STATUS_IMPUTED_OVERALL_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_imputed_status_overall_cuts']][quintile_iter],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Imputed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_imputed_N_status_overall_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_status_overall_cuts']])
            ## GROWTH IMPACT (Based upon OVERALL QUINTILES CUTS)
            quintile_academic_impact_info_GROWTH_OBSERVED_OVERALL_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_growth_overall_cuts']][quintile_iter],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Observed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_N_growth_overall_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_growth_overall_cuts']])
            quintile_academic_impact_info_GROWTH_IMPUTED_OVERALL_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_imputed_growth_overall_cuts']][quintile_iter],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Imputed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_imputed_N_growth_overall_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_growth_overall_cuts']])

            ## STATUS IMPACT (Based upon SUBGROUP QUINTILES CUTS)
            quintile_academic_impact_info_STATUS_OBSERVED_SUBGROUP_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_status_subgroup_cuts']][quintile_iter],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Observed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_N_status_subgroup_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_status_subgroup_cuts']])
            quintile_academic_impact_info_STATUS_IMPUTED_SUBGROUP_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_imputed_status_subgroup_cuts']][quintile_iter],
                                                                            growth_or_status="STATUS",
                                                                            academic_impact_type="Status_Imputed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_imputed_N_status_subgroup_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_status_subgroup_cuts']])
            ## GROWTH IMPACT (Based upon SUBGROUP QUINTILES CUTS)
            quintile_academic_impact_info_GROWTH_OBSERVED_SUBGROUP_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_growth_subgroup_cuts']][quintile_iter],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Observed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_N_growth_subgroup_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_growth_subgroup_cuts']])
            quintile_academic_impact_info_GROWTH_IMPUTED_SUBGROUP_CUTS <- getAcademicImpactInfo(academicImpactValues[['quintile_academic_impact_imputed_growth_subgroup_cuts']][quintile_iter],
                                                                            growth_or_status="GROWTH",
                                                                            academic_impact_type="Growth_Imputed",
                                                                            small_n=(academicImpactValues[['quintile_academic_impact_imputed_N_growth_subgroup_cuts']][quintile_iter] < small_n_group_size),
                                                                            Quintile_Group_Percentages=academicImpactValues[['quintile_group_percentages_growth_subgroup_cuts']])

            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Observed']][['Overall_Cuts']] <- quintile_academic_impact_info_STATUS_OBSERVED_OVERALL_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Observed']][['Overall_Cuts']] <- quintile_academic_impact_info_GROWTH_OBSERVED_OVERALL_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Imputed']][['Overall_Cuts']] <- quintile_academic_impact_info_STATUS_IMPUTED_OVERALL_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Imputed']][['Overall_Cuts']] <- quintile_academic_impact_info_GROWTH_IMPUTED_OVERALL_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Observed']][['Subgroup_Cuts']] <- quintile_academic_impact_info_STATUS_OBSERVED_SUBGROUP_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Observed']][['Subgroup_Cuts']] <- quintile_academic_impact_info_GROWTH_OBSERVED_SUBGROUP_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Status']][['Imputed']][['Subgroup_Cuts']] <- quintile_academic_impact_info_STATUS_IMPUTED_SUBGROUP_CUTS
            Academic_Impact_Overview[[parameters[['assessment_type']]]][[Academic_Impact_Overview_Group]][[paste0("Q", quintile_iter)]][[toupper(academic_impact_label)]][[current_grade]][['Growth']][['Imputed']][['Subgroup_Cuts']] <- quintile_academic_impact_info_GROWTH_IMPUTED_SUBGROUP_CUTS

            if (!is.na(prior_grade)) {
                if (parameters[['include.imputations']]) {
                    quintile_academic_impact_info <- quintile_academic_impact_info_GROWTH_IMPUTED_OVERALL_CUTS
                    assign(paste0("status_pgon_q", quintile_iter), paste0(quintile_academic_impact_info_STATUS_IMPUTED_OVERALL_CUTS[['Color']], 80))
                } else {
                    quintile_academic_impact_info <- quintile_academic_impact_info_GROWTH_OBSERVED_OVERALL_CUTS
                    assign(paste0("status_pgon_q", quintile_iter), paste0(quintile_academic_impact_info_STATUS_OBSERVED_OVERALL_CUTS[['Color']], 80))
                }
                assign(paste0("growth_pgon_q", quintile_iter), paste0(quintile_academic_impact_info[['Color']], 80))
                if (get(paste0("growth_pgon_q", quintile_iter)) == "NA80") assign(paste0("growth_pgon_q", quintile_iter), "#FFFFFF80")
            } else {
                if (parameters[['include.imputations']]) {
                    quintile_academic_impact_info <- quintile_academic_impact_info_STATUS_IMPUTED_OVERALL_CUTS
                } else {
                    quintile_academic_impact_info <- quintile_academic_impact_info_STATUS_OBSERVED_OVERALL_CUTS
                }
                assign(paste0("status_pgon_q", quintile_iter), paste0(quintile_academic_impact_info[['Color']], 80))
            }
            if (get(paste0("status_pgon_q", quintile_iter)) == "NA80") assign(paste0("status_pgon_q", quintile_iter), "#FFFFFF80")

            grid.lines(x=quintile_x_coors[quintile_iter] + c(-quintile_box_width, quintile_box_width)/2, y=c(quintile_academic_impact_y_coor, quintile_academic_impact_y_coor))
            grid.text(x=quintile_x_coors[quintile_iter], y=quintile_academic_impact_y_coor + 0.01, paste0("Q", quintile_iter), gp=gpar(cex=0.9))
            grid.rect(x=quintile_x_coors[quintile_iter], y=quintile_academic_impact_y_coor - 0.01, width=quintile_box_width, height=0.02, gp=gpar(fill=quintile_academic_impact_info[['Color']]))
            grid.text(x=quintile_x_coors[quintile_iter], y=quintile_academic_impact_y_coor - 0.01, quintile_academic_impact_info[['Label']], gp=gpar(col=quintile_academic_impact_info[['Label_Color']], cex=quintile_academic_impact_info[['cex']]))
        }

  # save(Academic_Impact_Overview, file="../Data/Academic_Impact_Overview.Rdata")

        ### Participation
        tmp_counts <- list()
        if (!is.na(prior_grade)){
            tmp_counts[['prior_data']] <- list("total"=dim(tmp_data[['prior_data']])[1], "valid_status"=sum(!is.na(tmp_data[['prior_data']][['SCALE_SCORE']]), na.rm=TRUE), "valid_growth"=sum(!is.na(tmp_data[['prior_data']][['SGP_BASELINE']]), na.rm=TRUE))
            tmp_counts[['current_data']] <- list("total"=dim(tmp_data[['current_data']])[1], "valid_status"=sum(!is.na(tmp_data[['current_data']][['SCALE_SCORE']]), na.rm=TRUE), "valid_growth"=sum(!is.na(tmp_data[['current_data']][['SGP_BASELINE']]), na.rm=TRUE))
            tmp_counts[['prior_data']][['valid_status_percent']] <- format(100*tmp_counts[['prior_data']][['valid_status']]/tmp_counts[['prior_data']][['total']], nsmall=1, digits=1)
            tmp_counts[['current_data']][['valid_status_percent']] <- format(100*tmp_counts[['current_data']][['valid_status']]/tmp_counts[['current_data']][['total']], nsmall=1, digits=1)
            tmp_counts[['prior_data']][['valid_growth_percent']] <- format(100*tmp_counts[['prior_data']][['valid_growth']]/tmp_counts[['prior_data']][['total']], nsmall=1, digits=1)
            tmp_counts[['current_data']][['valid_growth_percent']] <- format(100*tmp_counts[['current_data']][['valid_growth']]/tmp_counts[['current_data']][['total']], nsmall=1, digits=1)
        } else {
            tmp_counts[['prior_data']] <- list("total"=dim(tmp_data[['prior_data']])[1], "valid_status"=sum(!is.na(tmp_data[['prior_data']][['SCALE_SCORE']]), na.rm=TRUE), "valid_growth"=as.character(NA))
            tmp_counts[['current_data']] <- list("total"=dim(tmp_data[['current_data']])[1], "valid_status"=sum(!is.na(tmp_data[['current_data']][['SCALE_SCORE']]), na.rm=TRUE), "valid_growth"=as.character(NA))
            tmp_counts[['prior_data']][['valid_status_percent']] <- format(100*tmp_counts[['prior_data']][['valid_status']]/tmp_counts[['prior_data']][['total']], nsmall=1, digits=1)
            tmp_counts[['current_data']][['valid_status_percent']] <- format(100*tmp_counts[['current_data']][['valid_status']]/tmp_counts[['current_data']][['total']], nsmall=1, digits=1)
            tmp_counts[['prior_data']][['valid_growth_percent']] <- as.character(NA)
            tmp_counts[['current_data']][['valid_growth_percent']] <- as.character(NA)
        }

        grid.text(x=0.025, y=base_participation_y_coor, "Participation", gp=gpar(cex=1), just="left")
        grid.text(x=0.25, y=base_participation_y_coor - 0.03, "Total", gp=gpar(cex=0.9), just="center")
        grid.text(x=0.5, y=base_participation_y_coor - 0.03, "Status", gp=gpar(cex=0.9), just="center")
        grid.text(x=0.8, y=base_participation_y_coor - 0.03, "Growth", gp=gpar(cex=0.9), just="center")
        grid.text(x=0.025, y=base_participation_y_coor - 0.06, strtail(parameters[['prior_year']], 4), gp=gpar(cex=0.8), just="left")
        grid.text(x=0.025, y=base_participation_y_coor - 0.085, strtail(parameters[['current_year']], 4), gp=gpar(cex=0.8), just="left")
        grid.text(x=0.25, y=base_participation_y_coor - 0.06, format(tmp_counts[['prior_data']][['total']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.7), just="center")
        grid.text(x=0.5, y=base_participation_y_coor - 0.06, paste0(format(tmp_counts[['prior_data']][['valid_status']], big.mark=",", scientific=FALSE), " (", tmp_counts[['prior_data']][['valid_status_percent']], "%)"), gp=gpar(cex=0.7), just="center")
        if (!is.na(prior_grade)){
            grid.text(x=0.8, y=base_participation_y_coor - 0.06, paste0(format(tmp_counts[['prior_data']][['valid_growth']], big.mark=",", scientific=FALSE), " (", tmp_counts[['prior_data']][['valid_growth_percent']], "%)"), gp=gpar(cex=0.7), just="center")
        } else {
            grid.text(x=0.8, y=base_participation_y_coor - 0.06, as.character(NA), gp=gpar(cex=0.7), just="center")
        }
        grid.text(x=0.25, y=base_participation_y_coor - 0.085, format(tmp_counts[['current_data']][['total']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.7), just="center")
        grid.text(x=0.5, y=base_participation_y_coor - 0.085, paste0(format(tmp_counts[['current_data']][['valid_status']], big.mark=",", scientific=FALSE), " (", tmp_counts[['current_data']][['valid_status_percent']], "%)"), gp=gpar(cex=0.7), just="center")
        if (!is.na(prior_grade)){
            grid.text(x=0.8, y=base_participation_y_coor - 0.085, paste0(format(tmp_counts[['current_data']][['valid_growth']], big.mark=",", scientific=FALSE), " (", tmp_counts[['current_data']][['valid_growth_percent']], "%)"), gp=gpar(cex=0.7), just="center")
        } else {
            grid.text(x=0.8, y=base_participation_y_coor - 0.085, as.character(NA), gp=gpar(cex=0.7), just="center")
        }


        ### Legend/Key for Academic Impact
        spectrum_sequence <- seq(-40, 20, length=95)
        spectrum_cuts_growth <- c(-40, -25, -15, -5, 5, 20)
        spectrum_cuts_status <- c(-0.55, -0.4, -0.25, -0.1, 0.05, 0.2)
        spectrum_approx_fun <- approxfun(spectrum_sequence, seq(0.025, 0.975, length.out=length(spectrum_sequence)))
        spectrum_labels <- c("Severe", "Large", "Moderate", "Modest\nto\nNone", "Improvement")
        spectrum_labels_cex <- c(0.65, 0.65, 0.65, 0.55, 0.65)
        for (spectrum_iter in seq_along(spectrum_sequence)-1) {
            grid.lines(x=rep((0.03+spectrum_iter/100), 2), y=c(base_key_y_coor-0.023, base_key_y_coor+0.023), gp=gpar(lwd=3.3, lineend=2, col=getAcademicImpactInfo(spectrum_sequence[spectrum_iter+1])[['Color']]))
        }
        grid.rect(x=0.5, y=base_key_y_coor, width=0.95, height=0.05)
        grid.lines(x=c(0.025, 0.975), y=rep(base_key_y_coor + 0.05, 2), gp=gpar(lwd=1.0))
        grid.text(x=0.025, y=base_key_y_coor + 0.09, "Growth/Status Change Legend", gp=gpar(cex=1.0), just="left")
        for (spectrum_cuts_iter in seq_along(spectrum_cuts_growth)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_cuts_growth[spectrum_cuts_iter])
            grid.lines(x=c(tmp_x_coor, tmp_x_coor), y=base_key_y_coor+c(0.045, 0.055), gp=gpar(lwd=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.0625, spectrum_cuts_growth[spectrum_cuts_iter], gp=gpar(cex=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.0375, spectrum_cuts_status[spectrum_cuts_iter], gp=gpar(cex=0.5))
        }
        spectrum_label_values <- (head(spectrum_cuts_growth, -1) + tail(spectrum_cuts_growth, -1))/2
        for (spectrum_cuts_iter in seq_along(spectrum_label_values)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_label_values[spectrum_cuts_iter])
            grid.text(x=tmp_x_coor, y=base_key_y_coor, spectrum_labels[spectrum_cuts_iter], gp=gpar(col="white", cex=spectrum_labels_cex[spectrum_cuts_iter]))
        }
        grid.rect(x=0.05, y=base_key_y_coor-0.065, width=0.05, height=0.05)
        grid.text(x=0.1, y=base_key_y_coor-0.065, "Calculations suppressed\ndue to small group N (< 50)", just="left", gp=gpar(cex=0.8))

    ### Pop overview_vp
    popViewport()

    #############################
    ### GROWTH FIGURE
    #############################
    if (!is.na(prior_grade)) {
        ### top_left_figure_vp
        tmp.bins <- roundUp(length(unique(tmp_data[['current_data']][[prior_score_variable]]))/10)
        bin <- hexbin(x=tmp_data[['current_data']][[prior_score_variable]],
                      y=tmp_data[['current_data']][['SCALE_SCORE']],
                      xbins=max(tmp.bins, 50), xlab = "", ylab="")

        top_left_figure_vp <- hexViewport(bin, mar = unit(rep(0,4), "npc"), #, 0.0125, 0.01, 0.0125)
                    xbnds = extendrange(parameters[[content_area]][['x_axis_range']], f=0.01),
                    ybnds = extendrange(parameters[[content_area]][['y_axis_range']], f=0.01),
                    clip = "on", vp.name="top_left_figure_vp")
        top_left_figure_vp@hexVp.on$name <- "top_left_figure_vp"
        top_left_figure_vp@hexVp.on$valid.pos.row <- top_left_figure_vp@hexVp.on$layout.pos.row <- c(4L, 4L)
        top_left_figure_vp@hexVp.on$valid.pos.col <- top_left_figure_vp@hexVp.on$layout.pos.col <- c(2L, 2L)
        top_left_figure_vp@hexVp.on$width <- figure_vp$layout$widths[2]
        top_left_figure_vp@hexVp.on$height <- figure_vp$layout$heights[4]
        top_left_figure_vp@hexVp.on$gp <- gpar(fill="transparent")

        pushHexport(top_left_figure_vp, clip="on")
            grid.clip(
                x=sum(parameters[[content_area]][['x_axis_range']])/2,
                y=sum(parameters[[content_area]][['x_axis_range']])/2,
                width=diff(parameters[[content_area]][['x_axis_range']]),
                height=diff(parameters[[content_area]][['y_axis_range']]), default.units="native")
            # grid.hexagons(bin, style = "lattice", pen = gray(0.8), border = gray(0.8))#, border = gray(.1), pen = gray(.6), minarea = .1, maxarea = 1.5)style = c("colorscale", "centroids", "lattice"
            grid.hexagons(bin, style = "colorscale", colramp = function(n){LinGray(n,beg = 95, end = 65)})

            for (cutscore.iter in tmp_data[['achievement_cutscores_prior']][[paste("GRADE", prior_grade, sep="_")]]) {
                grid.lines(x=c(cutscore.iter, cutscore.iter), y=parameters[[content_area]][['y_axis_range']],
                    gp=gpar(col="blue", lty=2, lwd=1.0), default.units="native")
            }
            for (cutscore.iter in tmp_data[['achievement_cutscores_current']][[paste("GRADE", current_grade, sep="_")]]) {
                grid.lines(x=parameters[[content_area]][['x_axis_range']], y=c(cutscore.iter, cutscore.iter),
                    gp=gpar(col="blue", lty=2, lwd=1.0), default.units="native")
            }

            # grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']], rev(tmp_data[['plotting_domain_sequence_growth']])), y=c(tmp_data[['current_data_lm_fitted_values']], rev(tmp_data[['prior_data_lm_fitted_values']])), gp=gpar(fill=growth_polygon_fill, lwd=0.0, col=rgb(1, 0, 0, 0.5)), default.units="native") #fill=rgb(1, 0, 0, 0.5)
            grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']][1:19], rev(tmp_data[['plotting_domain_sequence_growth']][1:19])), y=c(tmp_data[['current_data_lm_fitted_values']][1:19], rev(tmp_data[['prior_data_lm_fitted_values']][1:19])), gp=gpar(fill=growth_pgon_q1, lwd=0.0, col=growth_pgon_q1), default.units="native")
            grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']][19:39], rev(tmp_data[['plotting_domain_sequence_growth']][19:39])), y=c(tmp_data[['current_data_lm_fitted_values']][19:39], rev(tmp_data[['prior_data_lm_fitted_values']][19:39])), gp=gpar(fill=growth_pgon_q2, lwd=0.0, col=growth_pgon_q2), default.units="native")
            grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']][39:59], rev(tmp_data[['plotting_domain_sequence_growth']][39:59])), y=c(tmp_data[['current_data_lm_fitted_values']][39:59], rev(tmp_data[['prior_data_lm_fitted_values']][39:59])), gp=gpar(fill=growth_pgon_q3, lwd=0.0, col=growth_pgon_q3), default.units="native")
            grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']][59:79], rev(tmp_data[['plotting_domain_sequence_growth']][59:79])), y=c(tmp_data[['current_data_lm_fitted_values']][59:79], rev(tmp_data[['prior_data_lm_fitted_values']][59:79])), gp=gpar(fill=growth_pgon_q4, lwd=0.0, col=growth_pgon_q4), default.units="native")
            grid.polygon(x=c(tmp_data[['plotting_domain_sequence_growth']][79:95], rev(tmp_data[['plotting_domain_sequence_growth']][79:95])), y=c(tmp_data[['current_data_lm_fitted_values']][79:95], rev(tmp_data[['prior_data_lm_fitted_values']][79:95])), gp=gpar(fill=growth_pgon_q5, lwd=0.0, col=growth_pgon_q5), default.units="native")

            grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['current_data_lm_fitted_values']], gp=gpar(col="red"), default.units="native")
            grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['prior_data_lm_fitted_values']], default.units="native")
            if (parameters[['include.imputations']])
              grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['current_data_lm_imputed_fitted_values']], gp=gpar(lty=3, col="black"), default.units="native")

            ## Legend
            if (parameters[['include.imputations']]) height_adjustment <- 0.0 else height_adjustment <- 0.03
            grid.rect(x=unit(0.02, "npc"), y=unit(0.98, "npc"), width=unit(0.45, "npc"), height=unit(0.13 - height_adjustment, "npc"), just=c("left", "top"), gp=gpar(fill = parameters[['graphic_format']][['colors_background']]))
            grid.lines(x=c(0.04, 0.07), y=c(0.96, 0.96), gp=gpar(lwd=1.2))
            grid.text(x=0.08, y=0.96, paste(strtail(parameters[['prior_year_base']], 4), "to", strtail(parameters[['prior_year']], 4), "Mean Conditional Status"), just="left", gp=gpar(cex=0.5))
            grid.lines(x=c(0.04, 0.07), y=c(0.93, 0.93), gp=gpar(lwd=1.2, col="red"))
            grid.text(x=0.08, y=0.93, paste(strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "Mean Conditional Status (Observed)"), just="left", gp=gpar(cex=0.5))
            if (parameters[['include.imputations']]) {
              grid.lines(x=c(0.04, 0.07), y=c(0.90, 0.90), gp=gpar(lwd=1.2, col="black", lty=3))
              grid.text(x=0.08, y=0.90, paste(strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "Mean Conditional Status (+ Imputed)"), just="left", gp=gpar(cex=0.5))
            }
            grid.rect(x=.045, y=0.86 + height_adjustment, width=0.02, height=0.02, gp=gpar(fill=growth_polygon_fill, lwd=0.3), just=c("left", "bottom"))
            grid.text(x=0.08, y=0.87 + height_adjustment, paste(strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "Growth Impact"), just="left", gp=gpar(cex=0.5))
        popViewport() # top_left_figure_vp

        ### top_left_figure_left_axis_vp
        pushViewport(top_left_figure_left_axis_vp)
            grid.text(x=0.4, y=0.5, paste(strtail(parameters[['current_year']], 4), "Grade", current_grade, "Scaled Score"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), rot=90, just="center")
            grid.lines(x=c(1,1), y=parameters[[content_area]][['y_axis_range']], default.units="native")
            for (tick_iter in parameters[[content_area]][['y_axis_ticks']]) {
                grid.lines(x=c(0.9, 1), y=c(tick_iter, tick_iter), default.units="native")
                grid.text(x=0.86, y=tick_iter, tick_iter, gp=gpar(cex=0.5), just="right", default.units="native")
            }
        popViewport()

        ### top_left_figure_bottom_axis_vp
        pushViewport(top_left_figure_bottom_axis_vp)
            grid.lines(x=parameters[[content_area]][['x_axis_range']], y=c(1,1), default.units="native")
            for (tick_iter in parameters[[content_area]][['x_axis_ticks']]) {
                grid.lines(x=c(tick_iter, tick_iter), y=c(0.9, 1), default.units="native")
                grid.text(x=tick_iter, y=0.82, tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
            }

            grid.text(x=0.5, y=0.4, paste(strtail(parameters[['prior_year']], 4), "Grade", prior_grade, "Scaled Score with Achievement Level Cuts"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
        popViewport()

        ### top_left_figure_top_axis_vp
        if (!is.null(student_group)) {
            pushViewport(top_left_figure_top_axis_vp)
                grid.text(x=0.5, y=0.8, paste(strtail(parameters[['prior_year']], 4), "Grade", prior_grade, "Scaled Score Decile Cuts"),
                    gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
                tmp_deciles_subset <- tail(head(tmp_data[['decile_breaks_scale_score_prior_current_data']], -1), -1)
                tmp_deciles_no_subset <- tail(head(tmp_data[['decile_breaks_scale_score_prior_no_subset_current_data']], -1), -1)
                tmp_deciles_range <- range(c(tmp_deciles_subset, tmp_deciles_no_subset))
                grid.lines(x=tmp_deciles_range, y=c(0.4,0.4), default.units="native")
                for (tick_iter in seq_along(tmp_deciles_subset)) {
                    grid.lines(x=c(tmp_deciles_subset[tick_iter], tmp_deciles_subset[tick_iter]), y=c(0.4, 0.3), default.units="native")
                    grid.text(x=tmp_deciles_subset[tick_iter], y=0.22, 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
                }
                for (tick_iter in seq_along(tmp_deciles_no_subset)) {
                    grid.lines(x=c(tmp_deciles_no_subset[tick_iter], tmp_deciles_no_subset[tick_iter]), y=c(0.4, 0.5), default.units="native")
                    grid.text(x=tmp_deciles_no_subset[tick_iter], y=0.58, 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
                }
                grid.text(x=unit(tmp_deciles_range[1], "native")-unit(0.025, "npc"), y=unit(0.48, "native"), "All Students", gp=gpar(parameters[['graphic_format']][['colors.font']][2], cex=0.5), just="right")
                grid.text(x=unit(tmp_deciles_range[1], "native")-unit(0.025, "npc"), y=unit(0.32, "native"), student_group[['TITLE_LABEL']], gp=gpar(parameters[['graphic_format']][['colors.font']][2], cex=0.5), just="right")
            popViewport()
        } else {
            pushViewport(top_left_figure_top_axis_vp)
                grid.text(x=0.5, y=0.6, paste(strtail(parameters[['prior_year']], 4), "Grade", prior_grade, "Scaled Score Decile Cuts"),
                    gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
                tmp_deciles <- tail(head(tmp_data[['decile_breaks_scale_score_prior_current_data']], -1), -1)
                grid.lines(x=range(tmp_deciles), y=c(0.3,0.3), default.units="native")
                for (tick_iter in seq_along(tmp_deciles)) {
                    grid.lines(x=c(tmp_deciles[tick_iter], tmp_deciles[tick_iter]), y=c(0.3, 0.2), default.units="native")
                    grid.text(x=tmp_deciles[tick_iter], y=0.12, 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
                }
            popViewport()
        }

        ### top_left_figure_right_axis_vp
        pushViewport(top_left_figure_right_axis_vp)
            if (is.null(student_group)) {
                tmp_deciles <- tail(head(tmp_data[['decile_breaks_current_data']], -1), -1)
                if (parameters[['include.imputations']]) tmp_deciles_imputed <- tail(head(tmp_data[['decile_breaks_current_data_imputed']], -1), -1) else tmp_deciles_imputed <- NULL
            } else {
                tmp_deciles <- tail(head(tmp_data[['decile_breaks_current_data_no_subset']], -1), -1)
                if (parameters[['include.imputations']]) tmp_deciles_imputed <- tail(head(tmp_data[['decile_breaks_current_data_imputed_no_subset']], -1), -1) else tmp_deciles_imputed <- NULL
            }
            grid.text(x=0.7, y=tmp_deciles[5], paste(strtail(parameters[['current_year']], 4), "Grade", current_grade, "Scaled Score Decile Cuts"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center", rot=90, default.units="native")
            grid.lines(x=c(0.2, 0.2), y=range(tmp_deciles, tmp_deciles_imputed), default.units="native")
            for (tick_iter in seq_along(tmp_deciles)) {
                grid.lines(x=c(0.1, 0.2), y=c(tmp_deciles[tick_iter], tmp_deciles[tick_iter]), default.units="native")
                grid.text(x=0.02, y=tmp_deciles[tick_iter], 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
            }
            if (parameters[['include.imputations']]) {
              for (tick_iter in seq_along(tmp_deciles_imputed)) {
                  grid.lines(x=c(0.2, 0.3), y=c(tmp_deciles_imputed[tick_iter], tmp_deciles_imputed[tick_iter]), default.units="native")
                  grid.text(x=0.38, y=tmp_deciles_imputed[tick_iter], 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
              }
            }

            label_margin <- unit(tmp_deciles[9], "native") + unit(0.05, "npc")
            grid.text(x=0.1, y=label_margin, "Observed", gp=gpar(parameters[['graphic_format']][['colors.font']][2], cex=0.5), just="left", rot=90, default.units="native")
            if (parameters[['include.imputations']])
              grid.text(x=0.3, y=label_margin, "Observed + Imputed", gp=gpar(parameters[['graphic_format']][['colors.font']][2], cex=0.5), just="left", rot=90, default.units="native")
        popViewport()

        ### bottom_left_figure_vp
        pushViewport(bottom_left_figure_vp)
            for (cutscore.iter in tmp_data[['achievement_cutscores_prior']][[paste("GRADE", prior_grade, sep="_")]]) {
                grid.lines(x=c(cutscore.iter, cutscore.iter), y=parameters[[content_area]][['academic_impact_axis_range_BL']],
                    gp=gpar(col="blue", lty=2, lwd=0.9), default.units="native")
            }

            grid.lines(x=parameters[[content_area]][['x_axis_range']], y=c(0,0), default.units="native")
            grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['conditional_status_change']], gp=gpar(col="red", lty=3), default.units="native")
            grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['sgp_change']], gp=gpar(col="red"), default.units="native")
            if (parameters[['include.imputations']])
              grid.lines(x=tmp_data[['plotting_domain_sequence_growth']], y=tmp_data[['sgp_change_imputed']], gp=gpar(col="black", lty=3), default.units="native")
            tmp_quintile_range_cuts <- head(tail(tmp_data[['quintile_breaks_scale_score_prior_current_data']], -1), -1)
            for (line_iter in tmp_quintile_range_cuts){
                grid.lines(x=c(line_iter, line_iter), y=parameters[[content_area]][['academic_impact_axis_range_BL']], gp=gpar(lty=3, col="grey70"), default.units="native")
            }

            ## Legend
            if (parameters[['include.imputations']]) height_adjustment <- 0.0 else height_adjustment <- 0.03
            grid.rect(x=unit(0.02, "npc"), y=unit(0.98, "npc"), width=unit(0.48, "npc"), height=unit(0.18 - height_adjustment, "npc"), just=c("left", "top"), gp=gpar(fill = parameters[['graphic_format']][['colors_background']]))
            grid.lines(x=c(0.04, 0.07), y=c(0.95, 0.95), gp=gpar(lwd=1.2, col="red"))
            grid.text(x=0.08, y=0.95, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "SGP Difference (Observed)"), just="left", gp=gpar(cex=0.5))
            if (parameters[['include.imputations']]) {
              grid.lines(x=c(0.04, 0.07), y=c(0.91, 0.91), gp=gpar(lwd=1.2, col="black", lty=3))
              grid.text(x=0.08, y=0.91, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "SGP Difference (+ Imputed)"), just="left", gp=gpar(cex=0.5))
            }
            grid.lines(x=c(0.04, 0.07), y=c(0.87, 0.87) + height_adjustment, gp=gpar(lwd=1.2, col="red", lty=3))
            grid.text(x=0.08, y=0.87 + height_adjustment, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "Conditional Status Difference (Observed)"), just="left", gp=gpar(cex=0.5))
            grid.text(x=0.04, y=0.83 + height_adjustment, paste0(format(cor(tmp_data[['current_data_lm_fitted_values']] - tmp_data[['prior_data_lm_fitted_values']], tmp_data[['current_data_sgp_lm_fitted_values']] - tmp_data[['prior_data_sgp_lm_fitted_values']], use="na.or.complete"), digits=2, nsmall=2), ": Correlation SGP and Conditional Status Difference"), just="left", gp=gpar(cex=0.5))
        popViewport()

        ### bottom_left_figure_left_axis_vp
        pushViewport(bottom_left_figure_left_axis_vp)
            grid.text(x=0.25, y=0.5, paste(strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4)),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), rot=90, just="center")
            grid.text(x=0.5, y=0.5, "Conditional Status/Growth Change",
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), rot=90, just="center")
            grid.lines(x=c(1,1), y=parameters[[content_area]][['academic_impact_axis_range_BL']], default.units="native")
            for (tick_iter in parameters[[content_area]][['academic_impact_axis_ticks_BL']]) {
                grid.lines(x=c(0.9, 1), y=c(tick_iter, tick_iter), default.units="native")
                grid.text(x=0.86, y=tick_iter, tick_iter, gp=gpar(cex=0.5), just="right", default.units="native")
            }
        popViewport()

        ### bottom_left_figure_bottom_axis_vp
        pushViewport(bottom_left_figure_bottom_axis_vp)
            grid.text(x=0.5, y=0.2, paste(strtail(parameters[['prior_year']], 4), "Grade", prior_grade, "Scaled Score with Decile & Achievement Level Cuts"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
            grid.lines(x=parameters[[content_area]][['x_axis_range']], y=c(0.6, 0.6), default.units="native")
            for (tick_iter in parameters[[content_area]][['x_axis_ticks']]) {
                grid.lines(x=c(tick_iter, tick_iter), y=c(0.5, 0.6), default.units="native")
                grid.text(x=tick_iter, y=0.42, tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
            }
            tmp_deciles <- tail(head(tmp_data[['decile_breaks_scale_score_prior_current_data']], -1), -1)
            grid.lines(x=range(tmp_deciles), y=c(0.7,0.7), default.units="native")
            for (tick_iter in seq_along(tmp_deciles)) {
                grid.lines(x=c(tmp_deciles[tick_iter], tmp_deciles[tick_iter]), y=c(0.7, 0.8), default.units="native")
                grid.text(x=tmp_deciles[tick_iter], y=0.88, 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
            }
        popViewport()

        ### bottom_left_figure_top_axis_vp
        pushViewport(bottom_left_figure_top_axis_vp)
            if (parameters[['include.imputations']]) height_adjustment <- 0.0 else height_adjustment <- 0.1
            grid.text(x=0.5, y=1.0, paste(strtail(parameters[['prior_year']], 4), "&", strtail(parameters[['current_year']], 4), "Grade", current_grade, "Median SGPs by Quintile"), gp=gpar(cex=0.8))
            tmp_left_text_margin <- 0.6*tmp_data[['decile_breaks_scale_score_prior_current_data']][2] + 0.4*parameters[[content_area]][['x_axis_range']][1]
            tmp_right_text_margin <- 0.4*tmp_data[['decile_breaks_scale_score_prior_current_data']][10] + 0.6*parameters[[content_area]][['x_axis_range']][2]
            grid.text(x=tmp_left_text_margin, y=0.7, paste(strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Students"), just="right", gp=gpar(cex=0.5), default.units="native")
            grid.text(x=tmp_left_text_margin, y=0.6, paste(strtail(parameters[['current_year']], 4), "Grade", current_grade, "Students"), just="right", gp=gpar(cex=0.5), default.units="native")
            grid.text(x=tmp_left_text_margin, y=0.5, "Academic Impact (Observed)", just="right", gp=gpar(cex=0.5), default.units="native")
            if (parameters[['include.imputations']])
              grid.text(x=tmp_left_text_margin, y=0.4, "Academic Impact (+ Imputed)", just="right", gp=gpar(cex=0.5), default.units="native")
            grid.text(x=tmp_left_text_margin, y=0.3 + height_adjustment, paste("Count Observed", strtail(parameters[['prior_year']], 4)), just="right", gp=gpar(cex=0.5), default.units="native")
            grid.text(x=tmp_left_text_margin, y=0.2 + height_adjustment, paste("Count Observed", strtail(parameters[['current_year']], 4)), just="right", gp=gpar(cex=0.5), default.units="native")
            tmp_quintile_median_coordinates <- tmp_data[['decile_breaks_scale_score_prior_current_data']][c(2,4,6,8,10)]
            grid.text(x=tmp_quintile_median_coordinates, y=0.83, c("Q1", "Q2", "Q3", "Q4", "Q5"), gp=gpar(cex=0.65), default.units="native", just="center")
            grid.text(x=tmp_quintile_median_coordinates, y=0.7, format(tmp_data[['prior_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']], nsmall=1), gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_median_coordinates, y=0.6, format(tmp_data[['current_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']], nsmall=1), gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_median_coordinates, y=0.5, format(tmp_data[['current_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']] - tmp_data[['prior_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']], digits=1, nsmall=1), gp=gpar(cex=0.5), default.units="native", just="center")
            if (parameters[['include.imputations']])
              grid.text(x=tmp_quintile_median_coordinates, y=0.4, format(tmp_data[['current_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE_IMPUTED']] - tmp_data[['prior_data_summaries_quintiles']][['MEDIAN_SGP_BASELINE']], digits=1, nsmall=1), gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_median_coordinates, y=0.3 + height_adjustment, format(tmp_data[['prior_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_quintile_median_coordinates, y=0.2 + height_adjustment, format(tmp_data[['current_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")

            ## Overall SGP
            grid.text(x=tmp_right_text_margin, y=0.83, "Overall", gp=gpar(cex=0.7), just="center", default.units="native")
            grid.text(x=tmp_right_text_margin, y=0.7, format(as.numeric(median(tmp_data[['prior_data']][['SGP_BASELINE']], na.rm=TRUE)), nsmall=1), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_right_text_margin, y=0.6, format(as.numeric(median(tmp_data[['current_data']][['SGP_BASELINE']], na.rm=TRUE)), nsmall=1), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_right_text_margin, y=0.5, format(median(tmp_data[['current_data']][['SGP_BASELINE']], na.rm=TRUE) - median(tmp_data[['prior_data']][['SGP_BASELINE']], na.rm=TRUE), digits=1, nsmall=1), gp=gpar(cex=0.5), just="center", default.units="native")
            if (parameters[['include.imputations']]) {
              grid.text(x=tmp_right_text_margin, y=0.4, format(median(tmp_data[['current_data']][['MEAN_SGP_BASELINE_IMPUTED']], na.rm=TRUE) - median(tmp_data[['prior_data']][['SGP_BASELINE']], na.rm=TRUE), digits=1, nsmall=1), gp=gpar(cex=0.5), just="center", default.units="native")
            }
            grid.text(x=tmp_right_text_margin, y=0.3 + height_adjustment, format(sum(tmp_data[['prior_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE), big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_right_text_margin, y=0.2 + height_adjustment, format(sum(tmp_data[['current_data_summaries_quintiles']][['COUNT_GROWTH_OBSERVED']], na.rm=TRUE), big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")

            ### Quintile line demarcations
            tmp_quintile_range_cuts <- tmp_data[['decile_breaks_scale_score_prior_current_data']][c(3,5,7,9)]
            for (line_iter in tmp_quintile_range_cuts){
                grid.lines(x=c(line_iter, line_iter), y=c(0,0.8), gp=gpar(lty=3, col="grey70"), default.units="native")
            }
        popViewport()

        ### bottom_left_figure_right_axis_vp
        pushViewport(bottom_left_figure_right_axis_vp)
            ### Academic Impact Spectrum
            if (is.null(student_group)) {
                tmp_z_score_range <- parameters[[content_area]][['academic_impact_axis_range_BL']]
                tmp_rect_center <- sum(tmp_z_score_range)/2
                tmp_rect_height <- diff(extendrange(tmp_z_score_range, f=0.007))
                tmp_y_coors <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
                tmp_standardized_spectrum_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
            } else {
                tmp_z_score_range <- parameters[[content_area]][['academic_impact_axis_range_BL']]
                tmp_rect_center <- sum(tmp_z_score_range)/2
                tmp_rect_height <- diff(tmp_z_score_range)
                tmp_y_coors <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
                tmp_standardized_spectrum_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
            }
            tmp_spectrum_approx_fun <- approxfun(tmp_y_coors, tmp_standardized_spectrum_scores)
            for (spectrum_line_iter in tmp_y_coors) {
                grid.lines(x=c(0.52, 0.73), y=rep(spectrum_line_iter, 2), gp=gpar(col=getAcademicImpactInfo(tmp_spectrum_approx_fun(spectrum_line_iter))[['Color']], lineend=2, lwd=2.8), default.units="native")
            }
            grid.rect(x=0.625, y=tmp_rect_center, width=0.25, height=tmp_rect_height, gp=gpar(lwd=1.0), default.units="native")
        popViewport()

    } else { ### END if(!is.na(prior_grade)) statement
        pushViewport(growth_left_vp)
            grid.text(x=0.5, y=0.5, paste0("No Growth Data:\n", parameters[['assessment_abb']], " ", content_area_label, "\nGrade ", current_grade), gp=gpar(cex=3))
        popViewport()
    } ### GROWTH VIEWPORT

    ##################################################
    ### STATUS
    ##################################################
    ### top_right_figure_vp
    pushViewport(top_right_figure_vp)
        grid.clip(x=unit(0.5, "npc"), y=unit(0.5, "npc"), width=unit(1, "npc"), height=unit(1, "npc"))
        for (cutscore.iter in tmp_data[['achievement_cutscores_current']][[paste("GRADE", current_grade, sep="_")]]) {
            grid.lines(x=parameters[[content_area]][['x_axis_range_cohort_comparison']], y=c(cutscore.iter, cutscore.iter),
                gp=gpar(col="blue", lty=2, lwd=1.0), default.units="native")
        }
        # grid.polygon(x=c(seq(0.5, 99.5, length=100), rev(seq(0.5, 99.5, length=100))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5,99.5, length=100)))), gp=gpar(fill=status_polygon_fill, lwd=0.0, col=rgb(1, 0, 0, 0.5)), default.units="native") # fill=rgb(1, 0, 0, 0.5)
        # grid.polygon(x=c(seq(0.5,19.5, length=20), rev(seq(0.5,19.5, length=20))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5,19.5, length=20)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5,19.5, length=20)))), gp=gpar(fill=paste0(status_pgon_q1, 80), lwd=0.0, col=status_pgon_q1), default.units="native")
        # grid.polygon(x=c(seq(19.5,39.5, length=20), rev(seq(19.5,39.5, length=20))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(19.5,39.5, length=20)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(19.5,39.5, length=20)))), gp=gpar(fill=paste0(status_pgon_q2, 80), lwd=0.0, col=status_pgon_q2), default.units="native")
        # grid.polygon(x=c(seq(39.5,59.5, length=20), rev(seq(39.5,59.5, length=20))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(39.5,59.5, length=20)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(39.5,59.5, length=20)))), gp=gpar(fill=paste0(status_pgon_q3, 80), lwd=0.0, col=status_pgon_q3), default.units="native")
        # grid.polygon(x=c(seq(59.5,79.5, length=20), rev(seq(59.5,79.5, length=20))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(59.5,79.5, length=20)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(59.5,79.5, length=20)))), gp=gpar(fill=paste0(status_pgon_q4, 80), lwd=0.0, col=status_pgon_q4), default.units="native")
        # grid.polygon(x=c(seq(79.5,99.5, length=20), rev(seq(79.5,99.5, length=20))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(79.5,99.5, length=20)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(79.5,99.5, length=20)))), gp=gpar(fill=paste0(status_pgon_q5, 80), lwd=0.0, col=status_pgon_q5), default.units="native")
        grid.polygon(x=c(c(0.5, seq(1,20, length=20)), rev(c(0.5, seq(1,20, length=20)))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](c(0.5, seq(1,20, length=20))), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](c(0.5, seq(1,20, length=20))))), gp=gpar(fill=status_pgon_q1, lwd=0.0, col=status_pgon_q1), default.units="native")
        grid.polygon(x=c(seq(20,40, length=21), rev(seq(20,40, length=21))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(20,40, length=21)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(20,40, length=21)))), gp=gpar(fill=status_pgon_q2, lwd=0.0, col=status_pgon_q2), default.units="native")
        grid.polygon(x=c(seq(40,60, length=21), rev(seq(40,60, length=21))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(40,60, length=21)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(40,60, length=21)))), gp=gpar(fill=status_pgon_q3, lwd=0.0, col=status_pgon_q3), default.units="native")
        grid.polygon(x=c(seq(60,80, length=21), rev(seq(60,80, length=21))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(60,80, length=21)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(60,80, length=21)))), gp=gpar(fill=status_pgon_q4, lwd=0.0, col=status_pgon_q4), default.units="native")
        grid.polygon(x=c(c(seq(80,99, length=20), 99.5), rev(c(seq(80,99, length=20), 99.5))), y=c(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](c(seq(80,99, length=20), 99.5)), rev(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](c(seq(80,99, length=20), 99.5)))), gp=gpar(fill=status_pgon_q5, lwd=0.0, col=status_pgon_q5), default.units="native")
        grid.lines(x=seq(0.5, 99.5, length=100), y=tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)), gp=gpar(col="red"), default.units="native")
        if (parameters[['include.imputations']])
          grid.lines(x=seq(0.5, 99.5, length=100), y=tmp_data[[paste0('current_data_percentile_cuts_current_score_imputed_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)), gp=gpar(lty=3, col="red"), default.units="native")
        grid.lines(x=seq(0.5, 99.5, length=100), y=tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)), gp=gpar(col="black"), default.units="native")

        ## Legend
        if (parameters[['include.imputations']]) height_adjustment <- 0.00 else height_adjustment <- 0.03
        grid.rect(x=unit(0.02, "npc"), y=unit(0.98, "npc"), width=unit(0.35, "npc"), height=unit(0.18 - height_adjustment, "npc"), just=c("left", "top"), gp=gpar(fill = parameters[['graphic_format']][['colors_background']]))
        grid.lines(x=c(0.04, 0.07), y=c(0.96, 0.96), gp=gpar(lwd=1.2, col="black"))
        grid.text(x=0.08, y=0.96, paste(strtail(parameters[['prior_year']], 4), "Percentiles (Observed)"), just="left", gp=gpar(cex=0.5))
        grid.lines(x=c(0.04, 0.07), y=c(0.93, 0.93), gp=gpar(lwd=1.2, col="red"))
        grid.text(x=0.08, y=0.93, paste(strtail(parameters[['current_year']], 4), "Percentiles (Observed)"), just="left", gp=gpar(cex=0.5))
        if (parameters[['include.imputations']]) {
          grid.lines(x=c(0.04, 0.07), y=c(0.90, 0.90), gp=gpar(lwd=1.2, col="red", lty=3))
          grid.text(x=0.08, y=0.90, paste(strtail(parameters[['current_year']], 4), "Percentiles (+ Imputed)"), just="left", gp=gpar(cex=0.5))
        }
        grid.rect(x=.045, y=0.86 + height_adjustment, width=0.02, height=0.02, gp=gpar(fill=status_polygon_fill, lwd=0.3), just=c("left", "bottom"))
        grid.text(x=0.08, y=0.87 + height_adjustment, paste(strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "Status Impact"), just="left", gp=gpar(cex=0.5))
        grid.lines(x=c(0.04, 0.07), y=c(0.84, 0.84) + height_adjustment, gp=gpar(lwd=1.2, col="red", lty=4))
        grid.lines(x=c(0.04, 0.07), y=c(0.83, 0.83) + height_adjustment, gp=gpar(lwd=1.2, col="black", lty=4))
        grid.text(x=0.08, y=0.835 + height_adjustment, paste(strtail(parameters[['prior_year']], 4), "and", strtail(parameters[['current_year']], 4), "Mean Scaled Scores"), just="left", gp=gpar(cex=0.5))

        ## Percent proficient lines
        prior_data_proficient_x_coor <- which.min(tmp_data[[paste0('prior_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)) <
                                                    tmp_data[['achievement_cutscores_prior']][[paste("GRADE", current_grade, sep="_")]][parameters[['proficient_cutscore_level']]]) - 1
        current_data_proficient_x_coor <- which.min(tmp_data[[paste0('current_data_percentile_cuts_current_score_', Splinefun_Label, '_splinefun')]](seq(0.5, 99.5, length=100)) <
                                                    tmp_data[['achievement_cutscores_prior']][[paste("GRADE", current_grade, sep="_")]][parameters[['proficient_cutscore_level']]]) - 1
        grid.lines(x=c(prior_data_proficient_x_coor, prior_data_proficient_x_coor),
                    y=c(convertY(unit(0.2, "npc"), unitTo="native"), tmp_data[['achievement_cutscores_prior']][[paste("GRADE", current_grade, sep="_")]][parameters[['proficient_cutscore_level']]]),
                    gp=gpar(lwd=0.7, lty=1, col="black"), default.units="native")
        grid.lines(x=c(current_data_proficient_x_coor, current_data_proficient_x_coor),
                    y=c(convertY(unit(0.1, "npc"), unitTo="native"), unit(tmp_data[['achievement_cutscores_prior']][[paste("GRADE", current_grade, sep="_")]][parameters[['proficient_cutscore_level']]], "native")),
                    gp=gpar(lwd=0.7, lty=1, col="red"), default.units="native")
        grid.text(x=unit(100 - tmp_data[['percent_proficient_prior']], "native") - unit(0.01, "npc"), y=unit(parameters[[content_area]][['y_axis_range']][1], "native") + unit(0.2, "npc"), paste0(strtail(parameters[['prior_year']], 4), ": ", format(tmp_data[['percent_proficient_prior']], digits=1, nsmall=1), "% Proficient"),
                gp=gpar(cex=0.8), just=c("right", "center"), default.unit="native")
        grid.text(x=unit(100 - tmp_data[['percent_proficient_current']], "native") - unit(0.01, "npc"), y=unit(parameters[[content_area]][['y_axis_range']][1], "native") + unit(0.1, "npc"), paste0(strtail(parameters[['current_year']], 4), ": ", format(tmp_data[['percent_proficient_current']], digits=1, nsmall=1), "% Proficient"),
                gp=gpar(cex=0.8, col="red"), just=c("right", "center"), default.units="native")

        ## Mean scale score lines
        grid.lines(x=unit(c(0.03, 0.95), "npc"), y=mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE), gp=gpar(col="black", lty=4, lwd=0.9), default.units="native")
        grid.lines(x=unit(c(0.03, 0.95), "npc"), y=mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE), gp=gpar(col="red", lty=4, lwd=0.9), default.units="native")
    popViewport()

    ### top_right_figure_left_axis_vp
    pushViewport(top_right_figure_left_axis_vp)
        grid.text(x=0.2, y=0.5, paste("Grade", current_grade, "Scaled/Standardized Score and Achievement Level Cuts"),
            gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), rot=90, just="center")
        grid.lines(x=c(0.7, 0.7), y=parameters[[content_area]][['y_axis_range']], default.units="native")
        for (tick_iter in parameters[[content_area]][['y_axis_ticks']]) {
            grid.lines(x=c(0.6, 0.7), y=c(tick_iter, tick_iter), default.units="native")
            grid.text(x=0.56, y=tick_iter, tick_iter, gp=gpar(cex=0.5), just="right", default.units="native")
        }

        if (is.null(student_group)){
            tmp_z_score_range <- 0.5*round((parameters[[content_area]][['y_axis_range']] - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)/c(0.5, 0.5))
            tmp_z_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], by=0.5)
            tmp_standardized_scores <- tmp_z_scores*sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) + mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        } else {
            tmp_z_score_range <- 0.5*round((parameters[[content_area]][['y_axis_range']] - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)/c(0.5, 0.5))
            tmp_z_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], by=0.5)
            tmp_standardized_scores <- tmp_z_scores*sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE) + mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        for (tick_iter in which(tmp_standardized_scores > parameters[[content_area]][['y_axis_range']][1] & tmp_standardized_scores < parameters[[content_area]][['y_axis_range']][2])) {
            grid.lines(x=c(0.7, 0.8), y=c(tmp_standardized_scores[tick_iter], tmp_standardized_scores[tick_iter]), default.units="native")
            grid.text(x=0.84, y=tmp_standardized_scores[tick_iter], tmp_z_scores[tick_iter], gp=gpar(cex=0.5), just="left", default.units="native")
        }
    popViewport()

    ### top_right_figure_bottom_axis_vp
    pushViewport(top_right_figure_bottom_axis_vp)

        grid.lines(x=parameters[[content_area]][['x_axis_range_cohort_comparison']], y=c(0.75, 0.75), default.units="native")
        for (tick_iter in parameters[[content_area]][['x_axis_range_cohort_comparison_ticks']]) {
            grid.lines(x=c(tick_iter, tick_iter), y=c(0.67, 0.75), default.units="native")
            grid.text(x=tick_iter, y=0.60, tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
        }

        ## student_group deciles
        if (!is.null(student_group)) {
            overall_percentiles <- unique(data.table(PERCENTILE=seq(100)-0.5, SCALE_SCORE=tmp_data[['percentile_breaks_prior_data_no_subset']]), by="SCALE_SCORE")
            tmp_percentile_function <- approxfun(overall_percentiles[['SCALE_SCORE']], overall_percentiles[['PERCENTILE']])
            tmp_student_group_deciles <- tmp_percentile_function(head(tail(tmp_data[['decile_breaks_prior_data']], -1), -1))

            grid.lines(x=range(tmp_student_group_deciles), y=c(0.83, 0.83), default.units="native")
            for (tick_iter in seq_along(parameters[[content_area]][['x_axis_range_cohort_comparison_ticks']])) {
                grid.lines(x=c(tmp_student_group_deciles[tick_iter], tmp_student_group_deciles[tick_iter]), y=c(0.83, 0.92), default.units="native")
                grid.text(x=tmp_student_group_deciles[tick_iter], y=1.0, 10*tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
            }
        }

        if (!is.null(student_group)) {
            grid.text(x=0.5, y=0.3, paste0("Grade ", current_grade, " Overall and Subgroup Percentile Ranks"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
        } else {
            grid.text(x=0.5, y=0.3, paste0("Grade ", current_grade, " Overall Percentile Ranks"),
                gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
        }
    popViewport()


    ### top_right_figure_top_axis_vp
    pushViewport(top_right_figure_top_axis_vp)
        grid.text(x=0.5, y=0.6, paste(strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Scaled Score"),
            gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
        tmp_scaled_scores_percentiles <- 100*tmp_data[['prior_data_current_score_ecdf']](parameters[[content_area]][['x_axis_range_cohort_comparison_scale_score_ticks']])
        grid.lines(x=range(tmp_scaled_scores_percentiles), y=c(0.3,0.3), default.units="native")
        for (tick_iter in seq_along(tmp_scaled_scores_percentiles)) {
            grid.lines(x=c(tmp_scaled_scores_percentiles[tick_iter], tmp_scaled_scores_percentiles[tick_iter]), y=c(0.3, 0.2), default.units="native")
            grid.text(x=tmp_scaled_scores_percentiles[tick_iter], y=0.12, parameters[[content_area]][['x_axis_range_cohort_comparison_scale_score_ticks']][tick_iter], gp=gpar(cex=0.5), just="center", default.units="native")
        }
    popViewport()

    ### top_right_figure_right_axis_vp
    pushViewport(top_right_figure_right_axis_vp)
    ## Current and Prior Mean Z-scores
        if (is.null(student_group)){
            tmp_z_score_prior_data <- (mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        } else {
            tmp_z_score_prior_data <- (mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        grid.text(x=0.1, y=mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE), format(round(tmp_z_score_prior_data, digits=2), nsmall=2, digits=2), gp=gpar(cex=0.5, col="black"), just="right", default.units="native")
        grid.text(x=0.1, y=mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE), format(round(tmp_z_score_current_data, digits=2), nsmall=2, digits=2), gp=gpar(cex=0.5, col="red"), just="right", default.units="native")
    popViewport()

    ### bottom_right_figure_vp
    pushViewport(bottom_right_figure_vp)
        grid.lines(x=parameters[[content_area]][['x_axis_range_cohort_comparison']], y=c(0,0), default.units="native")
        grid.lines(x=seq(3, 97), y=tmp_data[[paste0('status_percentile_change_', Splinefun_Label)]], gp=gpar(col="red"), default.units="native")
        if (parameters[['include.imputations']])
          grid.lines(x=seq(3, 97), y=tmp_data[[paste0('status_percentile_change_imputed_', Splinefun_Label)]], gp=gpar(col="red", lty=3), default.units="native")

        ## Legend
        if (!is.na(prior_grade)) legend_height <- 0.13 else legend_height <- 0.10
        if (!parameters[['include.imputations']]) legend_height <- legend_height - 0.03
        if (parameters[['include.imputations']]) {
            grid.rect(x=unit(0.02, "npc"), y=unit(0.98, "npc"), width=unit(0.45, "npc"), height=unit(legend_height, "npc"), just=c("left", "top"), gp=gpar(fill = parameters[['graphic_format']][['colors_background']]))
            grid.lines(x=c(0.04, 0.07), y=c(0.95, 0.95), gp=gpar(lwd=1.2, col="red"))
            grid.text(x=0.08, y=0.95, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Status Change (Observed)"), just="left", gp=gpar(cex=0.5))
            grid.lines(x=c(0.04, 0.07), y=c(0.92, 0.92), gp=gpar(lwd=1.2, col="red", lty=3))
            grid.text(x=0.08, y=0.92, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Status Change (+ Imputed)"), just="left", gp=gpar(cex=0.5))
        } else {
            grid.rect(x=unit(0.02, "npc"), y=unit(0.98, "npc"), width=unit(0.45, "npc"), height=unit(legend_height, "npc"), just=c("left", "top"), gp=gpar(fill = parameters[['graphic_format']][['colors_background']]))
            grid.lines(x=c(0.04, 0.07), y=c(0.95, 0.95), gp=gpar(lwd=1.2, col="red"))
            grid.text(x=0.08, y=0.95, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Status Change (Observed)"), just="left", gp=gpar(cex=0.5))
        }
        if (!is.na(prior_grade) && parameters[['include.imputations']]) grid.text(x=0.04, y=0.885, paste0(format(cor(tmp_data[[paste0('status_percentile_change_imputed_', Splinefun_Label)]], tmp_data[['sgp_change_imputed']], use="na.or.complete"), nsmall=2, digits=2), ": Correlation SGP and Status Percentile Change"), just="left", gp=gpar(cex=0.5))
        if (!is.na(prior_grade) && !parameters[['include.imputations']]) grid.text(x=0.04, y=0.905, paste0(format(cor(tmp_data[[paste0('status_percentile_change_', Splinefun_Label)]], tmp_data[['sgp_change']], use="na.or.complete"), nsmall=2, digits=2), ": Correlation SGP and Status Percentile Change"), just="left", gp=gpar(cex=0.5))

        ## Mean scale score lines
        if (is.null(student_group)){
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']])
              tmp_z_score_current_data_imputed <- (mean(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_prior_data <- 0
            tmp_y_coor <- (tmp_z_score_current_data - tmp_z_score_prior_data)*sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        } else {
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            if (parameters[['include.imputations']])
              tmp_z_score_current_data_imputed <- (mean(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_prior_data <- (mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_y_coor <- (tmp_z_score_current_data - tmp_z_score_prior_data)*sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        grid.lines(x=unit(c(0.03, 0.95), "npc"), y=c(tmp_y_coor, tmp_y_coor), gp=gpar(col="red", lty=4, lwd=0.9), default.units="native")
    popViewport()

    ### bottom_right_figure_left_axis_vp
    pushViewport(bottom_right_figure_left_axis_vp)
        grid.text(x=0.2, y=0.5, paste(strtail(parameters[['current_year']], 4), "to", strtail(parameters[['prior_year']], 4), "Scaled/Standardized Score Difference"),
            gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.75), rot=90, just="center")
        grid.lines(x=c(0.7, 0.7), y=parameters[[content_area]][['academic_impact_axis_range_BR']], default.units="native")
        for (tick_iter in parameters[[content_area]][['academic_impact_axis_ticks_BR']]) {
            grid.lines(x=c(0.6, 0.7), y=c(tick_iter, tick_iter), default.units="native")
            grid.text(x=0.56, y=tick_iter, tick_iter, gp=gpar(cex=0.5), just="right", default.units="native")
        }

        if (is.null(student_group)) {
            tmp_z_score_range <- 0.1*round((parameters[[content_area]][['academic_impact_axis_range_BR']])/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)/c(0.1, 0.1))
            tmp_z_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], by=0.05)
            tmp_standardized_scores <- tmp_z_scores*sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        } else {
            tmp_z_score_range <- 0.1*round((parameters[[content_area]][['academic_impact_axis_range_BR']])/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)/c(0.1, 0.1))
            tmp_z_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], by=0.05)
            tmp_standardized_scores <- tmp_z_scores*sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        for (tick_iter in which(tmp_standardized_scores > parameters[[content_area]][['academic_impact_axis_range_BR']][1] & tmp_standardized_scores < parameters[[content_area]][['academic_impact_axis_range_BR']][2])) {
            grid.lines(x=c(0.7, 0.8), y=c(tmp_standardized_scores[tick_iter], tmp_standardized_scores[tick_iter]), default.units="native")
            grid.text(x=0.84, y=tmp_standardized_scores[tick_iter], format(tmp_z_scores[tick_iter], digits=2, nsmall=2), gp=gpar(cex=0.5), just=c("left", "center"), default.units="native")
        }
    popViewport()

    ### bottom_right_figure_bottom_axis_vp
    pushViewport(bottom_right_figure_bottom_axis_vp)
        grid.lines(x=parameters[[content_area]][['x_axis_range_cohort_comparison']], y=c(0.6, 0.6), default.units="native")
        for (tick_iter in parameters[[content_area]][['x_axis_range_cohort_comparison_ticks']]) {
            grid.lines(x=c(tick_iter, tick_iter), y=c(0.5, 0.6), default.units="native")
            grid.text(x=tick_iter, y=0.42, tick_iter, gp=gpar(cex=0.5), just="center", default.units="native")
        }
        tmp_scaled_scores_percentiles <- 100*tmp_data[['prior_data_current_score_ecdf']](parameters[[content_area]][['x_axis_range_cohort_comparison_scale_score_ticks']])
        grid.lines(x=range(tmp_scaled_scores_percentiles), y=c(0.7,0.7), default.units="native")
        for (tick_iter in seq_along(tmp_scaled_scores_percentiles)) {
            grid.lines(x=c(tmp_scaled_scores_percentiles[tick_iter], tmp_scaled_scores_percentiles[tick_iter]), y=c(0.7, 0.8), default.units="native")
            grid.text(x=tmp_scaled_scores_percentiles[tick_iter], y=0.88, parameters[[content_area]][['x_axis_range_cohort_comparison_scale_score_ticks']][tick_iter], gp=gpar(cex=0.5), just="center", default.units="native")
        }
        grid.text(x=0.5, y=0.2, paste0("Grade ", current_grade, " Percentile Ranks and Scaled Scores (", strtail(parameters[['prior_year']], 4), " Norm)"),
            gp=gpar(col=parameters[['graphic_format']][['colors.font']][2], cex=0.8), just="center")
    popViewport()

    ### bottom_right_figure_top_axis_vp
    pushViewport(bottom_right_figure_top_axis_vp)
        if (parameters[['include.imputations']]) height_adjustment <- 0.0 else height_adjustment <- 0.1
        grid.text(x=0.5, y=1.0, paste(strtail(parameters[['prior_year']], 4), "&", strtail(parameters[['current_year']], 4), "Grade", current_grade, "Mean Scaled (Standardized) Scores by Quintile"), gp=gpar(cex=0.8))
        grid.text(x=-3, y=0.7, paste(strtail(parameters[['prior_year']], 4), "Grade", current_grade, "Students"), just="right", gp=gpar(cex=0.5), default.units="native")
        grid.text(x=-3, y=0.6, paste(strtail(parameters[['current_year']], 4), "Grade", current_grade, "Students"), just="right", gp=gpar(cex=0.5), default.units="native")
        grid.text(x=-3, y=0.5, "Change (Observed)", just="right", gp=gpar(cex=0.5), default.units="native")
        if (parameters[['include.imputations']])
          grid.text(x=-3, y=0.4, "Change (+ Imputed)", just="right", gp=gpar(cex=0.5), default.units="native")
        grid.text(x=-3, y=0.3 + height_adjustment, paste("Count Observed", strtail(parameters[['prior_year']], 4)), just="right", gp=gpar(cex=0.5), default.units="native")
        grid.text(x=-3, y=0.2 + height_adjustment, paste("Count Observed", strtail(parameters[['current_year']], 4)), just="right", gp=gpar(cex=0.5), default.units="native")
        tmp_quintile_mean_scaled_score_coordinates <- c(10, 30, 50, 70, 90)
        grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.83, c("Q1", "Q2", "Q3", "Q4", "Q5"), gp=gpar(cex=0.65), default.units="native", just="center")
        if (is.null(student_group)) {
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.7, paste0(format(tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.6, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.5, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            if (parameters[['include.imputations']]) {
              grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.4, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_IMPUTED']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
              " (", format(tmp_data[['current_data_summaries_current_quintiles']][["MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED"]] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            }
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.3 + height_adjustment, format(tmp_data[['prior_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.2 + height_adjustment, format(tmp_data[['current_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
        } else {
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.7, paste0(format(tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_z_score_prior_data, digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.6, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_z_score_current_data, digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.5, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
                " (", format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            if (parameters[['include.imputations']]) {
              grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.4, paste0(format(tmp_data[['current_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_IMPUTED']] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE']], nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]),
              " (", format(tmp_data[['current_data_summaries_current_quintiles']][["MEAN_SCALE_SCORE_IMPUTED_STANDARDIZED"]] - tmp_data[['prior_data_summaries_current_quintiles']][['MEAN_SCALE_SCORE_STANDARDIZED']], digits=0, nsmall=2), ")"),
                gp=gpar(cex=0.5), default.units="native", just="center")
            }
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.3 + height_adjustment, format(tmp_data[['prior_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
            grid.text(x=tmp_quintile_mean_scaled_score_coordinates, y=0.2 + height_adjustment, format(tmp_data[['current_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
        }

        grid.text(x=unit(1.03, "npc"), y=0.83, "Overall", gp=gpar(cex=0.7), just="center", default.units="native")
        grid.text(x=unit(1.03, "npc"), y=0.7, paste0(format(mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE), nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]), " (", format(tmp_z_score_prior_data, digits=0, nsmall=2), ")"),
            gp=gpar(cex=0.5), just="center", default.units="native")
        grid.text(x=unit(1.03, "npc"), y=0.6, paste0(format(mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE), nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]), " (", format(tmp_z_score_current_data, digits=0, nsmall=2), ")"),
            gp=gpar(cex=0.5), just="center", default.units="native")
        grid.text(x=unit(1.03, "npc"), y=0.5, paste0(format(mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE), nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]), " (", format(tmp_z_score_current_data - tmp_z_score_prior_data, digits=0, nsmall=2), ")"),
            gp=gpar(cex=0.5), just="center", default.units="native")
        if (parameters[['include.imputations']])
          grid.text(x=unit(1.03, "npc"), y=0.4, paste0(format(mean(tmp_data[['current_data']][['MEAN_SCALE_SCORE_IMPUTED']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE), nsmall=parameters[['scale_score_round_digits']], digits=parameters[['scale_score_round_digits']]), " (", format(tmp_z_score_current_data_imputed - tmp_z_score_prior_data, digits=0, nsmall=2), ")"),
            gp=gpar(cex=0.5), just="center", default.units="native")
        grid.text(x=unit(1.03, "npc"), y=0.3 + height_adjustment, format(sum(tmp_data[['prior_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], na.rm=TRUE), big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")
        grid.text(x=unit(1.03, "npc"), y=0.2 + height_adjustment, format(sum(tmp_data[['current_data_summaries_current_quintiles']][['COUNT_STATUS_OBSERVED']], na.rm=TRUE), big.mark=",", scientific=FALSE), gp=gpar(cex=0.5), just="center", default.units="native")

        ### Quintile line demarcations
        tmp_quintile_range_cuts <- c(20, 40, 60, 80)
        for (line_iter in tmp_quintile_range_cuts){
            grid.lines(x=c(line_iter, line_iter), y=c(0,0.8), gp=gpar(lty=3, col="grey70"), default.units="native")
        }
    popViewport()

    ### bottom_right_figure_right_axis_vp
    pushViewport(bottom_right_figure_right_axis_vp)
        if (is.null(student_group)){
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_prior_data <- 0
            tmp_sd_current_data <- sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
        } else {
            tmp_z_score_current_data <- (mean(tmp_data[['current_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_z_score_prior_data <- (mean(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE) - mean(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE))/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_sd_current_data <- sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
        }
        grid.text(x=0.1, y=(tmp_z_score_current_data - tmp_z_score_prior_data)*tmp_sd_current_data,
                format(round(tmp_z_score_current_data - tmp_z_score_prior_data, digits=2), nsmall=2, digits=2), gp=gpar(cex=0.5, col="red"), just="right", default.units="native")

        ### Academic Impact Spectrum
        if (is.null(student_group)) {
            tmp_z_score_range <- 0.1*round((parameters[[content_area]][['academic_impact_axis_range_BR']])/sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)/c(0.1, 0.1))
            tmp_rect_center <- sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)*sum(tmp_z_score_range)/2
            tmp_rect_height <- sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)*diff(extendrange(tmp_z_score_range, f=0.007))
            tmp_y_coors <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)*sd(tmp_data[['prior_data']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_standardized_spectrum_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
        } else {
            tmp_z_score_range <- 0.1*round((parameters[[content_area]][['academic_impact_axis_range_BR']])/sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)/c(0.1, 0.1))
            tmp_rect_center <- sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)*sum(tmp_z_score_range)/2
            tmp_rect_height <- sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)*diff(tmp_z_score_range)
            tmp_y_coors <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)*sd(tmp_data[['prior_data_no_subset']][['SCALE_SCORE']], na.rm=TRUE)
            tmp_standardized_spectrum_scores <- seq(tmp_z_score_range[1], tmp_z_score_range[2], length.out=100)
        }
        tmp_spectrum_approx_fun <- approxfun(tmp_y_coors, tmp_standardized_spectrum_scores)
        for (spectrum_line_iter in tmp_y_coors) {
            grid.lines(x=c(0.52, 0.73), y=rep(spectrum_line_iter, 2), gp=gpar(col=getAcademicImpactInfo(tmp_spectrum_approx_fun(spectrum_line_iter), growth_or_status="STATUS")[['Color']], lineend=2, lwd=3.0), default.units="native")
        }
        grid.rect(x=0.625, y=tmp_rect_center, width=0.25, height=tmp_rect_height, gp=gpar(lwd=1.0), default.units="native")
    popViewport()

    ### left_right_figure_sep_vp
    pushViewport(left_right_figure_sep_vp)
            grid.lines(x=c(0.5, 0.5), y=c(0.05,0.95), gp=gpar(lwd=2, col="black"), default.units="native")
    popViewport()


    ### right_overview_figure_sep_vp
    pushViewport(right_overview_figure_sep_vp)
            grid.lines(x=c(0.5, 0.5), y=c(0.05,0.95), gp=gpar(lwd=2, col="black"), default.units="native")
    popViewport()

    ###   XXX
    save(Academic_Impact_Overview, file="../Data/Academic_Impact_Overview.Rdata") # XXX previously in `overview_vp`


    ### Pop figure_vp
    popViewport()

    ### capture viewports as grobs
    if (save_grobs) {
      academic_impact_grob <- grid.grab(wrap=TRUE)
      grob.path <- file.path(tmp_data[['file_path']], "GROBS")
      if (!dir.exists(grob.path)) dir.create(grob.path, recursive=TRUE)
      save(academic_impact_grob, file = file.path(grob.path, gsub(".pdf", ".rda", tmp_data[['file_name']])))
    }

    ### Turn off device
    dev.off()
} ### END covidImpactVisualization
