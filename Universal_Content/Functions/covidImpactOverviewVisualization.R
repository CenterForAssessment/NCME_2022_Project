##############################################################################
###
### covidImpactOverviewVisualization function
###
##############################################################################

covidImpactOverviewVisualization <- function(
    Academic_Impact_Overview,
    parameters,
    output_type="PDF",
    visualization_type=c("OVERALL", "QUINTILE_1", "QUINTILE_2"),
    Quintile_Cuts="Overall_Cuts",
    file_path=NULL,
    academic_impact_metric="Hybrid",  ## Hybrid or Status
    academic_impact_groups="ALL_STUDENTS"
) {


## Checks on parameters
if (is.null(parameters[["content_area_label"]])) {
    content_area_label <- paste(sapply(parameters[["content_area"]], capwords), collapse=" and ")
    content_area_label_column_headers <- sapply(names(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']]), capwords)
} else {
    content_area_label <- paste(parameters[["content_area_label"]], collapse=" and ")
    content_area_label_column_headers <- as.character(unlist(parameters[['content_area_label']]))
}

##   Plot Directory
if (is.null(file_path)) {
  file_path <- file.path("assets", "Rplots", "Impact", parameters[['assessment_type']])
}
if (!dir.exists(file_path)) dir.create(file_path, recursive=TRUE)

##  Plot file name
overall_file_name <- ifelse(academic_impact_metric=="Hybrid", "Academic_Impact_Overview_Overall_Hybrid", "Academic_Impact_Overview_Overall")
quintile_1_file_name <- ifelse(academic_impact_metric=="Hybrid", "Academic_Impact_Overview_Quintile_1_Hybrid", "Academic_Impact_Overview_1_Quintile")
quintile_2_file_name <- ifelse(academic_impact_metric=="Hybrid", "Academic_Impact_Overview_Quintile_2_Hybrid", "Academic_Impact_Overview_2_Quintile")


##   Filter Academic_Impact_Value
Academic_Impact_Overview <- filterAcademicImpactOverview(
                                                        Academic_Impact_Overview,
                                                        assessment_type=parameters[['assessment_type']],
                                                        status_grades=parameters[['current_grade']],
                                                        growth_grades=parameters[['current_grade']][!is.na(parameters[['prior_grade']])],
                                                        academic_impact_metric=academic_impact_metric,
                                                        use_imputations=parameters[['include.imputations']],
                                                        academic_impact_groups=academic_impact_groups)

### Create grid graphic
    ### Layout and viewports
    figure_vp <- viewport(layout=grid.layout(4, 3, widths = unit(c(0.01, 0.98, 0.01)*parameters[['graphic_format']][['fig_width']], rep("inches", 3)),
                      heights=unit(c(0.1, 0.125, 0.75, 0.025)*parameters[['graphic_format']][['fig_height']] +
                                c(0, 0, (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6, 0), rep("inches", 4))),
                      gp=gpar(cex=parameters[['graphic_format']][['fig_width']]/17))

    title_vp <- viewport(name="title_vp",
               layout.pos.row=1, layout.pos.col=1:3,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))

    legend_vp <- viewport(name="title_vp",
               layout.pos.row=2, layout.pos.col=1:3,
               xscale=c(0,1),
               yscale=c(0,1),
               gp=gpar(fill="transparent"))


    academic_impact_overview_vp <- viewport(name="academic_impact_overview_vp",
                layout.pos.row=3, layout.pos.col=2,
                xscale=c(0,sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))+3),
                yscale=c(0,length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+1.4),
                gp=gpar(fill="transparent"))

### Visualization Overall by Grade and Content Area
if ("OVERALL" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(overall_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(overall_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }


    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste(getStateAbbreviation(parameters[['state_abb']], type="LONG"), strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, paste(parameters[['assessment_abb']], content_area_label), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, "Grade & Content Area", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            X_COOR=seq(sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))) + rep(seq(2, by=0.5, length=length(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)),
            GRADE=unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names)),
            CONTENT_AREA=rep(names(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))
        )
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+1.0, content_area_label_column_headers, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=2.1), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+0.5, paste("Grade", unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names))), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.4), just="center", default.units="native")
        row_y_coors <- seq_along(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) - c(rep(0.25, length(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) -1), 0) - 0.3
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.0, length(Academic_Impact_Overview[[parameters[['assessment_type']]]])-1), 1.4)), just="right", default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) grid.lines(x=c(2.4, sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))+3.05), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']]) > 1) {
            x_coors_content_area_transitions <- head(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        for (row_iter in seq_along(names(Academic_Impact_Overview[[parameters[['assessment_type']]]]))) {
            meta_data <- getMetaData(Academic_Impact_Overview[[parameters[['assessment_type']]]][[row_iter]][['Overall']])
            meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE", "Label")) ### Need these variables to remove CONTENT_AREA x GRADE combinations with no academic impact
            setkey(meta_data, X_COOR)
            grid.rect(x=meta_data[['X_COOR']], y=rev(row_y_coors)[row_iter], width=0.95, height=0.75, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']]), default.units="native")
            grid.text(x=meta_data[['X_COOR']], y=rev(row_y_coors)[row_iter], meta_data[['Label']], gp=gpar(col=meta_data[['Label_Color']], cex=1.7*meta_data[['cex']]), default.units="native")
        }
    popViewport() ## academic_impact_overview_vp

    ## legend_vp
    pushViewport(legend_vp)
        base_key_y_coor <- 0.275
        spectrum_sequence <- seq(-40, 20, length=95)
        spectrum_cuts_growth <- c(-40, -25, -15, -5, 5, 20)
        spectrum_cuts_status <- c(-0.55, -0.4, -0.25, -0.1, 0.05, 0.2)
        spectrum_approx_fun <- approxfun(spectrum_sequence, seq(0.025, 0.225, length.out=length(spectrum_sequence)))
        spectrum_labels <- c("Severe", "Large", "Moderate", "Modest\nto\nNone", "Improvement")
        spectrum_labels_cex <- c(0.65, 0.65, 0.65, 0.55, 0.65)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.99, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        for (spectrum_iter in seq_along(spectrum_sequence)-1) {
            grid.lines(x=rep((0.026+spectrum_iter/475), 2), y=c(base_key_y_coor-0.185, base_key_y_coor+0.185), gp=gpar(lwd=3.55, lineend=2, col=getAcademicImpactInfo(spectrum_sequence[spectrum_iter+1])[['Color']]))
        }
        grid.rect(x=0.125, y=base_key_y_coor, width=0.2, height=0.4)
        grid.lines(x=c(0.025, 0.225), y=rep(base_key_y_coor + 0.355, 2), gp=gpar(lwd=1.0))
        grid.text(x=0.02, y=base_key_y_coor + 0.55, "Impact", gp=gpar(cex=0.9), just="left")
        grid.text(x=0.23, y=base_key_y_coor + 0.55, "Recovery", gp=gpar(cex=0.9), just="right")
        grid.text(x=0.233, y=base_key_y_coor + 0.43, "Growth Change", gp=gpar(cex=0.6), just="left")
        grid.text(x=0.233, y=base_key_y_coor + 0.28, "Status Change", gp=gpar(cex=0.6), just="left")
        for (spectrum_cuts_iter in seq_along(spectrum_cuts_growth)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_cuts_growth[spectrum_cuts_iter])
            grid.lines(x=c(tmp_x_coor, tmp_x_coor), y=base_key_y_coor+c(0.325, 0.385), gp=gpar(lwd=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.43, spectrum_cuts_growth[spectrum_cuts_iter], gp=gpar(cex=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.28, spectrum_cuts_status[spectrum_cuts_iter], gp=gpar(cex=0.5))
        }
        spectrum_label_values <- (head(spectrum_cuts_growth, -1) + tail(spectrum_cuts_growth, -1))/2
        for (spectrum_cuts_iter in seq_along(spectrum_label_values)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_label_values[spectrum_cuts_iter])
            grid.text(x=tmp_x_coor, y=base_key_y_coor, spectrum_labels[spectrum_cuts_iter], gp=gpar(col="white", cex=spectrum_labels_cex[spectrum_cuts_iter]))
        }
        grid.rect(x=0.235, y=base_key_y_coor, width=0.01, height=0.4)
        grid.text(x=0.245, y=base_key_y_coor, "Calculations suppressed\ndue to small group N (< 50)", just="left", gp=gpar(cex=0.8))
    popViewport() ## legend_vp

    popViewport() ## figure_vp

    ### Turn off device
    dev.off()
} ### END if ("OVERALL" in visualization_type)

### Visualization by Quintile by Grade and Content Area (quintiles based upon cuts defined by argument Quintile_Cuts)
if ("QUINTILE_1" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(quintile_1_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(quintile_1_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }


    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste(getStateAbbreviation(parameters[['state_abb']], type="LONG"), strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, paste(sapply(content_area_label, capwords), collapse=" and "), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, "Grade, Content Area & Achievement Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            X_COOR=seq(sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))) + rep(seq(2, by=0.5, length=length(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)),
            GRADE=unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names)),
            CONTENT_AREA=rep(names(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))
        )
        x_coor_offset <- c(-0.38, -0.19, 0.0, 0.19, 0.38)
        col_x_coors_quintiles <- unlist(lapply(x_coors_dt[['X_COOR']], function(x) x + x_coor_offset))
        quintile_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+1.0, content_area_label_column_headers, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.9), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+0.55, paste("Grade", unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names))), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.2), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+0.25, "Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.0), just="center", default.units="native")
        grid.text(x=col_x_coors_quintiles, y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]]), rep(1:5, length(col_x_coors_quintiles)/5), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=0.9), just="center", default.units="native")
        row_y_coors <- seq_along(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) - c(rep(0.25, length(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) -1), 0) - 0.5
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.0, length(Academic_Impact_Overview[[parameters[['assessment_type']]]])-1), 1.4)), just="right", default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) grid.lines(x=c(2.4, sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))+3.05), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']]) > 1) {
            x_coors_content_area_transitions <- head(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        for (row_iter in seq_along(names(Academic_Impact_Overview[[parameters[['assessment_type']]]]))) {
            for (quintile_iter in seq_along(quintile_names)) {
                meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview[[parameters[['assessment_type']]]][[row_iter]][[quintile_names[quintile_iter]]], Quintile_Cuts=Quintile_Cuts)
                meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE", "Label"))
                meta_data <- getQuintileBoxInfo(meta_data)
                grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter], width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.75, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
            }
        }
    popViewport() ## academic_impact_overview_vp

    ## legend_vp
    pushViewport(legend_vp)
        base_key_y_coor <- 0.275
        spectrum_sequence <- seq(-40, 20, length=95)
        spectrum_cuts_growth <- c(-40, -25, -15, -5, 5, 20)
        spectrum_cuts_status <- c(-0.55, -0.4, -0.25, -0.1, 0.05, 0.2)
        spectrum_approx_fun <- approxfun(spectrum_sequence, seq(0.025, 0.225, length.out=length(spectrum_sequence)))
        spectrum_labels <- c("Severe", "Large", "Moderate", "Modest\nto\nNone", "Improvement")
        spectrum_labels_cex <- c(0.65, 0.65, 0.65, 0.55, 0.65)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.99, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        for (spectrum_iter in seq_along(spectrum_sequence)-1) {
            grid.lines(x=rep((0.026+spectrum_iter/475), 2), y=c(base_key_y_coor-0.185, base_key_y_coor+0.185), gp=gpar(lwd=3.55, lineend=2, col=getAcademicImpactInfo(spectrum_sequence[spectrum_iter+1])[['Color']]))
        }
        grid.rect(x=0.125, y=base_key_y_coor, width=0.2, height=0.4)
        grid.lines(x=c(0.025, 0.225), y=rep(base_key_y_coor + 0.355, 2), gp=gpar(lwd=1.0))
        grid.text(x=0.02, y=base_key_y_coor + 0.55, "Impact", gp=gpar(cex=0.9), just="left")
        grid.text(x=0.23, y=base_key_y_coor + 0.55, "Recovery", gp=gpar(cex=0.9), just="right")
        grid.text(x=0.233, y=base_key_y_coor + 0.43, "Growth Change", gp=gpar(cex=0.6), just="left")
        grid.text(x=0.233, y=base_key_y_coor + 0.28, "Status Change", gp=gpar(cex=0.6), just="left")
        for (spectrum_cuts_iter in seq_along(spectrum_cuts_growth)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_cuts_growth[spectrum_cuts_iter])
            grid.lines(x=c(tmp_x_coor, tmp_x_coor), y=base_key_y_coor+c(0.325, 0.385), gp=gpar(lwd=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.43, spectrum_cuts_growth[spectrum_cuts_iter], gp=gpar(cex=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.28, spectrum_cuts_status[spectrum_cuts_iter], gp=gpar(cex=0.5))
        }
        spectrum_label_values <- (head(spectrum_cuts_growth, -1) + tail(spectrum_cuts_growth, -1))/2
        for (spectrum_cuts_iter in seq_along(spectrum_label_values)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_label_values[spectrum_cuts_iter])
            grid.text(x=tmp_x_coor, y=base_key_y_coor, spectrum_labels[spectrum_cuts_iter], gp=gpar(col="white", cex=spectrum_labels_cex[spectrum_cuts_iter]))
        }
        grid.rect(x=0.235, y=base_key_y_coor, width=0.01, height=0.4)
        grid.text(x=0.245, y=base_key_y_coor, "Calculations suppressed\ndue to small group N (< 50)", just="left", gp=gpar(cex=0.8))

        grid.lines(x=c(0.36, 0.36), y=c(0.1, 0.9), gp=gpar(col="grey30"))
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) grid.text(x=0.445, y=0.5, "NOTE: Quintiles for each student\nsubgroup are based upon quintile\ncuts defined for All Students.\nBox width indicates group proportion.")
    popViewport() ## legend_vp

    popViewport() ## figure_vp

    ### Turn off device
    dev.off()
} ### END if ("QUINTILE_1" in visualization_type)


### Visualization by Quintile by Grade and Content Area (quintiles based upon BOTH overall and subgroup Quintile_Cuts)
if ("QUINTILE_2" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(quintile_2_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(quintile_2_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }


    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste(getStateAbbreviation(parameters[['state_abb']], type="LONG"), strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, paste(sapply(content_area_label, capwords), collapse=" and "), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, "Grade, Content Area & Achievement Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            X_COOR=seq(sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))) + rep(seq(2, by=0.5, length=length(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)),
            GRADE=unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names)),
            CONTENT_AREA=rep(names(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length)), sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))
        )
        x_coor_offset <- c(-0.38, -0.19, 0.0, 0.19, 0.38)
        col_x_coors_quintiles <- unlist(lapply(x_coors_dt[['X_COOR']], function(x) x + x_coor_offset))
        quintile_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+1.0, content_area_label_column_headers, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.9), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+0.55, paste("Grade", unlist(lapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], names))), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.2), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]])+0.25, "Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.0), just="center", default.units="native")
        grid.text(x=col_x_coors_quintiles, y=length(Academic_Impact_Overview[[parameters[['assessment_type']]]]), rep(1:5, length(col_x_coors_quintiles)/5), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=0.9), just="center", default.units="native")
        row_y_coors <- seq_along(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) - c(rep(0.25, length(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])) -1), 0) - 0.5
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview[[parameters[['assessment_type']]]])), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.0, length(Academic_Impact_Overview[[parameters[['assessment_type']]]])-1), 1.4)), just="right", default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) grid.lines(x=c(2.4, sum(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length))+3.05), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']]) > 1) {
            x_coors_content_area_transitions <- head(sapply(Academic_Impact_Overview[[parameters[['assessment_type']]]][['ALL_STUDENTS']][['Overall']], length), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        ## ALL STUDENTS
        for (row_iter in 1) {
            for (quintile_iter in seq_along(quintile_names)) {
                meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview[[parameters[['assessment_type']]]][[row_iter]][[quintile_names[quintile_iter]]], Quintile_Cuts="Overall_Cuts")
                meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE", "Label"))
                meta_data <- getQuintileBoxInfo(meta_data)
                grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter], width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.75, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
            }
        }
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) {
            ## SUBGROUP QUINTILE RESULTS USING OVERALL CUTS
            for (row_iter in seq(from=2, to=length(Academic_Impact_Overview[[parameters[['assessment_type']]]]))) {
                for (quintile_iter in seq_along(quintile_names)) {
                    meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview[[parameters[['assessment_type']]]][[row_iter]][[quintile_names[quintile_iter]]], Quintile_Cuts="Overall_Cuts")
                    meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE"))
                    meta_data <- getQuintileBoxInfo(meta_data)
                    grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter] + 0.215, width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.4, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
                }
            }
            ## SUBGROUP QUINTILE RESULTS USING SUBGROUP CUTS
            for (row_iter in seq(from=2, to=length(Academic_Impact_Overview[[parameters[['assessment_type']]]]))) {
                for (quintile_iter in seq_along(quintile_names)) {
                    meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview[[parameters[['assessment_type']]]][[row_iter]][[quintile_names[quintile_iter]]], Quintile_Cuts="Subgroup_Cuts")
                    meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE"))
                    meta_data <- getQuintileBoxInfo(meta_data)
                    grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter] - 0.215, width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.4, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
                }
            }
        } ### END SUBGROUP QUINTILE RESULTS
    popViewport() ## academic_impact_overview_vp

    ## legend_vp
    pushViewport(legend_vp)
        base_key_y_coor <- 0.275
        spectrum_sequence <- seq(-40, 20, length=95)
        spectrum_cuts_growth <- c(-40, -25, -15, -5, 5, 20)
        spectrum_cuts_status <- c(-0.55, -0.4, -0.25, -0.1, 0.05, 0.2)
        spectrum_approx_fun <- approxfun(spectrum_sequence, seq(0.025, 0.225, length.out=length(spectrum_sequence)))
        spectrum_labels <- c("Severe", "Large", "Moderate", "Modest\nto\nNone", "Improvement")
        spectrum_labels_cex <- c(0.65, 0.65, 0.65, 0.55, 0.65)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.99, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        for (spectrum_iter in seq_along(spectrum_sequence)-1) {
            grid.lines(x=rep((0.026+spectrum_iter/475), 2), y=c(base_key_y_coor-0.185, base_key_y_coor+0.185), gp=gpar(lwd=3.55, lineend=2, col=getAcademicImpactInfo(spectrum_sequence[spectrum_iter+1])[['Color']]))
        }
        grid.rect(x=0.125, y=base_key_y_coor, width=0.2, height=0.4)
        grid.lines(x=c(0.025, 0.225), y=rep(base_key_y_coor + 0.355, 2), gp=gpar(lwd=1.0))
        grid.text(x=0.02, y=base_key_y_coor + 0.55, "Impact", gp=gpar(cex=0.9), just="left")
        grid.text(x=0.23, y=base_key_y_coor + 0.55, "Recovery", gp=gpar(cex=0.9), just="right")
        grid.text(x=0.233, y=base_key_y_coor + 0.43, "Growth Change", gp=gpar(cex=0.6), just="left")
        grid.text(x=0.233, y=base_key_y_coor + 0.28, "Status Change", gp=gpar(cex=0.6), just="left")
        for (spectrum_cuts_iter in seq_along(spectrum_cuts_growth)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_cuts_growth[spectrum_cuts_iter])
            grid.lines(x=c(tmp_x_coor, tmp_x_coor), y=base_key_y_coor+c(0.325, 0.385), gp=gpar(lwd=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.43, spectrum_cuts_growth[spectrum_cuts_iter], gp=gpar(cex=0.5))
            grid.text(x=tmp_x_coor, y=base_key_y_coor + 0.28, spectrum_cuts_status[spectrum_cuts_iter], gp=gpar(cex=0.5))
        }
        spectrum_label_values <- (head(spectrum_cuts_growth, -1) + tail(spectrum_cuts_growth, -1))/2
        for (spectrum_cuts_iter in seq_along(spectrum_label_values)) {
            tmp_x_coor <- spectrum_approx_fun(spectrum_label_values[spectrum_cuts_iter])
            grid.text(x=tmp_x_coor, y=base_key_y_coor, spectrum_labels[spectrum_cuts_iter], gp=gpar(col="white", cex=spectrum_labels_cex[spectrum_cuts_iter]))
        }
        grid.rect(x=0.235, y=base_key_y_coor, width=0.01, height=0.4)
        grid.text(x=0.245, y=base_key_y_coor, "Calculations suppressed\ndue to small group N (< 50)", just="left", gp=gpar(cex=0.8))

        grid.lines(x=c(0.36, 0.36), y=c(0.1, 0.9), gp=gpar(col="grey30"))
        if (length(Academic_Impact_Overview[[parameters[['assessment_type']]]]) > 1) grid.text(x=0.445, y=0.5, "NOTE: Quintiles for each student\nsubgroup are based upon quintile\ncuts defined for All Students.\nBox width indicates group proportion.")
    popViewport() ## legend_vp

    popViewport() ## figure_vp

    ### Turn off device
    dev.off()
} ### END if ("QUINTILE_2" in visualization_type)

} ### END covidImpactOverviewVisualization function
