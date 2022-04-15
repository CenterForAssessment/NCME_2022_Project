##############################################################################
###
### covidImpactOverviewCompilationVisualization_STATE_SPECIFIC function
###
##############################################################################

covidImpactOverviewCompilationVisualization_STATE_SPECIFIC <- function(
    Academic_Impact_Overview_COMPILATION,
    parameters,
    output_type="PDF",
    visualization_type=c("OVERALL", "QUINTILE_1", "QUINTILE_2"),
    Quintile_Cuts="Overall_Cuts",
    academic_impact_metric, ## Hybrid or Status
    file_path=NULL,
    anonymize_overview_groups=TRUE,
    anonymize_overview_group_EXCLUDE=NULL,
    assessment_type, ## State_Assessment or ELP_Assessment
    academic_impact_overview_groups,
    academic_impact_groups="ALL_STUDENTS"
) {
  ###
  set.seed(3.1415)

  ### Checks on parameters
  if (is.null(parameters[["content_area_label"]])) {
    content_area_label <- paste(sapply(parameters[["content_area"]], capwords), collapse=" and ")
  } else {
    content_area_label <- paste(parameters[["content_area_label"]], collapse=" and ")
  }

##   Plot Directory
if (is.null(file_path)) file_path <- file.path("assets", "Rplots", "Impact", parameters[['assessment_type']], toupper(gsub(" ", "_", academic_impact_groups)))
if (!dir.exists(file_path)) dir.create(file_path, recursive=TRUE)

##  Plot file name
state_label <- head(academic_impact_overview_groups, 1)
academic_impact_groups_label <- toupper(gsub(" ", "_", academic_impact_groups))
overall_file_name <- ifelse(academic_impact_metric=="Hybrid", paste("Academic_Impact_Overview_Overall_COMPILATION", state_label, "Hybrid", academic_impact_groups_label, sep="_"), paste("Academic_Impact_Overview_Overall_COMPILATION", academic_impact_groups_label, sep="_"))
quintile_1_file_name <- ifelse(academic_impact_metric=="Hybrid", paste("Academic_Impact_Overview_Quintile_1_COMPILATION", state_label, "Hybrid", academic_impact_groups_label, sep="_"), paste("Academic_Impact_Overview_Quintile_1_COMPILATION", academic_impact_groups_label, sep="_"))
quintile_2_file_name <- ifelse(academic_impact_metric=="Hybrid", paste("Academic_Impact_Overview_Quintile_2_COMPILATION", state_label, "Hybrid", academic_impact_groups_label, sep="_"), paste("Academic_Impact_Overview_Quintile_2_COMPILATION", academic_impact_groups_label, sep="_"))

### Process Academic_Impact_Overview_COMPILATION
Academic_Impact_Overview_COMPILATION_PROCESSED <- list()
for (academic_impact_overview_groups_iter in academic_impact_overview_groups) {
                    Academic_Impact_Overview_COMPILATION_PROCESSED[[academic_impact_overview_groups_iter]] <-
                        filterAcademicImpactOverview(
                                Academic_Impact_Overview=Academic_Impact_Overview_COMPILATION[[academic_impact_overview_groups_iter]],
                                assessment_type=assessment_type,
                                status_grades=Academic_Impact_Overview_COMPILATION[[academic_impact_overview_groups_iter]][[assessment_type]][['parameters']][["STATUS"]],
                                growth_grades=Academic_Impact_Overview_COMPILATION[[academic_impact_overview_groups_iter]][[assessment_type]][['parameters']][["GROWTH"]],
                                academic_impact_metric=academic_impact_metric,
                                use_imputations=Academic_Impact_Overview_COMPILATION[[academic_impact_overview_groups_iter]][[assessment_type]][['parameters']][['include.imputations']],
                                academic_impact_groups=academic_impact_groups)
}

### Anonymize if requested
if (anonymize_overview_groups) {
    tmp_length <- length(Academic_Impact_Overview_COMPILATION_PROCESSED)-length(anonymize_overview_group_EXCLUDE)
    names_to_anonymize <- setdiff(names(Academic_Impact_Overview_COMPILATION_PROCESSED), anonymize_overview_group_EXCLUDE)
    names(Academic_Impact_Overview_COMPILATION_PROCESSED)[match(names_to_anonymize, names(Academic_Impact_Overview_COMPILATION_PROCESSED))] <- paste("State", sample(LETTERS[seq(tmp_length)], tmp_length, replace=FALSE))
    overall_file_name <- paste(overall_file_name, "ANONYMIZED", sep="_")
    quintile_1_file_name <- paste(quintile_1_file_name, "ANONYMIZED", sep="_")
    quintile_2_file_name <- paste(quintile_2_file_name, "ANONYMIZED", sep="_")
}
name_order <- order(names(Academic_Impact_Overview_COMPILATION_PROCESSED))

### Create grid graphic
    ### Layout and viewports
    figure_vp <- viewport(layout=grid.layout(4, 3, widths = unit(c(0.01, 0.98, 0.01)*parameters[['graphic_format']][['fig_width']], rep("inches", 3)),
                      heights=unit(c(0.1, 0.125, 0.75, 0.025)*parameters[['graphic_format']][['fig_height']] +
                                c(0, 0, (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6, 0), rep("inches", 4))),
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
                xscale=c(0,length(unlist(parameters[['grades']]))+3),
                yscale=c(0,length(Academic_Impact_Overview_COMPILATION_PROCESSED)+1.4),
                gp=gpar(fill="transparent"))

if ("OVERALL" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(overall_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(overall_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }


    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste("Multi-State", strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, capwords(academic_impact_groups), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, paste(paste(sapply(content_area_label, capwords), collapse=" and "),  "by Grade"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            CONTENT_AREA=rep(names(parameters[['grades']]), sapply(parameters[['grades']], length)),
            GRADE=as.character(unlist(parameters[['grades']])),
            X_COOR=seq(length(unlist(parameters[['grades']]))) + rep(seq(2, by=0.5, length=length(parameters[['content_area']])), sapply(parameters[['grades']], length))
        )
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+1.0, sapply(names(parameters[['grades']]), capwords), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=2.1), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.5, paste("Grade", unlist(parameters[['grades']])), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.4), just="center", default.units="native")
        row_y_coors <- seq_along(names(Academic_Impact_Overview_COMPILATION_PROCESSED)) - c(rep(0.55, length(names(Academic_Impact_Overview_COMPILATION_PROCESSED))-1), 0.3)
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview_COMPILATION_PROCESSED)[name_order]), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.3, length(Academic_Impact_Overview_COMPILATION_PROCESSED)-1), 2)), just="right", default.units="native")
        if (length(Academic_Impact_Overview_COMPILATION_PROCESSED) > 1) grid.lines(x=c(2.4, tail(x_coors_dt[['X_COOR']], 1)+0.6), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(parameters[['grades']]) > 1) {
            x_coors_content_area_transitions <- head(cumsum(sapply(parameters[['grades']], length)), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        for (row_iter in seq_along(name_order)) {
            meta_data <- getMetaData(Academic_Impact_Overview_COMPILATION_PROCESSED[[name_order[row_iter]]][[assessment_type]][[academic_impact_groups]][['Overall']])
            meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE", "Label"))
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
        grid.text(x=0.125, y=base_key_y_coor + 0.6, "Growth/Status Change Legend", gp=gpar(cex=0.9), just="center")
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

### Visualization by Quintile
if ("QUINTILE_1" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(quintile_1_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(quintile_1_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }


    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste("Multi-State", strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, capwords(academic_impact_groups), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, paste(paste(sapply(content_area_label, capwords), collapse=" and "),  "by Grade & Achievement Quintile"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            CONTENT_AREA=rep(names(parameters[['grades']]), sapply(parameters[['grades']], length)),
            GRADE=as.character(unlist(parameters[['grades']])),
            X_COOR=seq(length(unlist(parameters[['grades']]))) + rep(seq(2, by=0.5, length=length(parameters[['content_area']])), sapply(parameters[['grades']], length))
        )
        x_coors_dt[,"X_COOR_Q1":=X_COOR-0.38][,"X_COOR_Q2":=X_COOR - 0.19][,"X_COOR_Q3":=X_COOR][,"X_COOR_Q4":=X_COOR + 0.19][,"X_COOR_Q5":=X_COOR + 0.38]
        quintile_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+1.0, sapply(names(parameters[['grades']]), capwords), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.9), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.55, paste("Grade", unlist(parameters[['grades']])), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.2), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.25, "Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.0), just="center", default.units="native")
        for (quintile_label_iter in 1:5) {
            grid.text(x=x_coors_dt[[paste0("X_COOR_", quintile_names[quintile_label_iter])]], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED), quintile_label_iter, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=0.9), just="center", default.units="native")
        }
        row_y_coors <- seq_along(names(Academic_Impact_Overview_COMPILATION_PROCESSED)) - c(rep(0.55, length(names(Academic_Impact_Overview_COMPILATION_PROCESSED))-1), 0.3)
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview_COMPILATION_PROCESSED)[name_order]), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.3, length(Academic_Impact_Overview_COMPILATION_PROCESSED)-1), 2)), just="right", default.units="native")
        if (length(Academic_Impact_Overview_COMPILATION_PROCESSED) > 1) grid.lines(x=c(2.4, tail(x_coors_dt[['X_COOR']], 1)+0.6), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(parameters[['grades']]) > 1) {
            x_coors_content_area_transitions <- head(cumsum(sapply(parameters[['grades']], length)), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        for (row_iter in seq_along(name_order)) {
            for (quintile_iter in seq_along(quintile_names)) {
                meta_data <- getMetaData(Academic_Impact_Overview_COMPILATION_PROCESSED[[name_order[row_iter]]][[assessment_type]][[academic_impact_groups]][[quintile_names[quintile_iter]]], Quintile_Cuts=Quintile_Cuts)
                meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE", "Label"))
                setkey(meta_data, X_COOR)
                grid.rect(x=meta_data[[paste0("X_COOR_", quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter], width=0.18, height=0.75, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
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
        grid.text(x=0.125, y=base_key_y_coor + 0.6, "Growth/Status Change Legend", gp=gpar(cex=0.9), just="center")
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
        if (length(Academic_Impact_Overview_COMPILATION_PROCESSED) > 1) grid.text(x=0.445, y=0.5, "NOTE: Quintiles for each\nstate are based upon quintile\ncuts for that state.")
    popViewport() ## legend_vp

    popViewport() ## figure_vp

    ### Turn off device
    dev.off()
} ### END if ("QUINTILE_1" in visualization_type)

### Visualization by Quintile
if ("QUINTILE_2" %in% visualization_type) {
    ### Set up PDF or PNG output
    if (output_type=="PDF") {
        pdf(file=file.path(file_path, paste0(quintile_2_file_name, ".pdf")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']])
    }
    if (output_type=="PNG") {
        png(file=file.path(file_path, paste0(quintile_2_file_name, ".png")),
            width=parameters[['graphic_format']][['fig_width']],
            height=parameters[['graphic_format']][['fig_height']] + (length(Academic_Impact_Overview_COMPILATION_PROCESSED) - 11)*0.6,
            bg=parameters[['graphic_format']][['colors_background']],
            units="in", res=150)
    }

    ## figure_vp
    pushViewport(figure_vp)

    ## title_vp
    pushViewport(title_vp)
        grid.roundrect(width=unit(0.99, "npc"), height=unit(0.85, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=parameters[['graphic_format']][['colors_border']], lwd=1.4))
        grid.text(x=0.01, y=0.5, paste("Multi-State", strtail(parameters[['prior_year']], 4), "to", strtail(parameters[['current_year']], 4), "COVID-19 Academic Impact"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.9), just="left", default.units="native")
        grid.text(x=0.99, y=0.7, capwords(academic_impact_groups), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
        grid.text(x=0.99, y=0.3, paste(paste(sapply(content_area_label, capwords), collapse=" and "),  "by Grade & Achievement Quintile"), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), just="right", default.units="native")
    popViewport() ## title_vp

    ## academic_impact_overview_vp
    pushViewport(academic_impact_overview_vp)
        x_coors_dt <- data.table(
            CONTENT_AREA=rep(names(parameters[['grades']]), sapply(parameters[['grades']], length)),
            GRADE=as.character(unlist(parameters[['grades']])),
            X_COOR=seq(length(unlist(parameters[['grades']]))) + rep(seq(2, by=0.5, length=length(parameters[['content_area']])), sapply(parameters[['grades']], length))
        )
        x_coors_dt[,"X_COOR_Q1":=X_COOR-0.38][,"X_COOR_Q2":=X_COOR - 0.19][,"X_COOR_Q3":=X_COOR][,"X_COOR_Q4":=X_COOR + 0.19][,"X_COOR_Q5":=X_COOR + 0.38]
        quintile_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")
        title_x_coors <- x_coors_dt[,mean(X_COOR), by="CONTENT_AREA"][['V1']]
        grid.text(x=title_x_coors, y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+1.1, sapply(names(parameters[['grades']]), capwords), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.9), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.65, paste("Grade", unlist(parameters[['grades']])), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.2), just="center", default.units="native")
        grid.text(x=x_coors_dt[['X_COOR']], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.35, "Quintile", gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=1.0), just="center", default.units="native")
        for (quintile_label_iter in 1:5) {
            grid.text(x=x_coors_dt[[paste0("X_COOR_", quintile_names[quintile_label_iter])]], y=length(Academic_Impact_Overview_COMPILATION_PROCESSED)+0.1, quintile_label_iter, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=0.9), just="center", default.units="native")
        }
        row_y_coors <- seq_along(names(Academic_Impact_Overview_COMPILATION_PROCESSED)) - c(rep(0.55, length(names(Academic_Impact_Overview_COMPILATION_PROCESSED))-1), 0.3)
        grid.text(x=2.2, y=row_y_coors, as.character(sapply(rev(names(Academic_Impact_Overview_COMPILATION_PROCESSED)[name_order]), capwords)), gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], cex=c(rep(1.3, length(Academic_Impact_Overview_COMPILATION_PROCESSED)-1), 2)), just="right", default.units="native")
        if (length(Academic_Impact_Overview_COMPILATION_PROCESSED) > 1) grid.lines(x=c(2.4, tail(x_coors_dt[['X_COOR']], 1)+0.6), y=mean(tail(row_y_coors, 2)), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
        if (length(parameters[['grades']]) > 1) {
            x_coors_content_area_transitions <- head(cumsum(sapply(parameters[['grades']], length)), -1)
            for (x_coors_content_area_transitions_iter in x_coors_content_area_transitions) {
                grid.lines(x=mean(x_coors_dt[['X_COOR']][c(x_coors_content_area_transitions_iter, x_coors_content_area_transitions_iter+1)]), y=range(row_y_coors) + c(-0.5, 1), gp=gpar(lwd=2.0, col="grey50"), default.units="native")
            }
        }
        ## STATE QUINTILE RESULTS USING OVERALL CUTS
        for (row_iter in seq_along(name_order)) {
            for (quintile_iter in seq_along(quintile_names)) {
                meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview_COMPILATION_PROCESSED[[name_order[row_iter]]][[assessment_type]][[academic_impact_groups]][[quintile_names[quintile_iter]]], Quintile_Cuts="Overall_Cuts")
                meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE"))
                meta_data <- getQuintileBoxInfo(meta_data)
                grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter] + 0.215, width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.4, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
            }
        }

        ## SUBGROUP QUINTILE RESULTS USING SUBGROUP CUTS
        for (row_iter in seq_along(name_order)) {
            for (quintile_iter in seq_along(quintile_names)) {
                meta_data <- getMetaData(tmp_meta_data=Academic_Impact_Overview_COMPILATION_PROCESSED[[name_order[row_iter]]][[assessment_type]][[academic_impact_groups]][[quintile_names[quintile_iter]]], Quintile_Cuts="Subgroup_Cuts")
                meta_data <- na.omit(meta_data[x_coors_dt, on=c("CONTENT_AREA", "GRADE")], cols=c("CONTENT_AREA", "GRADE"))
                meta_data <- getQuintileBoxInfo(meta_data)
                grid.rect(x=meta_data[[paste0('X_COOR_', quintile_names[quintile_iter])]], y=rev(row_y_coors)[row_iter] - 0.215, width=meta_data[[paste0('BOX_WIDTH_', quintile_names[quintile_iter])]], height=0.4, gp=gpar(col=parameters[['graphic_format']][['colors_font']][1], fill=meta_data[['Color']], lwd=0.2), default.units="native")
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
        grid.text(x=0.125, y=base_key_y_coor + 0.6, "Growth/Status Change Legend", gp=gpar(cex=0.9), just="center")
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
        if (length(Academic_Impact_Overview_COMPILATION_PROCESSED) > 1) grid.text(x=0.445, y=0.5, "NOTE: Quintiles for each\nstate are based upon quintile\ncuts for that state.")
    popViewport() ## legend_vp

    popViewport() ## figure_vp

    ### Turn off device
    dev.off()
} ### END if ("QUINTILE_2" in visualization_type)



} ### END covidImpactOverviewCompilationVisualization_STATE_SPECIFIC Visualization function
