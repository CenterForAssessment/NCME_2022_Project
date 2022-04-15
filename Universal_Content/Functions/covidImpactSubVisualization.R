##############################################################################
###
### covidImpactSubVisualization function
###
##############################################################################

covidImpactSubVisualization <- function(
    grob,                 #  actual grob or path to it
    subplot.types,        #  e.g. "no_title", "top_growth" - can be multiple
    content_area = NULL,  #  content_area.iter
    grade = NULL,         #  parameters[['current_grade']][grade.iter]
    parent_width = 17,    #  parameters[['graphic_format']][['fig_width']]
    parent_height = 11,   #  parameters[['graphic_format']][['fig_height']]
    output_path = NULL,
    output_name = NULL,    #  Should NOT have file extension - all svgz output for now
    output_compressed = TRUE # .svg vs .svgz (for now)
  ) {

  ### if `grob` is sent in as a file path, load object and use the name as `output_name`
  if (is.character(grob)) {
    if (file.exists(grob)) {
        load(grob)
        if (!exists("academic_impact_grob")) stop("\n\n\tGROB must be named 'academic_impact_grob'.\n\n")
    } else stop(paste0("\n\n\tFile in path to GROB, '", grob, "', does not exist.\n\n"))
    if (is.null(output_name)) output_name <- gsub(".rda|.Rdata", "", basename(grob))
    output_name <- gsub(".svg|.svgz|.pdf|.png", "", output_name)
    if (is.null(output_path)) output_path <- dirname(grob)
    if (is.null(content_area)) content_area <- gsub("Academic_Impact_|_Grade_[[:alnum:]]+", "", output_name)
    if (is.null(grade)) grade <- tail(strsplit(output_name, "_")[[1]], 1)
  }

  if (!is.null(output_name)) {
    output_name <- gsub(".svg|.svgz|.pdf|.png", "", output_name)
  } else output_name <- paste0("Academic_Impact_", content_area, "_Grade_", grade)

  if (is.null(output_path)) {
    output_path <- getwd()
  } else {
    if (!dir.exists(output_path)) dir.create(output_path, recursive=TRUE)
  }

  if (output_compressed) f.ext <- ".svgz" else f.ext <- ".svg"

  ### Create grid graphic (modified or non-saved) layouts/viewports
  top_plots_vp <- viewport(name = "top_plots",
            layout = grid.layout(nrow = 3, ncol = 3,
              widths = unit(c(0.05, 0.3, 0.04)*parent_width, rep("inches", 3)),
              heights = unit(c(0.065, 0.315, 0.05)*parent_height, rep("inches", 3))),
            gp=gpar(cex=parent_width/parent_width))

  ##  Need to write out `figure_vp` ? - probably not: academic_impact_grob$children[[1]]$list$vp$layout
  ##  Yes! Spread out the title_vp height to the top_plots and SGP table
  figure_vp <- viewport(name = "figure_vp",
            layout = grid.layout(9, 9,
              # widths = unit(c(0.05, 0.3, 0.04, 0.01, 0.06, 0.3, 0.04, 0.01, 0.19)*parent_width, rep("inches", 9)),
              # heights = unit(c(0.09, 0.025, 0.065, 0.315, 0.05, 0.02, 0.11, 0.25, 0.075)*parent_height, rep("inches", 9))),
              widths = unit(c(0.05, 0.3, 0.04, 0.01, 0.06, 0.3, 0.04, 0.01, 0.19)*parent_width, rep("inches", 9)),
              # heights = unit(c(0.0, 0.025, 0.065, 0.4, 0.05, 0.02, 0.115, 0.25, 0.075)*parent_height, rep("inches", 9))),
              heights = unit(c(0.0, 0.025, 0.065, 0.34, 0.05, 0.02, 0.175, 0.25, 0.075)*parent_height, rep("inches", 9))),
            gp=gpar(cex=parent_width/parent_height))


  ###   Utility Functions
  makeVP <- function(vp, row.change=NULL, col.change=NULL){
    if (!is.null(row.change)) {
      vp$layout.pos.row <- vp$layout.pos.row + as.integer(row.change)
      vp$valid.pos.row <- vp$valid.pos.row + as.integer(row.change)
    }
    if (!is.null(col.change)) {
      vp$layout.pos.col <- vp$layout.pos.col + as.integer(col.change)
      vp$valid.pos.col <- vp$valid.pos.col + as.integer(col.change)
    }
    return(vp)
  }

  ###   Output various subplot types

  ####  Modified SVG without title_vp
  if ("no_title" %in% subplot.types) {
    childlist <- names(academic_impact_grob$children)
    wrapped.grobs <- grep("recordedGrob", childlist); grob.names <- childlist[wrapped.grobs]

    figure.vp.name  <- academic_impact_grob$children[[1]]$list$vp$name
    wrapped.grobs <- wrapped.grobs[-c(1:3, length(wrapped.grobs))]
    grob.names <- grob.names[-c(1:3, length(wrapped.grobs))]

    target.vprts <- unlist(lapply(academic_impact_grob$children[wrapped.grobs], function(f) as.character(f[["list"]][["vp"]][["name"]])))
    target.grobs <- names(target.vprts)

    target.index <- which(childlist %in% target.grobs)
    target.end <- which(childlist == grob.names[which(grob.names == tail(target.grobs, 1))+1])

    target.gList <- academic_impact_grob$children[target.index[1]:target.end]

    vprts.to.rm <- target.vprts[target.vprts %in% ls()]
    rm(list=vprts.to.rm); gc()

    for (v in seq_along(target.gList)) {
      if ("list" %in% names(target.gList[[v]])) {
        current.vp <- target.gList[[v]][["list"]][["vp"]]
        if (!is.null(current.vp))
          assign(current.vp[["name"]], makeVP(current.vp))
        current.vp <- current.vp[["name"]]
        next
      }
      if (!is.null(current.vp))
        target.gList[[v]][["vp"]] <- get(current.vp)
    }

    target.gList <- target.gList[grep("recordedGrob", names(target.gList), invert=TRUE)]

    tbl.chunks <- names(unlist(lapply(target.gList, function(f) grep("bottom_left_figure_top_axis_vp|bottom_right_figure_top_axis_vp", as.character(f$vp$name)))))
    for (cnk in grep(".text.", tbl.chunks, value=TRUE)) {
      if (target.gList[[cnk]]$gp$cex == 0.5) {
        target.gList[[cnk]]$gp$cex <- 0.55
      }
      if (target.gList[[cnk]]$gp$cex == 0.65) {
        target.gList[[cnk]]$gp$cex <- 0.7
      }
    }

    lgnd.chunks <- names(unlist(lapply(target.gList, function(f)
        grep("top_left_figure_vp|bottom_left_figure_vp|top_right_figure_vp|bottom_right_figure_vp", as.character(f$vp$name)))))
    for (cnk in grep(".text.", lgnd.chunks, value=TRUE)) {
      if (target.gList[[cnk]]$gp$cex == 0.5) {
        target.gList[[cnk]]$gp$cex <- 0.525
      }
    }

    axis.chunks <- names(unlist(lapply(target.gList, function(f)
        grep("top_left_figure_left_axis_vp|top_left_figure_bottom_axis_vp|top_right_figure_left_axis_vp|top_right_figure_bottom_axis_vp|bottom_left_figure_left_axis_vp|bottom_left_figure_bottom_axis_vp|bottom_right_figure_bottom_axis_vp|bottom_right_figure_left_axis_vp", as.character(f$vp$name)))))
    for (cnk in grep(".text.", axis.chunks, value=TRUE)) {
      if (target.gList[[cnk]]$gp$cex == 0.5) {
        target.gList[[cnk]]$gp$cex <- 0.6
      }
    }

    brl.axis.chunks <- names(unlist(lapply(target.gList, function(f)
        grep("bottom_right_figure_left_axis_vp", as.character(f$vp$name)))))
    for (cnk in grep(".text.", brl.axis.chunks, value=TRUE)) {
      if ("left" %in% target.gList[[cnk]]$just) {
        lbl <- as.numeric(target.gList[[cnk]]$label)
        if (!is.na(lbl)) {
          if ((lbl/0.01) %% 2 == 1) {
            target.gList[[cnk]]$label <- ""
          } else {
            target.gList[[cnk]]$label <- as.character(lbl)
          }
        }
      }
    }

    if (exists("growth_left_vp")) {  #  only present in STATUS ONLY plots
      components <- vpList(
                      subtitle_middle_vp, subtitle_right_vp, growth_left_vp,
                      top_right_figure_vp, top_right_figure_left_axis_vp, top_right_figure_bottom_axis_vp, top_right_figure_top_axis_vp, top_right_figure_right_axis_vp,
                      bottom_right_figure_vp, bottom_right_figure_left_axis_vp, bottom_right_figure_bottom_axis_vp, bottom_right_figure_top_axis_vp, bottom_right_figure_right_axis_vp,
                      left_right_figure_sep_vp, right_overview_figure_sep_vp, overview_vp
      )
    } else {  #  GROWTH & STATUS plots
      components <- vpList(
                      subtitle_left_vp, subtitle_middle_vp, subtitle_right_vp,
                      top_left_figure_vp, top_left_figure_left_axis_vp, top_left_figure_bottom_axis_vp, top_left_figure_top_axis_vp, top_left_figure_right_axis_vp,
                      bottom_left_figure_vp, bottom_left_figure_left_axis_vp, bottom_left_figure_bottom_axis_vp, bottom_left_figure_top_axis_vp, bottom_left_figure_right_axis_vp,
                      top_right_figure_vp, top_right_figure_left_axis_vp, top_right_figure_bottom_axis_vp, top_right_figure_top_axis_vp, top_right_figure_right_axis_vp,
                      bottom_right_figure_vp, bottom_right_figure_left_axis_vp, bottom_right_figure_bottom_axis_vp, bottom_right_figure_top_axis_vp, bottom_right_figure_right_axis_vp,
                      left_right_figure_sep_vp, right_overview_figure_sep_vp, overview_vp
      )
    }

    tmp.grob <- gTree(childrenvp = figure_vp,
      name=paste0(content_area, ".GROWTH.GRADE.", grade),
      children=gList(gTree(vp="figure_vp", # "top_plots", # 'name' from `top_plots_vp`
      childrenvp=components,
      name=paste0("CHILDREN.", content_area, ".GROWTH.GRADE.", grade),
        children=gList(target.gList))))

    svglite::svglite(filename = file.path(output_path, paste0(output_name, "_no_title", f.ext)),
        pointsize = 12,
        fix_text_size = FALSE,
        bg = "transparent",
        width=parent_width,
        height=parent_height,
        # web_fonts = svglite::font_face( # base64 -b 0 /Users/avi/fonts/Oswald/static/Oswald-Light.ttf > /Users/avi/fonts/Oswald/static/Oswald-Light_base64.txt
        #    family = "Oswald",
        #    ttf = "/Users/avi/fonts/Oswald/static/Oswald-ExtraLight.ttf"))
        web_fonts = "https://fonts.googleapis.com/css2?family=Oswald:wght@300&display=swap")
    grid.draw(tmp.grob)
    dev.off()
  }

  ####  "top_growth" == top_left_figure
  if ("top_growth" %in% subplot.types) {
    childlist <- names(academic_impact_grob$children)
    wrapped.grobs <- grep("recordedGrob", childlist); grob.names <- childlist[wrapped.grobs]
    target.grobs <- names(unlist(lapply(academic_impact_grob$children[wrapped.grobs], function(f) grep("top_left_figure", as.character(f$list$vp)))))
    target.grobs <- c(target.grobs, grob.names[which(grob.names == tail(target.grobs, 1))+1])

    target.index <- which(childlist %in% target.grobs)

    target.gList <- academic_impact_grob$children[head(target.index, 1):tail(target.index, 1)]

    target.vprts <- as.character(unlist(lapply(target.gList, function(f) as.character(f[["list"]][["vp"]][["name"]]))))
    vprts.to.rm <- target.vprts[target.vprts %in% ls()]
    rm(list=vprts.to.rm); gc()
    # rm(list=c("top_left_figure_vp", "top_left_figure_left_axis_vp", "top_left_figure_bottom_axis_vp", "top_left_figure_top_axis_vp", "top_left_figure_right_axis_vp"))

    for (v in seq_along(target.gList)) {
      if ("list" %in% names(target.gList[[v]])) {
        current.vp <- target.gList[[v]][["list"]][["vp"]]
        if (!is.null(current.vp))
          assign(current.vp[["name"]], makeVP(current.vp, row.change= -2L))
        current.vp <- current.vp[["name"]]
        next
      }
      if (!is.null(current.vp))
        target.gList[[v]][["vp"]] <- get(current.vp)
    }

    target.gList <- target.gList[grep("recordedGrob", names(target.gList), invert=TRUE)]

    components <- vpList(
                    top_left_figure_vp,
                    top_left_figure_left_axis_vp,
                    top_left_figure_bottom_axis_vp,
                    top_left_figure_top_axis_vp,
                    top_left_figure_right_axis_vp
    )

    ##  overview_vp
    oview.grobs <- names(unlist(lapply(academic_impact_grob$children[wrapped.grobs], function(f) grep("overview_vp", as.character(f$list$vp)))))
    oview.grobs <- c(oview.grobs, grob.names[which(grob.names == oview.grobs)+1])
    oview.index <- which(childlist %in% oview.grobs)
    oview.gList <- academic_impact_grob$children[oview.index[1]:oview.index[2]]

    overall.fill <- oview.gList[[grep(".rect.", names(oview.gList))[2]]][["gp"]][["fill"]]

    target.gList[[grep("polygon", names(target.gList))]][["gp"]][["fill"]] <-
    target.gList[[grep("polygon", names(target.gList))]][["gp"]][["col"]] <- paste0(overall.fill, 80)

    tmp.grob <- gTree(childrenvp = top_plots_vp,
      name=paste0(content_area, ".GROWTH.GRADE.", grade),
      children=gList(gTree(vp="top_plots", # 'name' from `top_plots_vp`
      childrenvp=components,
      name=paste0("CHILDREN.", content_area, ".GROWTH.GRADE.", grade),
        children=gList(target.gList))))

    svglite::svglite(filename = file.path(output_path, paste0(output_name, "_top_growth", f.ext)),
        pointsize = 12, bg = "transparent",
        # web_fonts = svglite::font_face(
        #    family = "Oswald",
        #    ttf = "/Users/avi/fonts/Oswald/static/Oswald-ExtraLight.ttf"))
        web_fonts = "https://fonts.googleapis.com/css2?family=Oswald:wght@300&display=swap")
    grid.draw(tmp.grob)
    dev.off()
  }


} ### END covidImpactSubVisualization
