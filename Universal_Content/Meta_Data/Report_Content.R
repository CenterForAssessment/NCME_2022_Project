################################################################################
###                                                                          ###
###    List and order of child .Rmd files to be used in report/appendices    ###
###                                                                          ###
################################################################################

rmd.files <- list(
  report = list(
    file.order = c(
      "setup.Rmd",
      "params.Rmd",
      "0_Abstract.Rmd",
      "1_Intro__Overview.Rmd",
      "1_Intro_Background.Rmd",
      "1_Intro_Methods.Rmd",
      "1_Intro_Data_Sources.Rmd",
      "2_Participate__Analysis.Rmd",
      "2_Participate__Overview.Rmd",
      "2_Participate_Counts.Rmd",
      "3_Impact__Overview.Rmd",
      "3_Impact_Achievement_Analysis.Rmd",
      "3_Impact_Achievement_Overview.Rmd",
      "3_Impact_Growth_Analysis.Rmd",
      "3_Impact_Growth_Overview.Rmd",
      "4_Summary.Rmd"
    ),
    references = TRUE
  ),
  appendices = list(
    A = list(
      title = "Academic Imapact Analysis",
      file.order = c(
        "params.Rmd",
        "setup_impact_overview_appendix.Rmd",
        "Appendix_Impact_Intro.Rmd",
        "Appendix_Impact_Grade_Level_State.Rmd"
      ),
      references = NULL
    ),
    # B = c(),
    # C = c(),
    # B = list(
    #   title = "Initial SGP Analysis",
    #   file.order = c(
    #     "params.Rmd",
    #     "setup_sgp_appendix.Rmd",
    #     "Appendix_SGP_Analysis.Rmd"
    #   ),
    #   references = NULL
    # ),
    # C = list(
    #   title = "Imapact Report Generation",
    #   file.order = c(
    #     "Appendix_A_Init_Analyses.html"
    #   ),
    #   references = NULL
    # ),
    R = list(
      title = "R Session Information",
      file.order = c(
        "Appendix_R.Rmd"
      ),
      references = NULL
    )
  ),
  bookdown = list(
    rmd.path = file.path("assets", "rmd", "bookdown"),
    report = list(
      file.order = c()
    )
  ),
  pagedown = list(
    rmd.path = c()#,
    # report = list(
    #   file.order = c(1:4, 6, 5, 7:20)
    # )
  )
)

###   Modify/merge together the custom.files and report.config lists
if (!exists("custom.files")) {
  message("\n\tNo 'custom.files' list exists in your current environment.  The universal ('rmd.files') list will be returned.\n")
  custom.files <- list()
} else {
  rmd.files <- modifyList(rmd.files, custom.files) # This order allows subsetting of children (I think)
  # rmd.files <- modifyList(custom.files, rmd.files, keep.null = TRUE)
  if ("appendices" %in% names(rmd.files) & length(rmd.files[["appendices"]]) > 0)
    rmd.files[["appendices"]] <- rmd.files[["appendices"]][order(names(rmd.files[["appendices"]]))]
}

rmd.files$report.source$custom <-
  match(list.files(report.config$params$custom.rmd.path), rmd.files$report$file.order)
if (all(is.na(rmd.files$report.source$custom))) {
  rmd.files$report.source$custom <- NULL
} else {
  if (any(is.na(rmd.files$report.source$custom))) {
    message("\n\tFile(s):\n\t\t",
      paste(list.files(report.config$params$custom.rmd.path)[which((is.na(rmd.files$report.source$custom)))], collapse=" \n\t\t "),
      "\n\t located in the customized content directory do not appear in the report 'file.order'\n")
    rmd.files$report.source$custom <- rmd.files$report.source$custom[!is.na(rmd.files$report.source$custom)]
  }
}

rmd.files$report.source$universal <-
  setdiff((1:length(rmd.files$report$file.order)), rmd.files$report.source$custom)
if (all(is.na(rmd.files$report.source$universal))) {
  rmd.files$report.source$universal <- NULL
}

if (length(rmd.files$appendices) > 0) {
  for (apdx in seq(length(rmd.files$appendices))) {
    rmd.files$appendices[[apdx]]$file.source$custom <-
      match(list.files(report.config$params$custom.rmd.path), rmd.files$appendices[[apdx]]$file.order)
    if (all(is.na(rmd.files$appendices[[apdx]]$file.source$custom))) {
      rmd.files$appendices[[apdx]]$file.source$custom <- NULL
    }
    rmd.files$appendices[[apdx]]$file.source$universal <-
      setdiff((1:length(rmd.files$appendices[[apdx]]$file.order)), rmd.files$appendices[[apdx]]$file.source$custom)
    if (all(is.na(rmd.files$appendices[[apdx]]$file.source$universal))) {
      rmd.files$appendices[[apdx]]$file.source$universal <- NULL
    }
  }
}
