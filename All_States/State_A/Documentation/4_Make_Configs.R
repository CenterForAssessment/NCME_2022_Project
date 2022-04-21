#' ## Academic Impact Analysis Step 4: Make Configurations
#'
#' In this step we set up report configuration and content lists. This means we
#' specify any necessary meta-data and parameters required to run the report and
#' create/customize/complete the required YAML and RMD file config lists.
#'
#' This step assumes the user is operating with their working directory set to
#' "*NCME_2022_Project/All_States/State_A/Documentation*".

#+ echo = TRUE, purl = TRUE
# setwd("./Documentation")
##   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#' Load existing `Report_Data` and `Report_Analyses` objects from steps
#' 2 and 3.
#+ echo = TRUE, purl = TRUE
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")


###
###   Merge custom and universal config lists
###

##   Remove existing objects before (re)running
if (exists("report.config")) rm(report.config)
if (exists("rmd.files")) rm(rmd.files)

##   The "custom.config" list is created to supply unique client/state info.
##   It can also be used to override some of the Universal settings (authors, etc.)

# report.config <- list(
  client.info = list(
    state.name = "State A", # required at a minimum
    state.abv = "D.C.", # for cover page, not SGPstateData
    city.name = "Washington",
    organization = "State A Department of Education",
    org.head = "Secretary Miguel Cardona",
    github.repo = "https://github.com/CenterForAssessment/NCME_2022_Project/tree/main/All_States/State_A",
    acknowledgements = "the entire staff of the SADoE Assessment and Accountability Office, and particularly Maggie Q. Public,"
  )#,

  # Title/subtitle, author.names, author.affil, date
  top.level = list(  #  Title/subtitle, author.names, author.affil, date
    title = "Academic Impact in State A",
    subtitle = "Student Achievement and Growth during the COVID-19 Pandemic"
  ),
  # cat(yaml::as.yaml(top.level, indent.mapping.sequence=TRUE))
  ##  `params` are passed to R and executed internally to create Universal/Customized
  ##  text or used to run further analyses on the Report_Data.  Many of these can
  ##  be created internally in the `params.Rmd` script. This list is semi-exhaustive
  ##  of what can be supplied to the .Rmd.
  params = list(
    state.name = "State A", # required at a minimum
    state.abv = "S.A.",
    state.org = "State A Department of Education",
    state.org.abv = "SADoE",
    draft = TRUE, # NULL to remove draft status
    draft.text = "DRAFT REPORT -- DO NOT CITE OR CIRCULATE", # NULL to remove draft status
    executive.summary = FALSE
    keyword = "academic impact", # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
    imputations = FALSE,
    min.size.school = 15,  #  N size cutoff - exclude SCHOOLs with fewer than X students from summaries/analyses
    min.size.district = 50, # N size cutoff - exclude DISTRICTs with fewer than X students from summaries/analyses
    draft.text = c(), # "ALTERNATE DRAFT TEXT", #  auto configured to 'DRAFT REPORT -- DO NOT CITE' if report.config$top.level$draft = TRUE
    keyword = "academic impact", # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
    base.directory = getwd(),
    unvrsl.rmd.path = file.path("..", "..", "..", "Universal_Content", "rmarkdown", "Child_RMD"),
    custom.rmd.path = file.path("assets", "rmd", "Custom_Content"),
    sgp.abv = list( # SGP package abbreviation for accessing relevant SGPstateData meta-data.
      State_Assessment = "State_A"#,
      # College_Entrance = c(),
      # ELP_Assessment = c(),
      # Interim_Assessment = c()
    ),
    sgp.name = list(
      State_Assessment = "State A"
    ),
    test.name = list(
      State_Assessment = "A+ State Assessment Program"
    ),
    test.abv = list(
      State_Assessment = "ASAP"
    ),
    test.url = list(
      State_Assessment = "https://centerforassessment.github.io/SGPdata/"
    ),
    code.url = list(
      State_Assessment = "https://github.com/CenterForAssessment/NCME_2022_Project/tree/main/All_States/State_A/Initial_Data_Analysis"
    ),
    gof.path = list(
      State_Assessment = file.path("..", "Initial_Data_Analysis", "Goodness_of_Fit")
    )
  )
)

source(file.path(universal.content.path, "rmarkdown", "Child_RMD", "params.R"))

##   The following script will merge the report.config (universal) and custom.config lists and return 'report.config' to be used in next steps
source(file.path(universal.content.path, "Meta_Data", "Report_Configs.R"))

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.
# source(file.path(universal.content.path, "Meta_Data", "Report_Content.R"))

##    Besides adding/reordering Rmd files though custom.files, one can request a
##    subset of files. This will result in a truncated report, allowing chapter/section
##    editing/development. You always need to include `setup.Rmd` and `params.Rmd`!

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.

###    List and order of child .Rmd files to be used in report/appendices

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
      # "2_Participate_Enrolled_Students.Rmd",
      "2_Participate_Counts.Rmd",
      "3_Impact__Overview.Rmd",
      "3_Impact_Achievement_Analysis.Rmd",
      "3_Impact_Achievement_Overview.Rmd",
      "3_Impact_Growth_Analysis.Rmd",
      "3_Impact_Growth_Overview.Rmd",
      # "3_Impact_Synthesis.Rmd",
      "4_Summary.Rmd"
    ),
    references = TRUE
  ),
  appendices = list(
    # A = list(
    #   title = "Participation Supplimental Analyses",
    #   file.order = c(
    #     "setup_participation_appendix.Rmd",   #  Should be appendix specific (counter override, etc.)
    #     "params.Rmd",  #  Could be appendix specific - params_appendix_a.Rmd
    #     "Appendix_Participation_Intro.Rmd",
    #     "Appendix_Participation_by_School.Rmd",
    #     "Appendix_Participation_MinMax_Replace.Rmd"
    #   ),
    #   references = NULL
    # )#,
    #  Needs to be done still
    # G = list(
    #   title = "Goodness of Fit Plots",
    #   file.order = c(
    #     "setup_gofit_appendix.Rmd",   #  Should be appendix specific (counter override, etc.)
    #     "params.Rmd",  #  Could be appendix specific - params_gofit_appendix.Rmd
    #     "Appendix_GoFit_Intro.Rmd",
    #     "Appendix_GoFit_Grade_Level.Rmd"
    #   ),
    #   references = NULL
    # )
  ),
  bookdown = list(
    rmd.path = file.path("assets", "rmd", "bookdown"),
    report = list(
      file.order = c()
    )
  ),
  pagedown = list(
    rmd.path = c(),
    report = list(
      file.order = c(1:4, 6, 5, 7:20)
    )
  )
)

source(file.path(universal.content.path, "Meta_Data", "Report_Content.R"))


#####
###    Create the .yml and .Rmd "master/parent" documents for the `bookdown` site and `pagedown` report
#####

createReportScripts(report_config=report.config, rmd_file_list=rmd.files)

###   Save report YAML and file configurations
save(list=c("report.config", "rmd.files"), file = "Report_Configuration_MetaData.rda")
setwd("..")
