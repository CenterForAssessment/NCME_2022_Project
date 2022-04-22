#' ## Reporting Step 4: Parameters and configurations
#'
#' In this step we set up report configuration and content lists. This means we
#' specify any necessary meta-data and parameters required to run the report and
#' create/customize/complete the required YAML and RMD files that produce the report(s).
#'
#' This step assumes the user is operating with their working directory set to
#' "*NCME_2022_Project/All_States/State_A/Documentation*".

#+ echo = TRUE, purl = TRUE
# setwd("./Documentation")
##  Load `Literasee` package
require(Literasee)
##  Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#' Load existing `Report_Data` and `Report_Analyses` objects from steps 2 and 3.
#'
#+ echo = TRUE, purl = TRUE
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")

#' ### Report configurations - data and meta-data.
#'
#' A `custom.config` list is created to supply project specific information. It
#' can also be used to override some of the Universal settings (authors, etc.) We
#' create a "master" config list in the "*NCME_2022_Project/Universal_Content/Meta_Data*"
#' directory, and can merge the two together (prioritizing the elements in the
#' custom list).
#'
#' In this project, the config list is comprised of three sub-lists: `params`,
#' `client.info` and `top.level` information.
#'
#' The `params` object is given special treatment in `rmarkdown`. An object of this
#' name can be passed to `R` and executed internally either by creating a named
#' list and passing it in as an argument to `rmarkdown::render` or by creating a
#' section if the report `yaml` with the proper structure.
#'
#' The individual elements of the `params` can be any valid `R` data type. Character
#' strings can be used to provide text (from single words to larger chunks), index
#' a list, be fed into a functions argument, etc. The same is true for numeric values.
#' A list object could be passed in as well to provide several typed of data for
#' a specific task. The `params` specified here are used to run further analyses
#' on the `Report_Data`, provide logic to execute code/text chunks conditionally,
#' and supply required state-specific meta-data.
#'
#' An exhaustive list of the report should be kept to ensure all `params` are
#' defined. In the list below, we define only a few of the possible parameters,
#' and the unspecified ones are filled in internally in the `params.Rmd` script,
#' which is run at the beginning of the report rendering process.
#'
#' This list is semi-exhaustive of what can be supplied to the .Rmd.

#+ echo = TRUE, purl = TRUE
params = list(
  state.name = "State A", # required at a minimum
  state.abv = "S.A.",
  state.org = "State A Department of Education",
  state.org.abv = "SADoE",
  draft = TRUE, # NULL to remove draft status
  draft.text = "DRAFT REPORT -- DO NOT CITE", # default if `draft`=TRUE
  keyword = "academic impact", # lower case. Camel applied as needed or can be customized as keyword_camel
  imputations = FALSE,
  min.size.school = 15,  #  N size cutoff for inclusion in summaries/analyses
  min.size.district = 50, # N size cutoff for inclusion in summaries/analyses
  sgp.abv = list( # `SGP` abbreviation for accessing `SGPstateData` meta-data.
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

#' The `client.info` and `top.level` lists are all text elements that need to be
#' customized for each state. The `client.info` section is used exclusively in
#' the `nciea_report` theme in this demonstration project.  The `top.level` list
#' includes the information the report such as the title, authors, etc. that
#' will be included in the report front-matter.

#+ echo = TRUE, purl = TRUE
client.info = list(
  state.name = "State A", # required at a minimum
  state.abv = "D.C.", # for cover page, not SGPstateData
  city.name = "Washington",
  organization = "State A Department of Education",
  org.head = "Secretary Miguel Cardona",
  github.repo = "https://github.com/CenterForAssessment/NCME_2022_Project/tree/main/All_States/State_A",
  acknowledgements = "the entire staff of the SADoE Assessment and Accountability Office, and particularly Maggie Q. Public,"
)

# Title/subtitle, author.names, author.affil, date
top.level = list(  #  Title/subtitle, author.names, author.affil, date
  title = "Academic Impact in State A",
  subtitle = "Student Achievement and Growth during the COVID-19 Pandemic"
)

#' A `custom.files` list supplies a list of the `child` document. That is, it
#' defines what content will be included, excluded or customized in the report.
#' As with the `custom.config` list, we create a "master" content (child) list
#' in the "*NCME_2022_Project/Universal_Content/Meta_Data*" directory, and can
#' merge the two together (prioritizing the elements in the custom list).
#'
#' In this project, the content list is comprised of two main sub-lists: `report`,
#' and `appendices`. The `report` list below is identical to the one in the master
#' `Report_Content.R` script and references an individual child .Rmd file in the
#' "*NCME_2022_Project/Universal_Content/rmarkdown/Child_RMD*" directory. One can
#' re-order these files to change the ordering in the report. It is also possible
#' to edit/modify the "universal" child documents and keep the new version in a
#' separate directory (typically "*State_X/Documentation/assets/rmd/Custom_Content*").

#+ echo = TRUE, purl = TRUE
##  List and order of child .Rmd files to be used in report/appendices
##  The first two must be supplied and appear at the top of the report.
report = list(
  file.order = c(
    "setup.Rmd", # REQUIRED! - load R packages, declares knitr configs, etc.
    "params.Rmd",# REQUIRED! - check/add to `params` supplied by list or yaml
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
  references = TRUE # Are references/citations used?
)

#' Besides adding/reordering Rmd files though `custom.files`, one can request a
#' subset of files. This will result in a truncated report, allowing section
#' editing/development. You always need to include `setup.Rmd` and `params.Rmd`!
#'
#' This report contains several appendices. The first shows detailed visualizations
#' of academic impact. The second is a detailed description of the data preparation
#' and analysis (created directly from the `R` code!). Third is the appendix
#' you are reading now (also generated directly from `R` code ) regarding how to
#' create flexible, reproducible reports. The final appendix is simply the `R`
#' session information detailing the machinery behind the analysis and reporting.

#+ echo = TRUE, purl = TRUE
appendices = list(
  A = c(),
  B = list(
    title = "Initial SGP Analysis",
    file.order = c(
      "params.Rmd",
      "setup_sgp_appendix.Rmd",
      "Appendix_SGP_Analysis.Rmd"
    ),
    references = NULL
  ),
  C = list(
    title = "Imapact Report Generation",
    file.order = c(
      "Appendix_Imapact_Report_Generation.html"
    ),
    references = NULL
  ),
  R = c()
)

#' ###  Combine report meta data and generate .Rmd parents
#'
#' The following script will merge the report.config (universal) and custom.config
#' lists and return 'report.config'.
#+ echo = TRUE, purl = TRUE
custom.config <- list(client.info = client.info, top.level = top.level, params = params)
source(file.path(universal.content.path, "Meta_Data", "Report_Configs.R"))
rm(params) # remove this to prevent conflicts with `render` later.

#' Now merge the rmd.files (universal) and custom.files lists and return 'rmd.files'
#' to be used in next steps. The `custom.files` will override defaults if they
#' exist in "*assets/rmd/Custom_Content*".
#+ echo = TRUE, purl = TRUE
custom.files <- list(report = report, appendices = appendices)
source(file.path(universal.content.path, "Meta_Data", "Report_Content.R"))

#' With the combined meta-data, we can now create the .yml and .Rmd "master/parent"
#' documents for a `nciea_report` and/or `bookdown` site. These scripts can also
#' be used as "skeletons" for other reports (such as working drafts, alternate
#' output formats/templates, etc.)
#+ echo = TRUE, purl = TRUE
createReportScripts(report_config=report.config, rmd_file_list=rmd.files)
##  Save report YAML and file configurations
save(list=c("report.config", "rmd.files"), file = "Report_Configuration_MetaData.rda")
# setwd("..")
