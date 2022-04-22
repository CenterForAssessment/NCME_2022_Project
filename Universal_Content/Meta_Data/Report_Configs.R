################################################################################
###                                                                          ###
###  Report configuration list creation -- merge universal and custom lists  ###
###                                                                          ###
################################################################################

###   Universal (generic) report configuration elements:

report.config <- list(
  # Title/subtitle, author.names, author.affil, date
  top.level=list(
    # title = "Demonstration COVID Learning Loss Analysis", auto configured if NULL based on client state.name
    # subtitle = "Academic Impact During the COVID-19 Pandemic", # auto configured if NULL
    author.names = c("Adam R. VanIwaarden", "Damian W. Betebenner"),
    author.affil = "Center for Assessment", # Only needed if different affiliations.
    # date = "March 2021",  #  auto configured to MONTH YEAR format
    draft = TRUE,
    executive.summary = TRUE,
    project.team = "Team SGP",
    project.email = "avaniwaarden@nciea.org"
  ),
  ###   client.info to be customized for each state.
  client.info=c(), # placeholder for list order
  params = list(
    draft.text = c(), # "ALTERNATE DRAFT TEXT", #  auto configured to 'DRAFT REPORT -- DO NOT CITE' if report.config$top.level$draft = TRUE
    keyword = "academic impact", # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
    show.warnings = FALSE,
    base.directory = getwd(),
    unvrsl.rmd.path = file.path("..", "..", "..", "Universal_Content", "rmarkdown", "Child_RMD"),
    custom.rmd.path = file.path("assets", "rmd", "Custom_Content")
  ),
  output = list(
    bookdown = list(
      directory = "site"
    )#,
    # pagedown = list(
    #   file = "default"
    # )
  )
)

###   Modify/merge together the custom.config and report.config lists

if (!exists("custom.config")) {
  message("\n\tNo 'custom.config' list exists in your current environment.  The universal ('report.config') list will be returned unchanged (without report.config$client.info).\n")
  custom.config <- list()
} else {
  report.config <- modifyList(report.config, custom.config)
}

###   Set prelim params - "state.name" required at a minimum

if (is.null(report.config$params$state.name)) {
  report.config$params$state.name <- report.config$client.info$state.name
}
if (is.null(report.config$client.info$state.name)) {
  report.config$client.info$state.name <- report.config$params$state.name
}
if (is.null(report.config$params$state.name)) {
  stop("Full 'State' name must be provided at a minimum in either 'report.config$params$state.name' or 'report.config$client.info$state.name'.")
}

if (!is.null(report.config$top.level$draft)) {
  if (is.logical(report.config$top.level$draft)) {
    if (report.config$top.level$draft & is.null(report.config$params$draft.text)) {
      report.config$params$draft.text <- "DRAFT REPORT -- DO NOT CITE OR CIRCULATE"
    } else report.config$params$draft.text <- ""
  }
}

###   Add common top-level YAML fields

if (is.null(report.config$top.level$title)) {
  report.config$top.level$title <- paste(report.config$client.info$state.name, SGP::capwords(report.config$params$keyword), "Analysis")
}
if (is.null(report.config$top.level$subtitle)) {
  report.config$top.level$subtitle <- "Academic Impact During the COVID-19 Pandemic"
}

if (is.null(report.config$top.level$executive.summary)) {
  report.config$top.level$executive.summary <- FALSE
} else {
  if (!is.logical(report.config$top.level$executive.summary)) {
    report.config$top.level$executive.summary <- TRUE
  }
}
report.config$params$executive.summary <- report.config$top.level$executive.summary

if (is.null(report.config$top.level$bibliography)) {
  report.config$top.level$bibliography <- system.file("rmarkdown", "content", "bibliography", "Literasee.bib" , package = "Literasee")
}

##    Client Info
if (is.null(report.config$client.info$state.abv)) {
  if (toupper(report.config$client.info$state.name)=="DEMONSTRATION COVID") {
    report.config$client.info$state.abv <- "DEMO_COVID"
  } else {
    report.config$client.info$state.abv$State_Assessment <- getStateAbbreviation(report.config$client.info$state.name)
  }
}

##    Output info
if (is.null(report.config$output$bookdown)) {
  report.config$output$bookdown$directory <- "site"
}

if (is.null(report.config$output$pagedown)) {
  report.config$output$pagedown$file <- gsub(" ", "_", paste(report.config$client.info$state.name, SGP::capwords(report.config$params$keyword), "Analysis"))
}
