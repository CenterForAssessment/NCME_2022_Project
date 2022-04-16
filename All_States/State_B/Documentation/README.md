An Example Documentation Folder
================================

#  Purpose

This is a template for a "Documentation" directory that one might use (copy/paste)
into a project from which to build out `pagedown` report(s) and a `bookdown`
website using the child RMD files included in the Universal_Content repo.

#  Five steps to report generation

Generation of multiple format reports (e.g., a `bookdown` website and a `pagedown`
PDF document) can be generally conducted in five steps. These steps are lined out
generically in the `.R` scripts included in this repo.

First a consistent working environment should be set up, including a directory
structure with all the required external assets and libraries, as well as the `R`
command and function scripts included in this repo. Second, the data required to
generate the report must be compiled in a consistent data-object structure. Third,
any time-consuming and/or state-specific external analyses should be conducted
and thoroughly reviewed, and also compiled in a consistent data-object structure.
Fourth, list-objects that contain all the required meta-data and content indices
are compiled from generic (Universal) and custom sources to create dual-format
configuration scripts. Lastly, the desired formats are generated using the
appropriate `R` functions.

#  Included files

* The `1_Report_Setup_and_Maintenance.R` file contains `R` code from which required
  `R` packages can be installed or updated, and other assets can be copied into
  the report directory.
  - A script, `Universal_Content/Meta_Data/Report_Packages.R`, is available to
    both document and help install/update any `R` packages required for analyses
    to be run and the report(s) generated.
  - The "Documentation" directory for each branch (here "State_A and "State_B")
    can be be set up using the `setupReportDirectory` function. This function
    pulls in assets from the `Literasee` package.
    * These include `css`, `javascript`, `pandoc` and `Rmarkdown` (.Rmd) assets
      the `Literasee` package needs to create a "NCIEA" themed report and website.
    * Templates for custom child.Rmd files can also be added to the directory
      (available from the `Universal_Content` repo/submodule). Alternatively,
      template custom content can be copied over from another state/project directory.
  - Changes/updates/upgrades to these assets in the `Literasee` package can be
    pulled into the "Documentation" directory using the `updateAssets()` function.
* The `2_Report_Data.R` script runs all formatting, cleaning and subsetting
  required to compile all data sources into a single dataset that will be used
  at report run-time (rendering).
  - Create/format/alter/augment one or more raw data sets including `State_Assessment`,
    `College_Entrance`, `ELP_Assessment` and (potentially multiple) `Interim_Assessment`
    data objects.
  - The compiled data must be a named `list` object called `Report_Data`, saved
    in a "Documentation/Data/" directory (NOT included in the Github repo!).
* The script `3_Report_Analyses.R` is meant to house report-specific analyses
  that can be re/run before the report is compiled and may take an inordinate
  amount of time to run or requires extensive evaluation and investigation before
  inclusion in the report.
  - These analyses may be universal enough to run for all states, or may be unique
    enough for each state that the analysis is customized for each state.
  - Ideally each of the data sources (`State_Assessment`, `College_Entrance`,
    `ELP_Assessment` and `Interim_Assessment`) will have the same or similar
    analysis types (e.g., `participation`, `multiple_imputation`, `academic_impact`,
    etc.).
  - Some analyses may be short and included directly within child RMD scripts,
    while others may be placed externally in the "Report_Analyses" directory.
    The compiled analysis results are stored in a named `list` object  called
    `Report_Analysis`, which is saved in a "Documentation/Data" directory
    (NOT included in the GitHub repo!).
* Generation of the multiple format reports (e.g., a `bookdown` website and a
  `pagedown` PDF document) typically depends on different types of configuration
  scripts that list the child documents to knit together: _bookdown.yml and
  index.Rmd for `bookdown`, and a parent .Rmd file (e.g., STATE_X_Learning_Loss_Analysis.Rmd)
  for `pagedown`.
  - The child documents can be generic (i.e. `Universal_Content`) or customized/novel
    content. The `4_Make_Configs.R` script creates custom configuration lists that
    identify 1) state specific meta-data and report parameters and 2) a list of
    any custom Rmarkdown content to be used in place of, or in addition to, the
    universal report content.
  - These configuration and content lists are then combined with the `Universal_Content`
    configuration and content lists by `source(...)`'ing the R scripts
    `Universal_Content/Meta_Data/Report_Configs.R` and
    `Universal_Content/Meta_Data/Report_Content.R`.
  - The scripts are set up to give priority to the custom content, so that generic
    elements can be easily overridden. The combined custom and universal information
    `list` objects are then used by functions in the `Literasee` package to create
    the YAML and RMD files that control the report generation output.
* The `Make_Report.R` file contains `R` code to render the website/report using
  the `bookdown` and `pagedown` packages respectively.
