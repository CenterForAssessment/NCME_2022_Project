Report Meta Data
================

#  Purpose

This subdirectory contains meta-data necessary for the creation of the NCME 2022
Demonstration session reports. Specifically, the report configuration and RMD
child file information that are considered "universal" to these reports are
contained here for access by each branch (e.g., state/organization) for which a
report will be generated.

##  Included files

* The `Report_Configs.R` file contains `R` code that stores general report
  information in an appropriately formatted (named) `list` object. This object is
  given the name `report.config` and will be available for manipulation upon using
  `source(...)` to load it into the working `R` environment.
  - Information such as the report title/subtitle, author names/affiliations and
    parameters (data) that are required to created report content
    (tables/plots/conditional text)  are stored in this file.
  - This universal list can be combined with another list with customized meta-data
    (named `custom.config` in order to add client specific meta-data into the
    `report.config` list (state name, people to acknowledge etc.), or to change/update
    any of the universal meta-data that is stored in the `report.config` list.
* The `Report_Content.R` source code provides child RMD file information also
  stored as a `list` object. This meta-data includes the names of child files that
  all reports will likely contain (at a minimum). This object is given the name
  `rmd.files` and will be available for manipulation upon using `source(...)` to
  load it into the working `R` environment.
  - File name and order information is stored as the file name only (not a file
    path). The file paths are constructed based on the universal and custom child
    document paths provided in the `report.config` object
    (`report.config$params$unvrsl.rmd.path` and `report.config$params$custom.rmd.path`).  
  - Customized .Rmd documents based on "universal" child documents should be named
    identically if they are meant to replace the universal version entirely.  If
    they are meant to supplement or are entirely novel, they should be named
    uniquely and their position in the report noted in the final
    `rmd.files$file.order` element of the list.
  - Other information about the file order/content is also included here. For
    example, if file order should be different for the `pagedown` output (so as
    to optimize page breaks or table placement), this can be specified here. Or
    if the documents include citations that should be included in a "References"
    section.
  - Supplemental appendices are included in a separate "branch" of the list than
    the main report.

These two lists that are subsequently used by functions in the `Literasee` package
to create a "NCIEA" themed report and website. See the "NCME_2022_Project/Documentation"
subdirectory for example scripts of how to `source(...)` these two documents to
create the master lists for the "Demonstration_COVID" toy data analysis and render
the report/website.
