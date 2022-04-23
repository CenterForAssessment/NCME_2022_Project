#####
###   Update and install packages required for various analyses, summaries, etc.
###   included in the Academic Impact report.
###
###   Separate sections for CRAN and Github package versions.
#####

###   Update user's packages
update.packages(ask = FALSE, checkBuilt = TRUE)

###   Install/update latest packages from CRAN

##    Data management & manipulation
if (!require(data.table)) {
	install.packages("data.table", dep=T)
}
if (!require(R.utils)) {
	install.packages("R.utils", dep=T)
}
if (!require(stringr)) {
	install.packages("stringr", dep=T)
}

##    Plotting
if (!require(colorspace)) {
	install.packages("colorspace", dep=T)
}
if (!require(ggplot2)) {
	install.packages("ggplot2", dep=T)
}
if (!require(gghighlight)) {
	install.packages("gghighlight", dep=T)
}
if (!require(hexbin)) {
	install.packages("hexbin", dep=T)
}
if (!require(svglite)) {
	install.packages("svglite", dep=T)
}
if (!require(VIM)) {
	install.packages("VIM", dep=T)
}

##   Tables
if (!require(htmlTable)) {
	install.packages("htmlTable", dep=T)
}
if (!require(Gmisc)) {
	install.packages("Gmisc", dep=T)
}
if (!require(pander)) {
	install.packages("pander", dep=T)
}

###   Install/update latest packages from GITHUB
if (!require(remotes)) {
	install.packages("remotes", dep=T)
}

###   Install/update latest packages from GITHUB
remotes::install_github("centerforassessment/Literasee")
remotes::install_github("centerforassessment/cfaTools")
remotes::install_github("centerforassessment/toOrdinal")
remotes::install_github("centerforassessment/SGPdata")
remotes::install_github("centerforassessment/SGP")
# remotes::install_github("centerforassessment/SGPmatrices")
remotes::install_github('rstudio/rmarkdown')
remotes::install_github('rstudio/bookdown')
remotes::install_github('rstudio/pagedown')
remotes::install_github("rfortherestofus/pagedreport", ref = "main")
remotes::install_github("ropensci/qpdf")

###  Utils function from `pagedreport`
pkg_resource <- function(...) {
  system.file("resources", ..., package = "pagedreport", mustWork = TRUE)
}
