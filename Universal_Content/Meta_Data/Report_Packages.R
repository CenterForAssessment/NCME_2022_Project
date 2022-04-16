#####
###   Update and install packages required for various analyses, summaries, etc.
###   included in the LEARNING LOSS report.
###   Separate sections for CRAN and Github package versions.
#####

###   Update user's packages
update.packages(ask = FALSE, checkBuilt = TRUE)

###   Install/update latest packages from GITHUB
remotes::install_github("centerforassessment/Literasee")
#remotes::install_github("centerforassessment/cfaTools")
#remotes::install_github("centerforassessment/SGPdata")
#remotes::install_github("centerforassessment/SGP")
remotes::install_github("centerforassessment/SGPmatrices")
#remotes::install_github("Rdatatable/data.table")
remotes::install_github('rstudio/rmarkdown')
remotes::install_github('rstudio/bookdown')
remotes::install_github('rstudio/pagedown')
remotes::install_github("GRousselet/rogme")

###   Install/update latest packages from CRAN

##    Modeling & Output
if (!require(Hmisc)) {
  install.packages("Hmisc", dep=T)
}
if (!require(MASS)) {
  install.packages("MASS", dep=T)
}
if (!require(randomForest)) {
  install.packages("randomForest", dep=T)
}
if (!require(caret)) {
  install.packages("caret", dep=T)
}
if (!require(inTrees)) {
  install.packages("inTrees", dep=T)
}
if (!require(modelsummary)) {
  install.packages("modelsummary", dep=T)
}
# if (!require(stargazer)) {
#   install.packages("stargazer", dep=T)
# }

#Data Manipulation
if (!require(reshape2)) {
  install.packages("reshape2", dep=T)
}
if (!require(plyr)) {
  install.packages("plyr", dep=T)
}
if (!require(dplyr)) {
  install.packages("dplyr", dep=T)
}
if (!require(tibble)) {
  install.packages("tibble", dep=T)
}

#Plotting
if (!require(GGally)) {
  install.packages("GGally", dep=T)
}
if (!require(ggplot2)) {
  install.packages("ggplot2", dep=T)
}
if (!require(plotly)) {
  install.packages("plotly", dep=T)
}
if (!require(vcd)) {
  install.packages("vcd", dep=T)
}
if (!require(VIM)) {
  install.packages("VIM", dep=T)
}
if (!require(lemon)) {
  install.packages("lemon", dep=T)
}
if (!require(gghighlight)) {
  install.packages("gghighlight", dep=T)
}


#Tables
if (!require(kableExtra)) {
  install.packages("kableExtra", dep=T)
}
