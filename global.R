##############################
##  load necessary packages ##
##     for all scripts      ##
##############################

#check if packages need installing
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

my_packages <- c("shiny", "shinythemes", "shinycustomloader", "leaflet", "leaflet.providers",
                 "dplyr", "tidyr", "sf", "leaflegend", "terra", "tidyterra", "renv", "shinyFeedback")

load_or_install(my_packages)

renv::snapshot()

# Source text for the "About" panel
tabPanelAbout <- source("About.R")$value
