# 1872 Brazilian Census: Occupation and Sector Analysis
# Guilherme Lambais
# Run from project root

dir.create("build", showWarnings = FALSE)

if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")

source("source/fun_census.R")
source("source/data_census.R")

# To launch the Shiny dashboard:
# shiny::runApp("app.R")
