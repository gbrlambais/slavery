# Brazilian Census: Occupation and Sector Analysis (1872, 1920, Transformation)
# Guilherme Lambais
# Run from project root

dir.create("build", showWarnings = FALSE)

if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")

source("source/fun_census.R")
source("source/fun_census1920.R")
source("source/fun_amc.R")
source("source/data_census.R")
source("source/data_census1920.R")
source("source/data_transformation.R")

# To launch the Shiny dashboard:
# shiny::runApp("app.R")

# To export static site for GitHub Pages:
# source("source/export_static.R")
