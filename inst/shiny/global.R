library(shiny)
library(shinydashboard)
library(shinyjs)
library(colourpicker)
devtools::load_all() #delete at the end

# Maximum dimensions the app uses
MAX_DIMS <- 6

SPATIAL_COL_PATTERN <- "^(x|y|lon|long|longitude|lat|latitude|easting|northing)$"

# Sourcing the instructions for each step
source("instructions.R")
