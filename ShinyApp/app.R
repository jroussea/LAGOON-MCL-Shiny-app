library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)

source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)