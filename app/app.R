library(shiny)
library(tidyverse)

source("app/make_data.R")

source("app/ui.R")
source("app/server.R")

shinyApp(ui, server)
