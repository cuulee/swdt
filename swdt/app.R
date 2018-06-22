# Sentinel-1 Surface Water Dynamics Toolkit
# Marc Becker
# 2018

# Libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(colourpicker)
library(DT)
library(leaflet)
library(sf)
library(glue)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(raster)
library(uuid)
library(DBI)
library(showtext)
library(ggplot2)
library(tsar)

# Navbar with text @daattali
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl
  )
  navbar
}

# Source modules
source("module_tabAOI.R")
source("module_tabProcessing.R")
source("module_tabWaterExtent.R")
source("module_tabWaterDynamic.R")

# User Interface
ui <- tagList(
  # Add custom styles
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  useShinyjs(),
  navbarPageWithText(
    id = "navbar",
    theme = "bootstrap.css",
    "Sentinel-1 Water Dynamics Toolkit",
    tabAOIUI("tabAOI"),
    tabProcessingUI("tabProcessing"),
    tabWaterExtentUI("tabWaterExtent"),
    tabWaterDynamicUI("tabWaterDynamic"),
    text = textOutput("text", inline = TRUE)
  )
)

# Server
server <- function(input, output, session) {
  # Modules
  tabAOIOutput <- callModule(tabAOI, "tabAOI")
  tabProcessingOutput <- callModule(
    tabProcessing,
    "tabProcessing",
    tabAOIOutput
  )
  tabWaterExtentOutput <- callModule(
    tabWaterExtent,
    "tabWaterExtent",
    tabAOIOutput,
    tabProcessingOutput
  )
  tabWaterDynamicOutput <-  callModule(
    tabWaterDynamic,
    "tabWaterDynamic",
    tabAOIOutput,
    tabProcessingOutput,
    tabWaterExtentOutput
  )
  
  observe({
    #' Restrict access to tabs if content is missing
    #' 
    if (is.na(tabAOIOutput()$uuid())) {
      shinyjs::disable(selector = "#navbar li a[data-value=Processing]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=Processing]")
    }
    
    if (is.null(tabProcessingOutput()$temporal_statistics$minimum)) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"Water Extent\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"Water Extent\"]")
    }
    
    if (!is.null(tabWaterExtentOutput()$water_extent$minimum) & 
        !is.null(tabWaterExtentOutput()$water_extent$maximum)) {
      shinyjs::enable(selector = "#navbar li a[data-value=\"Water Dynamic\"]")
    } else {
      shinyjs::disable(selector = "#navbar li a[data-value=\"Water Dynamic\"]")
    }
  })

  output$text <- renderText({
    #' Add session info to navbar
    #'
    req(tabAOIOutput()$uuid())
    tabAOIOutput()$uuid()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
