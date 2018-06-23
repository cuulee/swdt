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
library(xml2)

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
    navbarMenu("Water Extent",
      tabWaterExtentUI("tabWaterExtentMinimum"),
      tabWaterExtentUI("tabWaterExtentMaximum")
    ),
    tabWaterDynamicUI("tabWaterDynamic"),
    text = textOutput("text", inline = TRUE)
  )
)

# Server
server <- function(input, output, session) {
  read_config <- function() {
    xml <- read_xml("./config.xml")
    name <-
      xml %>% 
      xml_find_all("//aoi/name") %>% 
      xml_text()
    
    image <-
      xml %>% 
      xml_find_all("//aoi/images") %>% 
      xml_text()
    
    shape <-
      xml %>% 
      xml_find_all("//aoi/shape") %>% 
      xml_text()
    
    return(tibble(Name = name, Image = image, Shape = shape))
  }
  
  # Modules
  tabAOIOutput <- callModule(tabAOI, "tabAOI", config=read_config())

  tabProcessingOutput <- callModule(
    tabProcessing,
    "tabProcessing",
    tabAOIOutput
  )
  
  tabWaterExtentMinimumOutput <- callModule(
    tabWaterExtent,
    "tabWaterExtentMinimum",
    tabAOIOutput,
    tabProcessingOutput,
    mode = "minimum"
  )
  
  tabWaterExtentMaximumOutput <- callModule(
    tabWaterExtent,
    "tabWaterExtentMaximum",
    tabAOIOutput,
    tabProcessingOutput,
    mode = "maximum"
  )
  
  tabWaterDynamicOutput <-  callModule(
    tabWaterDynamic,
    "tabWaterDynamic",
    tabAOIOutput,
    tabProcessingOutput,
    tabWaterExtentMinimumOutput,
    tabWaterExtentMaximumOutput
  )
  
  observe({
    #' Restrict access to tabs if content is missing
    #' 
    req(tabAOIOutput()$uuid())
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
    
    if (!is.null(tabWaterExtentMinimumOutput()$water_extent) & 
        !is.null(tabWaterExtentMaximumOutput()$water_extent)) {
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
