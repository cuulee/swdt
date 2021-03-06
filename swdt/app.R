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
library(raster)
library(uuid)
library(DBI)
library(showtext)
library(ggplot2)
library(tsar)
library(xml2)
library(htmlwidgets)
library(modeest)
library(lubridate)
library(bsplus)
library(tools)

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
source("thresholding.R")

# User Interface
ui <- tagList(
  # Add custom styles
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$head(tags$script(src = "lib/opacity/Control.Opacity.js")),
  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "lib/opacity/Control.Opacity.css"
  )),
  tags$head(tags$script(src = "lib/jquery/jquery-ui-1.10.3.custom.min.js")),
  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "lib/jquery/jquery-ui-1.10.3.custom.min.css"
  )),
  useShinyjs(),
  navbarPageWithText(
    id = "navbar",
    theme = "bootstrap.css",
    "Sentinel-1 Water Dynamics Toolkit",
    tabPanel(
      title = "AOI",
      id = "aoi",
      value = "aoi",
      tabAOIUI("tabAOI")
    ),
    tabPanel(
      title = "Processing",
      id = "processing",
      value = "processing",
      tabProcessingUI("tabProcessing")
    ),
    tabPanel(
      title = "Water Extent Minimum ",
      id = "water_extent_minimum",
      value = "water_extent_minimum",
      tabWaterExtentUI("tabWaterExtentMinimum")
    ),
    tabPanel(
      title = "Water Extent Maximum",
      id = "water_extent_maximum",
      value = "water_extent_maximum",
      tabWaterExtentUI("tabWaterExtentMaximum")
    )
    ,
    tabPanel(
      "Water Dynamics",
      id = "water_dynamic",
      value = "water_dynamic",
      tabWaterDynamicUI("tabWaterDynamic")
    ),
    text = textOutput("text", inline = TRUE)
  ),
  tags$script(src = "navigation_modal.js")
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

    thumb <-
      xml %>%
      xml_find_all("//aoi/thumbs") %>%
      xml_text()

    return(tibble(Name = name, Image = image, Shape = shape, Thumb = thumb))
  }

  # Modules
  tabAOIOutput <- callModule(tabAOI,
    "tabAOI",
    config = read_config(),
    app_session = session
  )

  tabProcessingOutput <- callModule(
    tabProcessing,
    "tabProcessing",
    tabAOIOutput,
    app_session = session
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

  tabWaterDynamicOutput <- callModule(
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
    if (is.null(tabAOIOutput()$uuid())) {
      shinyjs::disable(selector = "#navbar li a[data-value=processing]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=processing]")
    }

    if (is.null(tabProcessingOutput()$temporal_statistics$minimum)) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_extent_minimum\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_extent_minimum\"]")
    }

    if (is.null(tabWaterExtentMinimumOutput()$water_extent)) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_extent_maximum\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_extent_maximum\"]")
    }

    if (is.null(tabWaterExtentMaximumOutput()$water_extent)) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_dynamic\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_dynamic\"]")
    }
  })

  observeEvent(input$nav_processing, {
    if (is.null(tabAOIOutput()$uuid())) {
      showModal(
        modalDialog("No aoi selected.")
      )
    }
  })

  observeEvent(input$nav_water_extent_minimum, {
    if (is.null(tabProcessingOutput()$temporal_statistics$minimum)) {
      showModal(
        modalDialog("No processing executed.")
      )
    }
  })

  observeEvent(input$nav_water_extent_maximum, {
    if (is.null(tabWaterExtentMinimumOutput()$water_extent)) {
      showModal(
        modalDialog("No minimum water extent calculated.")
      )
    }
  })

  observeEvent(input$nav_water_dynamic, {
    if (is.null(tabWaterExtentMaximumOutput()$water_extent)) {
      showModal(
        modalDialog("No maximum water extent calculated.")
      )
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
