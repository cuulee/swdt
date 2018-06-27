# User interface
tabAOIUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      3,
      helpText(
        "This interface allows the selection of an area of interests."
      ),
      panel(
        heading = "AOI",
        uiOutput(ns("aoi")),
        actionButton(ns("start_session"), "Start Session")
      )
    ),
    column(
      9,
      tags$style(type = "text/css", 
                 "#tabAOI-map {height: calc(100vh - 80px) !important;}"),
      leafletOutput(ns("map"), height = 700, width = "100%")
    )
  )
}

# Server
tabAOI <- function(input, output, session, config, app_session) {
  aoi_data <- reactiveVal(NULL)

  observe({
    #' Check configuration file
    #'
    true_config <-
      config %>%
      filter(dir.exists(Image))

    false_config <-
      setdiff(config, true_config)

    # Validation
    if (nrow(true_config) == 0) {
      showModal(
        modalDialog("No valid path in configuration file")
      )
    } else {
      aoi_data(true_config)

      if (nrow(false_config) > 0) {
        false_names <-
          false_config %>%
          dplyr::select(Name) %>%
          pull()

        showModal(
          modalDialog(glue(
            "No valid path in configuration file for aoi ",
            glue::collapse(false_names, ",", last = " and ")
          ))
        )
      }
    }
  })

  output$aoi <- renderUI({
    #' Render aoi selection
    #'
    if (is.null(aoi_data())) {
      shinyjs::disable("start_session")
    }
    req(aoi_data())
    selectInput(session$ns("aoi"),
      choices = aoi_data()$Name,
      label = NULL,
      selected = aoi_data()$Name[1]
    )
  })

  uuid <- reactiveVal(NULL)
  image_path <- reactiveVal(NULL)
  shape_path <- reactiveVal(NULL)
  thumb_path <- reactiveVal(NULL)

  observeEvent(input$start_session, {
    #' Starts Session
    #'
    shinyjs::disable("aoi")
    shinyjs::disable("start_session")

    input$aoi %>%
      glue("-", UUIDgenerate()) %>%
      uuid()

    aoi_data() %>%
      filter(Name == input$aoi) %>%
      dplyr::select(Image) %>%
      pull() %>%
      image_path()

    aoi_data() %>%
      filter(Name == input$aoi) %>%
      dplyr::select(Shape) %>%
      pull() %>%
      shape_path()

    aoi_data() %>%
      filter(Name == input$aoi) %>%
      dplyr::select(Thumb) %>%
      pull() %>%
      thumb_path()

    # Change to processing tab
    updateTabsetPanel(app_session, inputId = "navbar", selected = "processing")
  })

  output$map <- renderLeaflet({
    #' Render leaflet map
    #'
    req(input$aoi)

    if (input$aoi == "NA") {
      leaflet() %>%
        setView(lng = 25.19, lat = 54.54, zoom = 4) %>%
        addTiles()
    } else {
      read_sf(glue("./data/", input$aoi), input$aoi) %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fill = FALSE, color = "#008cba")
    }
  })

  tabAOIOutput <- reactive({
    #' Module output
    #'
    list(
      aoi = input$aoi,
      uuid = uuid,
      image_path = image_path,
      shape_path = shape_path,
      thumb_path = thumb_path
    )
  })

  return(tabAOIOutput)
}
