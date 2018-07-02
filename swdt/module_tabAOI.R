# User interface
tabAOIUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      3,
      bs_accordion(id = glue("help_text_", id)) %>%
        bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bs_append(
          title = "Help",
          content = "This interface allows the selection of an area of interests."
        ),
      panel(
        heading = "AOI",
        uiOutput(ns("aoi")),
        actionButton(ns("start_session"), "Start Session"),
        actionButton(ns("restart_session"), "Restart Session")
      )
    ),
    column(
      9,
      tags$style(
        type = "text/css",
        "#tabAOI-map {height: calc(100vh - 80px) !important;}"
      ),
      leafletOutput(ns("map"), height = 700, width = "100%")
    )
  )
}

# Server
tabAOI <- function(input, output, session, config, app_session) {
  observe({
    shinyjs::disable("restart_session")
  })

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
  
  pass_aoi <- reactiveVal(NULL)
  
  observeEvent(input$aoi, {
    #' Workaround because restore does not work properly
    #' 
    pass_aoi(input$aoi)
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
    req(pass_aoi())
    shinyjs::disable("aoi")
    shinyjs::disable("start_session")

    pass_aoi() %>%
      glue("-", UUIDgenerate()) %>%
      uuid()

    aoi_data() %>%
      filter(Name == pass_aoi()) %>%
      dplyr::select(Image) %>%
      pull() %>%
      image_path()

    aoi_data() %>%
      filter(Name == pass_aoi()) %>%
      dplyr::select(Shape) %>%
      pull() %>%
      shape_path()

    aoi_data() %>%
      filter(Name == pass_aoi()) %>%
      dplyr::select(Thumb) %>%
      pull() %>%
      thumb_path()

    # Change to processing tab
    updateTabsetPanel(app_session, inputId = "navbar", selected = "processing")

    shinyjs::enable("restart_session")
  })

  shape_aoi <- reactiveVal(NULL)

  observe({
    #' Read shapefile
    #'
    req(pass_aoi())

    path <- aoi_data() %>%
      filter(Name == pass_aoi()) %>%
      dplyr::select(Shape) %>%
      pull()

    dsn <- dirname(path)
    layer <-
      basename(path) %>%
      file_path_sans_ext()

    read_sf(dsn, layer) %>%
      shape_aoi()
  })

  output$map <- renderLeaflet({
    #' Render leaflet map
    #'
    req(shape_aoi())
    
    if (is.null(pass_aoi())) {
      leaflet() %>%
        setView(lng = 25.19, lat = 54.54, zoom = 4) %>%
        addTiles()
    } else {
      shape_aoi() %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fill = FALSE, color = "#008cba")
    }
  })

  observeEvent(input$restart_session, {
    session$reload()
  })
  
  setBookmarkExclude("start_session")
  
  onBookmark(function(state) {
    #' Bookmark reactive values
    #'
    state$values$uuid <- uuid()
    state$values$shape_aoi <- shape_aoi()
    state$values$image_path <- image_path()
    state$values$shape_path <- shape_path()
    state$values$thumb_path <- thumb_path()
    state$values$pass_aoi <- pass_aoi()
  })
  
  onRestore(function(state) {
    uuid(state$values$uuid)
    shape_aoi(state$values$shape_aoi) 
    image_path(state$values$image_path)
    shape_path(state$values$shape_path)
    thumb_path(state$values$thumb_path)
    pass_aoi(state$values$pass_aoi)
  })
  
  onRestored(function(state) {
    updateSelectInput(session, "aoi", selected=state$values$pass_aoi)
  })

  tabAOIOutput <- reactive({
    #' Module output
    #'
    list(
      aoi = pass_aoi,
      uuid = uuid,
      shape_aoi = shape_aoi,
      image_path = image_path,
      shape_path = shape_path,
      thumb_path = thumb_path
    )
  })

  return(tabAOIOutput)
}
