# User interface
tabAOIUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  tabPanel(
    title = "AOI",
    id = "aoi",
    fluidRow(
      column(
        3,
        helpText(
          "This interface allows the selection of an area of interests."
        ),
        panel(
          heading = "AOI",
          selectInput(ns("aoi"),
            choices = list("Fuente" = "fuente"),
            label = NULL,
            selected = "fuente"
          ),
          actionButton(ns("start_session"), "Start Session")
        )
      ),
      column(
        9,
        leafletOutput(ns("map"), height = 700, width = "100%")
      )
    )
  )
}

# Server
tabAOI <- function(input, output, session) {
  uuid <- reactiveVal(NA)

  observeEvent(input$start_session, {
    #' Starts Session
    #'
    shinyjs::disable("aoi")
    shinyjs::disable("start_session")

    input$aoi %>%
      glue("-", UUIDgenerate()) %>%
      uuid()

    output$info <- renderText(uuid())
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
      uuid = uuid
    )
  })

  return(tabAOIOutput)
}
