# User interface
tabWaterDynamicUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      3,
      bs_accordion(id = glue("help_text_", id)) %>%
        bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bs_append(
          title = "Help",
          content = "This interface allows the creation of a water dynamics map based on the water extent maps."
        ),
      tags$script(HTML(
        glue("document.getElementById(\"help_text_", id, "-0-collapse\").classList.remove('in');")
      )),
      panel(
        heading = "Visualization",
        colourInput(ns("color_class_0"), "Never flooded", "#f4f1e0"),
        colourInput(ns("color_class_1"), "Temporarily flooded", "#9ecae1"),
        colourInput(ns("color_class_2"), "Permanently flooded", "#008CBA")
      ),
      panel(
        downloadButton(ns("download"), "Download")
      )
    ),
    column(
      9,
      tags$style(
        type = "text/css",
        "#tabWaterDynamic-map {height: calc(100vh - 80px) !important;}"
      ),
      withSpinner(
        leafletOutput(ns("map"),
          height = 700,
          width = "100%"
        ),
        type = 8,
        color = "#008cba"
      )
    )
  )
}

tabWaterDynamic <- function(input,
                            output,
                            session,
                            tabAOIInput,
                            tabProcessingInput,
                            tabWaterExtentMinimumInput,
                            tabWaterExtentMaximumInput) {
  compute_map <- reactive({
    #' Compute map
    #'
    withProgress(message = "Classification", value = 0, {
      stack(
        tabWaterExtentMinimumInput()$water_extent,
        tabWaterExtentMaximumInput()$water_extent
      ) %>%
        calc(sum) %>%
        reclassify(c(-Inf, 0, 0, 0, 1, 1, 2, Inf, 2))
    })
  })

  water_dynamic_map <- reactiveVal()

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    shinyjs::disable("download")
    r <- compute_map()
    water_dynamic_map(r)
    shinyjs::enable("download")

    # Create color palete
    pal <- colorFactor(c(
      input$color_class_2,
      input$color_class_1,
      input$color_class_0
    ),
    c(0, 1, 2),
    na.color = "transparent"
    )

    # Render map
    tabAOIInput()$shape_aoi() %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fill = FALSE, color = "#008cba") %>%
      addRasterImage(r, colors = pal, project = FALSE) %>%
      addLegend(
        position = "topright",
        pal = pal, values = c(0, 1, 2),
        title = "Class",
        opacity = 1,
        labFormat = labelFormat(transform = function(x) {
          label <- tibble(Value = x) %>%
            mutate(Label = case_when(
              Value == 0 ~ "Permanently flooded",
              Value == 1 ~ "Temporarily flooded",
              Value == 2 ~ "Never flooded"
            )) %>%
            dplyr::select(Label) %>%
            pull()
          return(label)
        })
      )
  })
  
  output$download <- downloadHandler(
    #' Download tiff file
    #'
    glue(tabAOIInput()$aoi, "_", 
         strftime(tabProcessingInput()$start_date(), "%Y-%m-%d"), 
         "_", 
         strftime(tabProcessingInput()$end_date(), "%Y-%m-%d"),
         ".tif"),
    content = function(file) {
      writeRaster(water_dynamic_map(), file, format = "GTiff")
    },
    contentType = "image/tiff"
  )
}
