# User interface
tabWaterExtentUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      3,
      helpText(
        "This interface allows the creation of water extent maps based on the minimum and maximum backscatter of the time series."
      ),
      panel(
        heading = "Classification",
        div(style = "display: inline-block;vertical-align:top;", numericInput(ns("threshold"),
          "Threshold",
          value = -20,
          width = "200px"
        )),
        div(
          style = "display: inline-block;vertical-align:top; float:right;",
          dropdownButton(
            numericInput(ns("outlier"), "Outlier", value = 30),
            circle = FALSE,
            status = "default",
            icon = NULL,
            width = "300px",
            size = "sm",
            right = FALSE
          )
        ),
        withSpinner(
          plotOutput(ns("histogram"),
            click = ns("plot_click"),
            height = 250
          ),
          type = 8,
          color = "#008cba"
        ),
        numericInput(ns("filter_size"),
          label = "Filter",
          value = 3,
          width = "200px"
        ),

        switchInput(ns("filter"),
          label = "Filter",
          value = FALSE
        ),

        actionButton(ns("classify"), "Classify")
      ),
      panel(
        heading = "Statistics",
        withSpinner(tableOutput(ns("statistics")),
          type = 8,
          color = "#008cba"
        )
      )
    ),
    column(
      9,
      withSpinner(leafletOutput(ns("map"), height = 700, width = "100%"),
        type = 8,
        color = "#008cba"
      )
    )
  )
}

tabWaterExtent <- function(input,
                           output,
                           session,
                           tabAOIInput,
                           tabProcessingInput,
                           mode) {
  layer <- reactiveVal(NULL)

  observe({
    #' Switch between minimum and maximum
    #'
    req(tabProcessingInput()$temporal_statistics$minimum)
    req(tabProcessingInput()$temporal_statistics$maximum)

    if (mode == "minimum") {
      layer(tabProcessingInput()$temporal_statistics$minimum)
    } else if (mode == "maximum") {
      layer(tabProcessingInput()$temporal_statistics$maximum)
    }
  })

  output$title <- renderText({
    #' Set navbar heading
    #'
    if (mode == "minimum") {
      "Minimum"
    } else if (mode == "maximum") {
      "Maximum"
    }
  })

  observe({
    #' Deactivated input widgets at start
    #'
    shinyjs::disable("filter_size")
    shinyjs::disable("base_raster")
  })

  observeEvent(input$filter, {
    #' Enable and disable filter size input
    #'
    if (input$filter) {
      shinyjs::enable("filter_size")
    } else {
      shinyjs::disable("filter_size")
    }
  })

  pass_filter_size <- reactiveVal(3)
  pass_filter <- reactiveVal(FALSE)
  pass_threshold <- reactiveVal(-20)

  observeEvent(input$classify, {
    #' Input parameter are passed through reactive values
    #' Allows limited reactivity of renderLeaflet
    #' RenderLeaflet is trigged by the histogram and the classify button
    #' And not through value inputs
    #'
    if (input$threshold != pass_threshold()) {
      # Prevents reloading of histogram if just filter_size is different
      pass_threshold(input$threshold)
    }
    pass_filter(input$filter)
    pass_filter_size(input$filter_size)
  })

  observeEvent(input$plot_click$x, {
    #' Workaround by passing plot_click$x through in reavtive value
    #' plot_click$x resets to NULL after a second
    #'
    pass_filter(input$filter)
    pass_filter_size(input$filter_size)

    if (!is.null(input$plot_click$x)) {
      pass_threshold(ceiling(input$plot_click$x))
      # Update numericInput of threshold with histogram selection
      updateNumericInput(session,
        "threshold",
        value = ceiling(input$plot_click$x)
      )
    }
  })

  water_extent <- reactiveVal(NULL)

  classify <- function(r, threshold, filter, filter_size, updateProgress) {
    #' Classify raster files based on threshold
    #'
    updateProgress(value = 0.1, detail = "Classify")

    r <- reclassify(r, c(
      -Inf,
      threshold,
      0,
      threshold,
      Inf,
      1
    ))

    if (filter) {
      updateProgress(value = 0.3, detail = "Filter")

      r <- focal(
        x = r,
        w = matrix(
          1 / filter_size^2,
          filter_size,
          filter_size
        ),
        fun = median
      )
    }

    updateProgress(value = 0.8, detail = "Write")
    return(r)
  }

  compute_water_extent <- reactive({
    req(layer())

    # Validation
    if (isolate(input$filter_size) %% 2 == 0) {
      showModal(
        modalDialog("Filter size musst be an uneven number")
      )
    }
    validate(
      need(isolate(input$filter_size) %% 2 != 0, FALSE)
    )

    # Initialize progressbar
    progress <- shiny::Progress$new()
    progress$set(message = "Classification", detail = "Get", value = 0)

    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }

    # Classify raster
    r <- classify(
      layer(),
      pass_threshold(),
      pass_filter(),
      pass_filter_size(),
      updateProgress
    )
    progress$close()
    water_extent(r)
    r
  })

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    req(compute_water_extent())

    # Map coulering
    pal <- colorFactor(c("#008cba", "#f4f1e0"),
      c(0, 1),
      na.color = "transparent"
    )
    pal_sentinel <- colorNumeric(c("#000000", "#FFFFFF"),
      values(layer()),
      na.color = "transparent"
    )

    # Create map
    leaflet() %>%
      addTiles() %>%
      addRasterImage(compute_water_extent(),
        colors = pal,
        project = FALSE,
        group = "Classified",
        opacity = 1
      ) %>%
      addRasterImage(layer(),
                     colors = pal_sentinel,
                     project = FALSE,
                     group = "Radar",
                     opacity = 1
      ) %>%
      onRender("function(el,x,data){
               var map = this;
               var labels = map.layerManager._byGroup.Classified;
               var opacitySlider = new L.Control.opacitySlider();
               
               for (const prop in labels) {
               opacitySlider.setOpacityLayer(labels[prop]);
               }
               
               map.addControl(opacitySlider);}") %>%
      addLegend(
        position = "topright",
        pal = pal, values = c(0, 1),
        title = NULL,
        opacity = 1,
        labFormat = labelFormat(transform = function(x) {
          return(ifelse(x == 0, "Water", "Land"))
        })
      ) %>%
      addLayersControl(
        overlayGroups = c("Radar", "Classified"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$statistics <- renderTable({
    #' Render table output with statistical measures
    #'
    req(compute_water_extent())

    val <- raster::getValues(compute_water_extent())
    water_pixel <- tibble(Val = val) %>%
      filter(Val == 0) %>%
      summarise(Val = n()) %>%
      pull()

    land_pixel <- tibble(Val = val) %>%
      filter(Val == 1) %>%
      summarise(Val = n()) %>%
      pull()

    tibble(Measure = character(), Value = numeric()) %>%
      add_row(Measure = "Water Percentage", Value = water_pixel / (water_pixel + land_pixel)) %>%
      add_row(Measure = "Land Percentage", Value = land_pixel / (water_pixel + land_pixel))
  })

  compute_histogram <- reactive({
    #' Compute histogram based on minimum backscatter raster file
    #' Seperated from maximum calculation to avoid unnecessary recalculation
    #' while switching between minimum and maximum tab
    #'
    req(layer())

    hist_data <- suppressWarnings(
      hist(layer(), breaks = 100)
    )
    hist_data <-
      tibble(
        counts = hist_data$counts,
        breaks = hist_data$mids
      ) %>%
      filter(counts > input$outlier)
  })

  output$histogram <- renderPlot({
    #' Render histogram output
    #'
    # Add Open Sans font from Bootstrap layout to ggplot
    font_add_google("Open Sans", "opensans")
    showtext_auto()

    # Plot histogram
    ggplot(compute_histogram(), aes(x = breaks, y = counts, fill = counts)) +
      geom_bar(stat = "identity", fill = "#008cba", alpha = 0.8) +
      xlab("Backscatter") + ylab("Frequency") +
      theme(
        text = element_text(size = 12, family = "opensans"),
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(size = 1, colour = "#e7e7e7"),
        axis.ticks = element_blank()
      ) +
      geom_vline(xintercept = pass_threshold(), size = 1)
  })

  tabWaterExtentOutput <- reactive({
    #' Module ouput
    #'
    list(
      water_extent = water_extent()
    )
  })

  return(tabWaterExtentOutput)
}
