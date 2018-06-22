# User interface
tabWaterExtentUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  tabPanel(
    title = "Water Extent",
    id = "water_extent",
    fluidRow(
      column(
        3,
        helpText(
          "This interface allows the creation of water extent maps based on the minimum and maximum backscatter of the time series."
        ),
        panel(
          heading = "Classification",
          numericInput(ns("threshold"),
            "Threshold",
            value = -38,
            width = "200px"
          ),
          switchInput(ns("filter"),
            label = "Filter",
            value = FALSE
          ),
          numericInput(ns("filter_size"),
            label = NULL,
            value = 3,
            width = "200px"
          ),
          actionButton(ns("classify"), "Classify")
        ),
        panel(
          heading = "Radar image",
          switchInput(
            inputId = ns("base_raster"),
            label = "Add",
            value = FALSE
          ),
          chooseSliderSkin("Flat"),
          setSliderColor("#008cba", c(1)),
          sliderInput(ns("opacity"),
            label = "Opacity", min = 0,
            max = 1, value = 1, width = "300px"
          )
        )
      ),
      column(
        9,

        tabsetPanel(
          tabPanel("Minimum",
            value = "minimum"
          ),
          tabPanel("Maximum",
            value = "maximum"
          ),
          id = ns("switch")
        ),
        fluidRow(
          column(
            6,
            plotOutput(ns("histogram"), click = ns("plot_click"), height = 250)
          ),
          column(
            1,
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
          column(
            5,
            tableOutput(ns("statistics"))
          )
        ),

        withSpinner(leafletOutput(ns("map"),
                                  height = 550,
                                  width = "100%"),
                    type=8,
                    color="#008cba"
        )
      )
    )
  )
}

tabWaterExtent <- function(input, output, session, tabAOIInput, tabProcessingInput) {
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

  water_extent <- reactiveValues(minimum = NULL, maximum = NULL)

  classify_water <- function(r, threshold, filter, filter_size, updateProgress) {
    #' Classify raster files based on threshold
    #'
    updateProgress(value = 0.1, detail = "Reclassify")

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

  output$statistics <- renderTable({
    #' Render table output with statistical measures
    #'
    if (input$switch == "minimum") {
      req(water_extent[["minimum"]])
      val <- getValues(water_extent[["minimum"]])
    } else if (input$switch == "maximum") {
      req(water_extent[["maximum"]])
      val <- getValues(water_extent[["maximum"]])
    }

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

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
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
    progress$set(message = "Classify", detail = "Get", value = 0)

    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    # Switch between maximum and minimum raster files
    if (input$switch == "minimum") {
      r <- classify_water(
        tabProcessingInput()$temporal_statistics$minimum,
        pass_threshold[["minimum"]],
        pass_filter(),
        pass_filter_size(),
        updateProgress
      )

      raster_sentinel <- tabProcessingInput()$temporal_statistics$minimum
      water_extent[["minimum"]] <- r
    } else if (input$switch == "maximum") {
      r <- classify_water(
        tabProcessingInput()$temporal_statistics$maximum,
        pass_threshold[["maximum"]],
        pass_filter(),
        pass_filter_size(),
        updateProgress
      )

      raster_sentinel <- tabProcessingInput()$temporal_statistics$maximum
      water_extent[["maximum"]] <- r
    }
    progress$close()

    # Map coulering
    pal <- colorFactor(c("#008cba", "#f4f1e0"),
      c(0, 1),
      na.color = "transparent"
    )
    pal_sentinel <- colorNumeric(c("#000000", "#FFFFFF"),
      values(raster_sentinel),
      na.color = "transparent"
    )

    # Create map
    map <- leaflet() %>%
      addTiles()

    if (input$base_raster) {
      map <-
        map %>%
        addRasterImage(raster_sentinel,
          colors = pal_sentinel,
          project = FALSE,
          group = "Sentinel-1"
        )
    }

    map %>%
      addRasterImage(r,
        colors = pal,
        project = FALSE,
        group = "Classified",
        opacity = input$opacity
      ) %>%
      addLegend(
        position = "topright",
        pal = pal, values = c(0, 1),
        title = "Water extent",
        opacity = 1,
        labFormat = labelFormat(transform = function(x) {
          return(ifelse(x == 0, "Water", "Land"))
        })
      )
  })

  pass_filter_size <- reactiveVal(3)
  pass_filter <- reactiveVal(FALSE)
  pass_threshold <- reactiveValues(minimum = -20, maximum = -20)

  observeEvent(input$classify, {
    #' Input parameter are passed through reactive values
    #' Allows limited reactivity of renderLeaflet
    #' RenderLeaflet is trigged by the histogram and the classify button
    #' And not through value inputs
    pass_filter(input$filter)
    pass_filter_size(input$filter_size)

    if (input$switch == "minimum") {
      pass_threshold[["minimum"]] <- input$threshold
    } else if (input$switch == "maximum") {
      pass_threshold[["maximum"]] <- input$threshold
    }
  })

  observeEvent(input$plot_click$x, {
    #' Workaround by passing plot_click$x through in reavtive value
    #' plot_click$x resets to NULL after a second
    #'
    pass_filter(input$filter)
    pass_filter_size(input$filter_size)

    if (!is.null(input$plot_click$x)) {
      if (input$switch == "minimum") {
        pass_threshold[["minimum"]] <- ceiling(input$plot_click$x)
      } else if (input$switch == "maximum") {
        pass_threshold[["maximum"]] <- ceiling(input$plot_click$x)
      }
      
      # Update numericInput of threshold with histogram selection
      updateNumericInput(session, 
                         "threshold", 
                         value = ceiling(input$plot_click$x))
    }
  })

  observeEvent(input$switch, {
    #' Update numericInput of threshold when switching between minimum and 
    #' maximum
    #' 
    if (input$switch == "minimum") {
      updateNumericInput(session, 
                         "threshold", 
                         value = pass_threshold[["minimum"]])
    } else if (input$switch == "maximum") {
      updateNumericInput(session, 
                         "threshold", 
                         value = pass_threshold[["maximum"]])
    }
  })

  compute_histogram_min <- reactive({
    #' Compute histogram based on minimum backscatter raster file
    #' Seperated from maximum calculation to avoid unnecessary recalculation
    #' while switching between minimum and maximum tab
    #'
    hist_data <- suppressWarnings(
      hist(tabProcessingInput()$temporal_statistics$minimum, breaks = 100)
    )
    hist_data <-
      tibble(
        counts = hist_data$counts,
        breaks = hist_data$mids
      ) %>%
      filter(counts > input$outlier)
  })

  compute_histogram_max <- reactive({
    #' Compute histogram based on maximum backscatter raster file
    #' Seperated from minimum calculation to avoid unnecessary recalculation
    #' while switching between minimum and maximum tab
    #'
    hist_data <- suppressWarnings(
      hist(tabProcessingInput()$temporal_statistics$maximum, breaks = 100)
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
    if (input$switch == "minimum") {
      hist_data <- compute_histogram_min()
    } else if (input$switch == "maximum") {
      hist_data <- compute_histogram_max()
    }

    # Add Open Sans font from Bootstrap layout to ggplot
    font_add_google("Open Sans", "opensans")
    showtext_auto()

    # Plot histogram
    plot <- ggplot(hist_data, aes(x = breaks, y = counts, fill = counts)) +
      geom_bar(stat = "identity", fill = "#008cba", alpha = 0.8) +
      xlab("Backscatter") + ylab("Frequency") +
      theme(
        text = element_text(size = 12, family = "opensans"),
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(size = 1, colour = "#e7e7e7"),
        axis.ticks = element_blank()
      )

    # Add threshold line
    if (input$switch == "minimum") {
      plot + geom_vline(xintercept = pass_threshold[["minimum"]], size = 1)
    } else if (input$switch == "maximum") {
      plot + geom_vline(xintercept = pass_threshold[["maximum"]], size = 1)
    }
  })
  
  tabWaterExtentOutput <- reactive({
    #' Module ouput
    #' 
    list(
      water_extent = water_extent
    )
  })
  
  return(tabWaterExtentOutput)
}
