# User interface
tabProcessingUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      4,
      helpText(
        "This interface allows the processing of Sentinel-1 time series to minimum and maximum backscatter raster files."
      ),
      panel(
        heading = "Filter",
        uiOutput(ns("date_range")),
        DTOutput(ns("table")),
        switchInput(ns("parallel"),
          label = "Parallel",
          value = FALSE,
          size = "small"
        ),
        actionButton(ns("calculate"), "Calculate")
      )
    ),
    column(
      8,
      tags$style(type = "text/css", 
                 "#tabProcessing-map {height: calc(100vh - 80px) !important;}"),
      leafletOutput(ns("map"),
        height = 700,
        width = "100%"
      )
    )
  )
}

# Server
tabProcessing <- function(input, output, session, tabAOIInput, app_session) {
  files <- reactive({
    #' Creates data table with available Sentinel-1 scenes
    #'
    files <- list.files(tabAOIInput()$image_path(), "^S1")
    paths <- list.files(tabAOIInput()$image_path(),
      "^S1",
      full.names = TRUE
    )
    thumbs <- list.files(glue("./www", tabAOIInput()$thumb_path()), "^S1")
    thumbs <- paste0("thumb/fuente/", thumbs)

    as_tibble(files) %>%
      separate(value,
        c("Mission", "Mode", "E", "Date", "Polarisation"),
        "_+",
        extra = "drop",
        fill = "right"
      ) %>%
      dplyr::select(-one_of("E")) %>%
      mutate(Date = str_sub(Date, 1, 8)) %>%
      mutate(Date = as.Date(Date, "%Y%m%d")) %>%
      cbind(files) %>%
      cbind(paths) %>%
      cbind(thumbs) %>%
      mutate_if(is.factor, as.character) %>%
      arrange(Date)
  })

  start_date <- reactiveVal()
  end_date <- reactiveVal()

  output$date_range <- renderUI({
    #' Render date range input
    #'
    files() %>%
      dplyr::select(Date) %>%
      filter(Date == min(Date)) %>%
      slice(1) %>%
      pull() %>%
      start_date()

    files() %>%
      dplyr::select(Date) %>%
      filter(Date == max(Date)) %>%
      slice(1) %>%
      pull() %>%
      end_date()

    dateRangeInput(session$ns("date_range"),
      label = "Date Range",
      start = isolate(start_date()),
      end = isolate(end_date()),
      language = "de"
    )
  })

  observeEvent(input$date_range, {
    #' Validate date range input
    #'
    if (input$date_range[1] > input$date_range[2]) {
      showModal(
        modalDialog("You cannot enter a start date later than the end date.")
      )
      updateDateRangeInput(session, "date_range", start = start_date(), end = end_date())
    } else {
      start_date(input$date_range[1])
      end_date(input$date_range[2])
    }
  })

  output$table <- renderDT({
    #' Render table with available Sentinel-1 scenes
    #'
    req(start_date())
    req(end_date)

    files() %>%
      dplyr::select("Mission", "Mode", "Date") %>%
      filter(Date > start_date()) %>%
      filter(Date < end_date())
  },
  style = "bootstrap",
  server = TRUE, selection = "single",
  options = list(
    iDisplayLength = 10,
    aLengthMenu = c(5, 10),
    bLengthChange = 0,
    bFilter = 0,
    bInfo = 0,
    bAutoWidth = 1
  )
  )

  thumb_extent <- reactive({
    #' Get raster extent for thumbs
    #'
    req(files())

    extent_raster <- files() %>%
      slice(1) %>%
      dplyr::select(paths) %>%
      pull() %>%
      raster() %>%
      projectRaster(crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>%
      extent()

    glue("[[", extent_raster@ymin,
         ", ", extent_raster@xmin,
         "], [", extent_raster@ymax, ", ", 
         extent_raster@xmax, "]]")
  })

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    map <- read_sf(
      tabAOIInput()$shape_path(),
      tabAOIInput()$aoi
    ) %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fill = FALSE, color = "#008cba")

    # Add Senintel-1 raster file
    if (is.null(input$table_row_last_clicked)) {
      map
    } else {
      r <-
        files() %>%
        slice(input$table_row_last_clicked) %>%
        dplyr::select(thumbs) %>%
        pull()

      map %>%
        htmlwidgets::onRender(paste0("function(el, x) {
                                 var map = this;
                                 var imageUrl = \'", r, "\';
                                 var imageBounds = ", thumb_extent(), ";
                                 L.imageOverlay(imageUrl, imageBounds).addTo(map);
    }"))
    }
  })

  temporal_statistics <- reactiveValues(minimum = NULL, maximum = NULL)

  observeEvent(input$calculate, {
    #' Calculate minium and maximum backscatter raster files from time series
    #' Searches for cached data in sqlite database
    #'
    shinyjs::disable("calculate")
    withProgress(
      message = "Calculation",
      detail = "Searching",
      value = 0, {
        
        # Create database folder
        if(!dir.exists("./database")) {
          dir.create("./database")
        }
        
        # Conntect to data base
        con <- dbConnect(RSQLite::SQLite(),
          dbname = "./database/swdt.sqlite"
        )

        # Create table if missing
        if (length(dbListTables(con)) == 0) {
          dbGetQuery(con, "CREATE TABLE temporal_statistic(
                             id INTEGER PRIMARY KEY NOT NULL,
                             aoi TEXT,
                             creation_date TEXT,
                             start_time TEXT,
                             end_time TEXT,
                             path_min TEXT,
                             path_max TEXT)")
        }

        # Search for cached data
        res <- dbGetQuery(con, glue(
          "SELECT * FROM temporal_statistic WHERE start_time = \'",
          strftime(start_date(), "%Y-%m-%dT%H:%M:%S%z"),
          "\' AND end_time = \'",
          strftime(end_date(), "%Y-%m-%dT%H:%M:%S%z"),
          "\' AND aoi = \'",
          tabAOIInput()$aoi,
          "\'"
        ))

        # Calculate, no cached data
        if (nrow(res) == 0) {
          s <-
            files() %>%
            filter(Date > start_date()) %>%
            filter(Date < end_date()) %>%
            dplyr::select("paths") %>%
            pull() %>%
            raster::stack()

          path_min <- glue(
            tabAOIInput()$image_path(),
            "/minimum/minimum-",
            tabAOIInput()$uuid(),
            ".tif"
          )

          path_max <- glue(
            tabAOIInput()$image_path(),
            "/maximum/maximum-",
            tabAOIInput()$uuid(),
            ".tif"
          )

          if (input$parallel) {
            # Parallel calculation with tsar package
            incProgress(0.2, detail = "Minimum")

            tsar(s,
              workers = list(minimum = function(x) return(min(x, na.rm = T))),
              cores = 4,
              out.name = path_min,
              out.bandnames = NULL,
              out.dtype = "FLT4S",
              separate = FALSE,
              na.in = NA,
              na.out = -999,
              overwrite = TRUE,
              verbose = FALSE,
              nodelist = NULL,
              bandorder = "BSQ",
              maxmemory = 1000,
              compress_tif = F
            )

            temporal_statistics[["minimum"]] <- raster(path_min)

            incProgress(0.6, detail = "Maximum")

            tsar(s,
              workers = list(maximum = function(x) return(max(x, na.rm = T))),
              cores = 4,
              out.name = path_max,
              out.bandnames = NULL,
              out.dtype = "FLT4S",
              separate = FALSE,
              na.in = NA,
              na.out = -999,
              overwrite = TRUE,
              verbose = FALSE,
              nodelist = NULL,
              bandorder = "BSQ",
              maxmemory = 1000,
              compress_tif = F
            )

            temporal_statistics[["maximum"]] <- raster(path_max)
          } else {
            # Calculation with raster package
            incProgress(0.2, detail = "Minimum")

            r_minimum <- calc(s, min)
            temporal_statistics[["minimum"]] <- r_minimum
            writeRaster(r_minimum, path_min, overwrite = TRUE)

            incProgress(0.6, detail = "Maximum")

            r_maximum <- calc(s, max)
            temporal_statistics[["maximum"]] <- r_maximum
            writeRaster(r_maximum, path_max, overwrite = TRUE)
          }

          # Write to database
          dbGetQuery(con, glue(
            "INSERT INTO temporal_statistic (
                             aoi,
                             creation_date,
                             start_time, 
                             end_time, 
                             path_min, 
                             path_max) VALUES (\'",
            tabAOIInput()$aoi,
            "\', \'",
            strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            strftime(start_date(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            strftime(end_date(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            path_min,
            "\', \'",
            path_max,
            "\')"
          ))
        } else { # Cached data available
          incProgress(0.5, detail = "Data in cache")

          r_minimum <-
            res %>%
            dplyr::select(path_min) %>%
            slice(1) %>%
            pull() %>%
            raster()

          temporal_statistics[["minimum"]] <- r_minimum

          r_maximum <-
            res %>%
            dplyr::select(path_max) %>%
            slice(1) %>%
            pull() %>%
            raster()

          temporal_statistics[["maximum"]] <- r_maximum
        }
      }
    )
    shinyjs::enable("calculate")
    showModal(
      modalDialog("Minimum and maximum backscatter raster files successfully calculated.",
        footer = tagList(
          modalButton("Dismiss"),
          actionButton(session$ns("next_tab"), "Next")
        )
      )
    )
  })

  observeEvent(input$next_tab, {
    #' Change to water extent tab after calculation
    #'
    removeModal()
    updateTabsetPanel(app_session, inputId = "navbar", selected = "water_extent_minimum")
  })

  tabProcessingOutput <- reactive({
    #' Module ouput
    #'
    list(
      temporal_statistics = temporal_statistics
    )
  })

  return(tabProcessingOutput)
}
