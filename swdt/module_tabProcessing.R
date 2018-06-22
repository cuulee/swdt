# User interface
tabProcessingUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  tabPanel(
    title = "Processing",
    id = "processing",
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
        leafletOutput(ns("map"),
          height = 700,
          width = "100%"
        )
      )
    )
  )
}

# Server
tabProcessing <- function(input, output, session, tabAOIInput) {
  files <- reactive({
    #' Creates data table with available Sentinel-1 scenes
    #'
    files <- list.files(glue("./data/", tabAOIInput()$aoi), "^S1")
    paths <- list.files(glue("./data/", tabAOIInput()$aoi),
      "^S1",
      full.names = TRUE
    )

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
      mutate_if(is.factor, as.character) %>%
      arrange(Date)
  })

  output$date_range <- renderUI({
    #' Render date range input
    #'
    start_date <- files() %>%
      dplyr::select(Date) %>%
      filter(Date == min(Date)) %>%
      slice(1) %>%
      pull()

    end_date <- files() %>%
      dplyr::select(Date) %>%
      filter(Date == max(Date)) %>%
      slice(1) %>%
      pull()

    dateRangeInput(session$ns("date_range"),
      label = "Date Range",
      start = start_date,
      end = end_date,
      language = "de"
    )
  })

  output$table <- renderDT({
    #' Render table with available Sentinel-1 scenes
    #'
    req(input$date_range)

    # Validate
    if (input$date_range[1] > input$date_range[2]) {
      showModal(
        modalDialog("You cannot enter an end date later than the start date")
      )
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
    validate(
      need(input$date_range[1] < input$date_range[2], FALSE)
    )

    files() %>%
      dplyr::select("Mission", "Mode", "Date") %>%
      filter(Date > input$date_range[1]) %>%
      filter(Date < input$date_range[2])
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

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    map <- read_sf(
      glue(
        "./data/",
        tabAOIInput()$aoi
      ),
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
        dplyr::select(paths) %>%
        pull() %>%
        raster() %>%
        stretch(minq = 0.05, maxq = 0.95)

      pal <- colorNumeric(c("#000000", "#FFFFFF"), values(r),
        na.color = "transparent"
      )

      map %>%
        addRasterImage(r, colors = pal)
    }
  })

  temporal_statistics <- reactiveValues(minimum = NULL, maximum = NULL)

  observeEvent(
    #' Calculate minium and maximum backscatter raster files from time series
    #' Searches for cached data in sqlite database
    #' 
    input$calculate, {
      withProgress(
        message = "Calculation",
        detail = "Searching",
        value = 0, {

          # Conntect to data base
          con <- dbConnect(RSQLite::SQLite(),
            dbname = "./database/swdt.sqlite"
          )
          
          # Create table if missing
          if(length(dbListTables(con)) == 0) {
            dbGetQuery(con, "CREATE TABLE temporal_statistic(
                             id INTEGER PRIMARY KEY NOT NULL,
                             start_time TEXT,
                             end_time TEXT,
                             path_min TEXT,
                             path_max TEXT)")
          }
          
          # Search for cached data
          res <- dbGetQuery(con, glue(
            "SELECT * FROM temporal_statistic WHERE start_time = \'",
            strftime(input$date_range[1], "%Y-%m-%dT%H:%M:%S%z"),
            "\' AND end_time = \'",
            strftime(input$date_range[2], "%Y-%m-%dT%H:%M:%S%z"),
            "\'"
          ))

          # Calculate, no cached data
          if (nrow(res) == 0) {
            s <-
              files() %>%
              filter(Date > input$date_range[1]) %>%
              filter(Date < input$date_range[2]) %>%
              dplyr::select("paths") %>%
              pull() %>%
              raster::stack()

            path_min <- glue(
              "./data/",
              tabAOIInput()$aoi,
              "/minimum/minimum-",
              tabAOIInput()$uuid(),
              ".tif"
            )

            path_max <- glue(
              "./data/",
              tabAOIInput()$aoi,
              "/maximum/maximum-",
              tabAOIInput()$uuid(),
              ".tif"
            )

            if (input$parallel) {
              # Parallel calculation with tsar package
              incProgress(0.2, detail = "Minimum")
              
              tsar(s, 
                   workers=list(minimum=function(x)return(min(x, na.rm=T))), 
                   cores=4, 
                   out.name=path_min, 
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
                   compress_tif = F)
              
              temporal_statistics[["minimum"]] <- raster(path_min)
              
              incProgress(0.6, detail = "Maximum")
              
              tsar(s, 
                   workers=list(maximum=function(x)return(max(x, na.rm=T))), 
                   cores=4, 
                   out.name=path_max, 
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
                   compress_tif = F)
              
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
                             start_time, 
                             end_time, 
                             path_min, 
                             path_max) VALUES (\'",
              strftime(input$date_range[1], "%Y-%m-%dT%H:%M:%S%z"),
              "\', \'",
              strftime(input$date_range[2], "%Y-%m-%dT%H:%M:%S%z"),
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
    }
  )

  tabProcessingOutput <- reactive({
    #' Module ouput
    #' 
    list(
      temporal_statistics = temporal_statistics
    )
  })

  return(tabProcessingOutput)
}
