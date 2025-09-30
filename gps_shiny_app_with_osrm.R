# gps_logger_shiny_app.R
# Final-Version mit: Farbliche Geschwindigkeitsanzeige, OSRM-Routen, Zeitspalten, Autozoom, Layout 50/50, hellblau >70 km/h

library(shiny)
library(leaflet)
library(data.table)
library(DT)
library(stringr)
library(RColorBrewer)
library(geosphere)

# ===========================
# Helferfunktionen
# ===========================

parse_nmea_with_speed <- function(nmea_lines) {
  gprmc <- grep("^\\$G[NP]RMC", nmea_lines, value = TRUE)
  if (length(gprmc) == 0) return(NULL)
  
  parsed <- lapply(gprmc, function(line) {
    x <- strsplit(line, ",")[[1]]
    if (length(x) < 8 || x[3] != "A") return(NULL)
    
    lat <- as.numeric(substr(x[4], 1, 2)) + as.numeric(substr(x[4], 3, nchar(x[4]))) / 60
    if (x[5] == "S") lat <- -lat
    
    lon <- as.numeric(substr(x[6], 1, 3)) + as.numeric(substr(x[6], 4, nchar(x[6]))) / 60
    if (x[7] == "W") lon <- -lon
    
    speed_knots <- as.numeric(x[8])
    speed_kmh <- speed_knots * 1.852
    
    data.frame(lat = lat, lon = lon, speed = speed_kmh)
  })
  
  coords <- do.call(rbind, parsed)
  as.data.table(coords)
}

get_speed_color <- function(speed) {
  if (is.na(speed)) return("gray")
  colors <- c("red", "orange", "yellow", "lightgreen", "green", "lightblue", "purple")
  speed <- min(speed, 100)
  ramp <- colorRampPalette(colors, space = "Lab")
  pal  <- ramp(100)
  index <- round((speed / 100) * 99) + 1
  return(pal[index])
}

parse_osrm_route <- function(route_raw) {
  if (!is.list(route_raw)) return(NULL)
  coords_dt <- rbindlist(lapply(route_raw, function(pt) {
    list(lat = as.numeric(pt$lat), lon = as.numeric(pt$lon))
  }), fill = TRUE)
  if (nrow(coords_dt) < 2) return(NULL)
  return(coords_dt)
}

# ===========================
# UI
# ===========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".dataTables_wrapper td:nth-child(2) { white-space: nowrap; width: 100px; }"))
  ),
  titlePanel("GPS-Logger Viewer mit OSRM & Geschwindigkeitsfarben"),
  fluidRow(
    column(width = 6,
           selectInput("day", "Wähle einen Tag:", choices = NULL),
           actionButton("apply_selection", "Auf Karte anwenden"),
           DTOutput("track_table")
    ),
    column(width = 6,
           leafletOutput("map", height = 700),
           tags$div(
             style = "margin-top:10px; text-align:center;",
             tags$div(
               style = paste0(
                 "display: inline-block; width: 100%; height: 20px; ",
                 "background: linear-gradient(to right, ",
                 "red, orange, yellow, lightgreen, green, lightblue, purple);"
               )
             ),
             tags$div(
               style = "display: flex; justify-content: space-between; font-size: 12px; padding: 2px 5px;",
               tags$span("0"),
               tags$span("10"),
               tags$span("20"),
               tags$span("30"),
               tags$span("50"),
               tags$span("70"),
               tags$span("100+ km/h")
             )
           )
    )
  )
)

# ===========================
# Server
# ===========================

server <- function(input, output, session) {
  req(exists("gps_index", envir = .GlobalEnv))
  req(exists("gps_data", envir = .GlobalEnv))

  gps_index <- get("gps_index", envir = .GlobalEnv)
  gps_data  <- get("gps_data",  envir = .GlobalEnv)

  # POI-Lookup vorbereiten
  poi_lookup  <- data.table(id = integer(), name = character())
  poi_warning <- NULL

  if (exists("poi_table", envir = .GlobalEnv)) {
    poi_table <- get("poi_table", envir = .GlobalEnv)
    if (all(c("id", "name") %in% names(poi_table))) {
      poi_lookup <- poi_table[, .(id, name)]
    } else {
      poi_warning <- "POI-Tabelle ohne 'id'/'name'-Spalten gefunden. Es wird eine leere Tabelle verwendet."
      poi_table   <- data.table(id = integer(), name = character())
    }
  } else {
    poi_warning <- "POI-Tabelle nicht gefunden. Es wird eine leere Tabelle verwendet."
    poi_table   <- data.table(id = integer(), name = character())
  }

  if (!is.null(poi_warning)) {
    showNotification(poi_warning, type = "warning")
  }

  # Tag-Spalte für Auswahl aufbereiten
  gps_index[, tag := format(start_time, "%Y-%m-%d")]
  updateSelectInput(
    session,
    "day",
    choices  = unique(gps_index$tag),
    selected = unique(gps_index$tag)[1]
  )
  
  selected_table <- reactiveVal(NULL)
  
  day_subset <- reactive({
    req(input$day)
    gps_index[tag == input$day]
  })
  
  output$track_table <- renderDT({
    df <- day_subset()
    if (nrow(df) == 0) return(NULL)
    
    df[, index := .I - 1]
    df[, `Fahrtzeit UTC` := paste(format(start_time, "%H:%M"), "–", format(end_time, "%H:%M"))]
    df[, `GPS [km]` := sprintf("%.1f", travel_distance_m / 1000)]
    df[, `OSRM [km]` := sprintf("%.1f", osrm_distance_m / 1000)]
    df[, `OSRM-Zeit` := osrm_duration_hm]
    df[, `Ist-Zeit` := real_duration_hm]
    
    if (nrow(poi_lookup) > 0) {
      df <- merge(df, poi_lookup, by.x = "poi_start_id", by.y = "id", all.x = TRUE)
      setnames(df, "name", "StartOrt")
      df <- merge(df, poi_lookup, by.x = "poi_end_id", by.y = "id", all.x = TRUE)
      setnames(df, "name", "ZielOrt")
    }

    setorder(df, start_time)

    if (!"StartOrt" %in% names(df)) df[, StartOrt := NA_character_]
    if (!"ZielOrt" %in% names(df)) df[, ZielOrt := NA_character_]
    df[is.na(StartOrt), StartOrt := "(unbekannt)"]
    df[is.na(ZielOrt), ZielOrt := "(unbekannt)"]
    
    df[, `Anzeigen` := sprintf('<input type="checkbox" id="anzeigen_%d">', index)]
    df[, `OSRM-Route` := sprintf('<input type="checkbox" id="osrm_%d">', index)]
    
    df <- df[, .(`Anzeigen`, `Fahrtzeit UTC`, StartOrt, ZielOrt, `GPS [km]`, `OSRM [km]`, `OSRM-Zeit`, `Ist-Zeit`, `OSRM-Route`)]
    
    datatable(
      df,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(dom = 'tip', paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      callback = JS(
        "table.on('change', 'input', function() {
var data = {};
table.rows().every(function(rowIdx, tableLoop, rowLoop) {
var row = this.node();
var anzeigen = $(row).find('input[id^=\"anzeigen_\"]').prop('checked');
var osrm = $(row).find('input[id^=\"osrm_\"]').prop('checked');
data[rowIdx] = { anzeigen: anzeigen, osrm: osrm };
});
Shiny.setInputValue('checkbox_data', data);
});"
      )
    ) %>%
      formatStyle("Fahrtzeit UTC", target = 'row', fontWeight = 'bold')
  })
  
  observeEvent(input$apply_selection, {
    raw <- input$checkbox_data
    if (is.null(raw)) return()
    
    df <- day_subset()
    anzeigen_flags <- rep(FALSE, nrow(df))
    osrm_flags <- rep(FALSE, nrow(df))
    
    for (i in seq_len(nrow(df))) {
      entry <- raw[[as.character(i - 1)]]
      if (!is.null(entry)) {
        anzeigen_flags[i] <- isTRUE(entry$anzeigen)
        osrm_flags[i]     <- isTRUE(entry$osrm)
      }
    }
    
    selected_table(data.table(
      id = df$id,
      anzeigen = anzeigen_flags,
      osrm = osrm_flags
    ))
  })
  
  output$map <- renderLeaflet({
    leaflet() |> addTiles()
  })
  
  observe({
    leafletProxy("map") |> clearShapes()
    selection <- selected_table()
    if (is.null(selection)) return()
    
    all_coords <- list()
    
    for (i in seq_len(nrow(selection))) {
      sel <- selection[i]
      id <- sel$id
      
      if (sel$anzeigen) {
        key <- paste0("track_", id)
        lines <- gps_data[[key]]
        coords <- parse_nmea_with_speed(lines)
        
        if (!is.null(coords) && nrow(coords) > 1) {
          for (j in 2:nrow(coords)) {
            color <- get_speed_color(coords$speed[j])
            leafletProxy("map") |> addPolylines(
              lng = coords$lon[(j - 1):j],
              lat = coords$lat[(j - 1):j],
              color = color,
              weight = 4,
              opacity = 0.9
            )
          }
          all_coords[[length(all_coords) + 1]] <- coords
        }
      }
      
      if (sel$osrm) {
        route_raw <- gps_index[id == sel$id]$osrm_route_coords[[1]]
        coords_dt <- parse_osrm_route(route_raw)
        if (!is.null(coords_dt)) {
          leafletProxy("map") |> addPolylines(
            lng = coords_dt$lon,
            lat = coords_dt$lat,
            color = "black",
            weight = 2,
            opacity = 0.6,
            dashArray = "5,5"
          )
        }
      }
    }
    
    if (length(all_coords) > 0) {
      all_coords_dt <- rbindlist(all_coords)
      leafletProxy("map") |> fitBounds(
        min(all_coords_dt$lon, na.rm = TRUE),
        min(all_coords_dt$lat, na.rm = TRUE),
        max(all_coords_dt$lon, na.rm = TRUE),
        max(all_coords_dt$lat, na.rm = TRUE)
      )
    }
  })
}

# ===========================
# App starten
# ===========================

shinyApp(ui, server)
