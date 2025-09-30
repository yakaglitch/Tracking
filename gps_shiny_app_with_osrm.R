# gps_logger_shiny_app.R
# Final-Version mit: Farbliche Geschwindigkeitsanzeige, OSRM-Routen, Zeitspalten, Autozoom, Layout 50/50, hellblau >70 km/h

library(shiny)
library(leaflet)
library(data.table)
library(DT)
library(stringr)
library(RColorBrewer)
library(geosphere)
library(htmltools)

# ===========================
# Initialisierung des App-Verzeichnisses und Startskripte
# ===========================

detect_app_root <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, cmd_args, fixed = TRUE)
  if (length(file_idx) > 0) {
    candidate <- sub(file_arg, "", cmd_args[file_idx[1]])
    if (nzchar(candidate)) {
      candidate_path <- tryCatch(normalizePath(candidate, mustWork = TRUE), error = function(...) NULL)
      if (!is.null(candidate_path)) {
        return(dirname(candidate_path))
      }
    }
  }
  
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (!is.null(frame$ofile)) {
      candidate_path <- tryCatch(normalizePath(frame$ofile, mustWork = TRUE), error = function(...) NULL)
      if (!is.null(candidate_path)) {
        return(dirname(candidate_path))
      }
    }
  }
  
  tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(...) getwd())
}

app_root_dir <- detect_app_root()
options(gps_app.root_dir = app_root_dir)

resolve_app_path <- function(...) {
  file.path(app_root_dir, ...)
}

run_app_startup_scripts <- local({
  executed <- FALSE
  
  function(force = FALSE) {
    if (!force && isTRUE(executed)) {
      return(invisible(TRUE))
    }
    
    scripts <- c(
      "gps_loading_files.R",
      "travel_distance_add.R",
      "osrm_distance.R",
      "POI_init.R"
    )
    
    for (script in scripts) {
      script_path <- resolve_app_path(script)
      
      if (!file.exists(script_path)) {
        message(sprintf("⚠️  Startskript nicht gefunden: %s", script_path))
        next
      }
      
      message(sprintf("▶️  Führe Startskript aus: %s", script))
      tryCatch(
        source(script_path, local = .GlobalEnv),
        error = function(err) {
          warning(sprintf("Fehler beim Ausführen von %s: %s", script, conditionMessage(err)), call. = FALSE)
        }
      )
    }
    
    executed <<- TRUE
    invisible(TRUE)
  }
})

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

track_cache_env <- new.env(parent = emptyenv())
osrm_cache_env  <- new.env(parent = emptyenv())

get_cached_track_coords <- function(track_id, gps_data_env) {
  cache_key <- paste0("track_", track_id)
  if (exists(cache_key, envir = track_cache_env, inherits = FALSE)) {
    return(track_cache_env[[cache_key]])
  }
  
  lines <- gps_data_env[[cache_key]]
  if (is.null(lines)) return(NULL)
  
  coords <- parse_nmea_with_speed(lines)
  track_cache_env[[cache_key]] <- coords
  coords
}

get_cached_osrm_coords <- function(track_id, gps_index_dt) {
  cache_key <- paste0("route_", track_id)
  if (exists(cache_key, envir = osrm_cache_env, inherits = FALSE)) {
    return(osrm_cache_env[[cache_key]])
  }
  
  if (!"osrm_route_coords" %in% names(gps_index_dt)) return(NULL)
  
  route_raw <- gps_index_dt[id == track_id]$osrm_route_coords[[1]]
  coords <- parse_osrm_route(route_raw)
  osrm_cache_env[[cache_key]] <- coords
  coords
}

add_speed_segments_to_map <- function(proxy, coords) {
  if (is.null(coords) || nrow(coords) < 2) return(proxy)
  
  segment_colors <- vapply(coords$speed[-1], get_speed_color, character(1))
  if (!length(segment_colors)) return(proxy)
  
  color_rle <- rle(segment_colors)
  start_idx <- 1
  
  for (i in seq_along(color_rle$values)) {
    segment_length <- color_rle$lengths[i]
    end_idx <- start_idx + segment_length
    idx_range <- start_idx:end_idx
    
    proxy <- proxy |> addPolylines(
      lng = coords$lon[idx_range],
      lat = coords$lat[idx_range],
      color = color_rle$values[i],
      weight = 4,
      opacity = 0.9,
      group = "gps_tracks"
    )
    
    start_idx <- end_idx
  }
  
  proxy
}

extract_lat_lon <- function(coord_entry) {
  if (is.null(coord_entry)) {
    return(c(NA_real_, NA_real_))
  }
  
  lat <- NA_real_
  lon <- NA_real_
  
  if (is.list(coord_entry) && !is.data.frame(coord_entry)) {
    if (!is.null(coord_entry$lat) && !is.null(coord_entry$lon)) {
      lat <- suppressWarnings(as.numeric(coord_entry$lat[1]))
      lon <- suppressWarnings(as.numeric(coord_entry$lon[1]))
    } else if (length(coord_entry) >= 2) {
      lat <- suppressWarnings(as.numeric(coord_entry[[1]]))
      lon <- suppressWarnings(as.numeric(coord_entry[[2]]))
    }
  } else if (is.list(coord_entry) && is.data.frame(coord_entry)) {
    if (all(c("lat", "lon") %in% names(coord_entry))) {
      lat <- suppressWarnings(as.numeric(coord_entry$lat[1]))
      lon <- suppressWarnings(as.numeric(coord_entry$lon[1]))
    }
  } else if (is.atomic(coord_entry) && length(coord_entry) >= 2) {
    lat <- suppressWarnings(as.numeric(coord_entry[1]))
    lon <- suppressWarnings(as.numeric(coord_entry[2]))
  }
  
  c(lat, lon)
}

prepare_poi_choices <- function(ref_lat, ref_lon, poi_dt) {
  if (nrow(poi_dt) == 0) {
    return(data.table(id = integer(), name = character(), dist_m = numeric()))
  }
  
  poi_valid <- copy(poi_dt[!is.na(lat) & !is.na(lon)])
  if (nrow(poi_valid) == 0 || any(is.na(c(ref_lat, ref_lon)))) {
    ordered_idx <- order(poi_dt$name)
    return(data.table(
      id = poi_dt$id[ordered_idx],
      name = poi_dt$name[ordered_idx],
      dist_m = rep(NA_real_, length(ordered_idx))
    ))
  }
  
  coords <- matrix(c(poi_valid$lon, poi_valid$lat), ncol = 2)
  distances <- distHaversine(coords, c(ref_lon, ref_lat))
  poi_valid[, dist_m := distances]
  ordered <- rbindlist(list(
    poi_valid[order(dist_m, na.last = TRUE)],
    copy(poi_dt[is.na(lat) | is.na(lon)])
  ), fill = TRUE)
  
  if (!"dist_m" %in% names(ordered)) {
    ordered[, dist_m := NA_real_]
  }
  
  ordered[, .(id = id, name = name, dist_m = dist_m)]
}

create_poi_dropdown <- function(track_id, field, selected_id, coord_entry, poi_dt) {
  lat_lon <- extract_lat_lon(coord_entry)
  choices <- prepare_poi_choices(lat_lon[1], lat_lon[2], poi_dt)
  
  option_html <- '<option value="">(unbekannt)</option>'
  if (nrow(choices) > 0) {
    option_html <- c(option_html, vapply(seq_len(nrow(choices)), function(i) {
      poi_id <- choices$id[i]
      poi_name <- choices$name[i]
      dist_val <- choices$dist_m[i]
      label <- htmltools::htmlEscape(poi_name)
      if (!is.na(dist_val)) {
        label <- sprintf("%s (%.0f m)", label, dist_val)
      }
      selected_attr <- if (!is.na(selected_id) && poi_id == selected_id) " selected" else ""
      sprintf('<option value="%s"%s>%s</option>', poi_id, selected_attr, label)
    }, character(1)))
  } else {
    if (!is.na(selected_id)) {
      option_html <- c(option_html, sprintf('<option value="%s" selected>(unbekannt)</option>', selected_id))
    }
  }
  
  if (!is.na(selected_id) && !(selected_id %in% choices$id)) {
    fallback_label <- sprintf("Manuelle Auswahl (ID %s)", selected_id)
    option_html <- c(option_html, sprintf('<option value="%s" selected>%s</option>', selected_id, htmltools::htmlEscape(fallback_label)))
  }
  
  sprintf(
    '<select class="poi-select" data-track-id="%s" data-field="%s">%s</select>',
    track_id,
    field,
    paste(option_html, collapse = "")
  )
}

route_usage_values <- c("offen", "dienst", "steuer")
route_usage_labels <- c(
  offen = "(offen) nicht verwendet",
  dienst = "Dienstkostenabrechnung",
  steuer = "Steuerwerbungskosten"
)

distance_source_values <- c("gps", "osrm", "manual")
distance_source_labels <- c(
  gps = "GPS",
  osrm = "OSRM",
  manual = "Manuell"
)

create_usage_dropdown <- function(track_id, selected_value) {
  if (is.na(selected_value) || !selected_value %in% route_usage_values) {
    selected_value <- "offen"
  }
  
  option_html <- vapply(route_usage_values, function(value) {
    label <- route_usage_labels[[value]]
    selected_attr <- if (identical(value, selected_value)) " selected" else ""
    sprintf('<option value="%s"%s>%s</option>', value, selected_attr, htmltools::htmlEscape(label))
  }, character(1))
  
  sprintf(
    '<select class="usage-select" data-track-id="%s">%s</select>',
    track_id,
    paste(option_html, collapse = "")
  )
}

create_distance_control <- function(track_id, source_value, manual_value) {
  if (is.na(source_value) || !source_value %in% distance_source_values) {
    source_value <- "gps"
  }
  
  manual_value <- suppressWarnings(as.numeric(manual_value))
  manual_display <- ifelse(
    is.na(manual_value),
    "",
    formatC(manual_value, format = "f", digits = 2)
  )
  
  option_html <- vapply(distance_source_values, function(value) {
    label <- distance_source_labels[[value]]
    selected_attr <- if (identical(value, source_value)) " selected" else ""
    sprintf('<option value="%s"%s>%s</option>', value, selected_attr, htmltools::htmlEscape(label))
  }, character(1))
  
  manual_disabled <- if (identical(source_value, "manual")) "" else " disabled"
  
  sprintf(
    paste0(
      '<div class="distance-control" data-track-id="%s">',
      '<select class="distance-source" data-track-id="%s">%s</select>',
      '<input type="text" class="manual-distance" data-track-id="%s" value="%s" placeholder="km"%s>',
      '</div>'
    ),
    track_id,
    track_id,
    paste(option_html, collapse = ""),
    track_id,
    htmltools::htmlEscape(manual_display),
    manual_disabled
  )
}

format_minutes_to_hm <- function(minutes_total) {
  minutes_total <- suppressWarnings(as.numeric(minutes_total))
  if (is.na(minutes_total) || minutes_total <= 0) {
    return("00:00")
  }
  
  hours <- floor(minutes_total / 60)
  minutes <- round(minutes_total - hours * 60)
  sprintf("%02d:%02d", hours, minutes)
}

workspace_state_path <- resolve_app_path("workspace_state.rds")

run_app_startup_scripts()

empty_workspace_state <- function() {
  data.table(
    id = integer(),
    route_usage = character(),
    route_continued = logical(),
    distance_source = character(),
    manual_distance_km = numeric(),
    updated_at = as.POSIXct(character())
  )
}

load_workspace_state <- function(path) {
  if (!file.exists(path)) {
    return(empty_workspace_state())
  }
  
  state <- tryCatch(readRDS(path), error = function(...) NULL)
  if (is.null(state) || !is.data.table(state)) {
    return(empty_workspace_state())
  }
  
  if (!"route_usage" %in% names(state)) {
    state[, route_usage := "offen"]
  }
  
  if (!"route_continued" %in% names(state)) {
    state[, route_continued := FALSE]
  }
  
  if (!"distance_source" %in% names(state)) {
    state[, distance_source := "gps"]
  }
  
  if (!"manual_distance_km" %in% names(state)) {
    state[, manual_distance_km := NA_real_]
  }
  
  if (!"updated_at" %in% names(state)) {
    state[, updated_at := as.POSIXct(character())]
  }
  
  state[, route_usage := ifelse(route_usage %in% route_usage_values, route_usage, "offen")]
  state[, route_continued := as.logical(route_continued)]
  state[, distance_source := ifelse(distance_source %in% distance_source_values, distance_source, "gps")]
  state[, manual_distance_km := suppressWarnings(as.numeric(manual_distance_km))]
  
  setcolorder(state, c("id", "route_usage", "route_continued", "distance_source", "manual_distance_km", "updated_at"))
  state
}

save_workspace_state <- function(state_dt, path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(state_dt, path)
}

upsert_workspace_state <- function(
    state_dt,
    track_id,
    usage_value = NULL,
    continued = NULL,
    distance_source = NULL,
    manual_distance_km = NULL
) {
  if (!is.data.table(state_dt)) {
    state_dt <- as.data.table(state_dt)
  }
  
  if (!"route_usage" %in% names(state_dt)) {
    state_dt[, route_usage := "offen"]
  }
  
  if (!"route_continued" %in% names(state_dt)) {
    state_dt[, route_continued := FALSE]
  }
  
  if (!"distance_source" %in% names(state_dt)) {
    state_dt[, distance_source := "gps"]
  }
  
  if (!"manual_distance_km" %in% names(state_dt)) {
    state_dt[, manual_distance_km := NA_real_]
  }
  
  if (!"updated_at" %in% names(state_dt)) {
    state_dt[, updated_at := as.POSIXct(character())]
  }
  
  if (!is.null(usage_value) && !usage_value %in% route_usage_values) {
    usage_value <- "offen"
  }
  
  if (!is.null(distance_source) && !distance_source %in% distance_source_values) {
    distance_source <- "gps"
  }
  
  if (!is.null(manual_distance_km)) {
    manual_distance_km <- suppressWarnings(as.numeric(manual_distance_km))
  }
  
  if (!is.null(continued)) {
    continued <- isTRUE(continued)
  }
  
  idx <- match(track_id, state_dt$id)
  now_ts <- Sys.time()
  
  if (is.na(idx)) {
    new_row <- data.table(
      id = track_id,
      route_usage = if (is.null(usage_value)) "offen" else usage_value,
      route_continued = if (is.null(continued)) FALSE else continued,
      distance_source = if (is.null(distance_source)) "gps" else distance_source,
      manual_distance_km = if (is.null(manual_distance_km)) NA_real_ else manual_distance_km,
      updated_at = now_ts
    )
    state_dt <- rbind(state_dt, new_row, fill = TRUE)
  } else {
    updates <- list(updated_at = now_ts)
    if (!is.null(usage_value)) {
      updates$route_usage <- usage_value
    }
    if (!is.null(continued)) {
      updates$route_continued <- continued
    }
    if (!is.null(distance_source)) {
      updates$distance_source <- distance_source
    }
    if (!is.null(manual_distance_km)) {
      updates$manual_distance_km <- manual_distance_km
    }
    state_dt[idx, names(updates) := updates]
  }
  
  setcolorder(state_dt, c("id", "route_usage", "route_continued", "distance_source", "manual_distance_km", "updated_at"))
  state_dt
}

# ===========================
# UI
# ===========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".dataTables_wrapper td:nth-child(2) { white-space: nowrap; width: 100px; }")),
    tags$style(HTML("select.poi-select, select.usage-select { width: 190px; }"))
  ),
  titlePanel("GPS-Logger Viewer mit OSRM & Geschwindigkeitsfarben"),
  fluidRow(
    column(width = 7,
           selectInput("day", "Wähle einen Tag:", choices = NULL),
           DTOutput("track_table"),
           div(
             style = "margin-top: 10px;",
             actionButton("daily_report", "Dienstkostenabrechnung für diesen Tag", class = "btn-primary")
           )
    ),
    column(width = 5,
           leafletOutput("map", height = 700),
           div(style = "margin-top: 10px;",
               checkboxInput("show_poi", "Points of Interest einblenden", TRUE)
           ),
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
  
  gps_index_initial <- copy(get("gps_index", envir = .GlobalEnv))
  gps_data          <- get("gps_data",  envir = .GlobalEnv)
  
  workspace_state_initial <- load_workspace_state(workspace_state_path)
  workspace_state_rv <- reactiveVal(workspace_state_initial)
  
  if (!"route_usage" %in% names(gps_index_initial)) {
    gps_index_initial[, route_usage := "offen"]
  }
  gps_index_initial[is.na(route_usage) | !(route_usage %in% route_usage_values), route_usage := "offen"]
  
  if (!"route_continued" %in% names(gps_index_initial)) {
    gps_index_initial[, route_continued := FALSE]
  }
  gps_index_initial[, route_continued := as.logical(route_continued)]
  gps_index_initial[is.na(route_continued), route_continued := FALSE]
  
  if (!"distance_source" %in% names(gps_index_initial)) {
    gps_index_initial[, distance_source := "gps"]
  }
  gps_index_initial[is.na(distance_source) | !(distance_source %in% distance_source_values), distance_source := "gps"]
  
  if (!"manual_distance_km" %in% names(gps_index_initial)) {
    gps_index_initial[, manual_distance_km := NA_real_]
  }
  gps_index_initial[, manual_distance_km := suppressWarnings(as.numeric(manual_distance_km))]
  
  if (nrow(workspace_state_initial) > 0 && "id" %in% names(gps_index_initial)) {
    match_idx <- match(gps_index_initial$id, workspace_state_initial$id)
    valid_idx <- which(!is.na(match_idx))
    if (length(valid_idx) > 0) {
      matched_usage <- workspace_state_initial$route_usage[match_idx[valid_idx]]
      matched_usage[is.na(matched_usage) | !(matched_usage %in% route_usage_values)] <- "offen"
      gps_index_initial[valid_idx, route_usage := matched_usage]
      
      matched_continued <- as.logical(workspace_state_initial$route_continued[match_idx[valid_idx]])
      matched_continued[is.na(matched_continued)] <- FALSE
      gps_index_initial[valid_idx, route_continued := matched_continued]
      
      matched_source <- workspace_state_initial$distance_source[match_idx[valid_idx]]
      matched_source[is.na(matched_source) | !(matched_source %in% distance_source_values)] <- "gps"
      gps_index_initial[valid_idx, distance_source := matched_source]
      
      matched_manual <- workspace_state_initial$manual_distance_km[match_idx[valid_idx]]
      gps_index_initial[valid_idx, manual_distance_km := suppressWarnings(as.numeric(matched_manual))]
    }
  }
  
  # POI-Lookup vorbereiten
  poi_warning <- NULL
  
  if (exists("poi_table", envir = .GlobalEnv)) {
    poi_table <- get("poi_table", envir = .GlobalEnv)
    if (all(c("id", "name") %in% names(poi_table))) {
      poi_table[, poi_color := "blue"]
      
      kategorie_col <- names(poi_table)[tolower(names(poi_table)) == "kategorie"]
      if (length(kategorie_col) > 0) {
        kategorie_clean <- tolower(trimws(poi_table[[kategorie_col[1]]]))
        poi_table[!is.na(kategorie_clean) & kategorie_clean == "dienstlich", poi_color := "red"]
      }
      
      typ_col <- names(poi_table)[tolower(names(poi_table)) == "typ"]
      if (length(typ_col) > 0) {
        typ_clean <- tolower(trimws(poi_table[[typ_col[1]]]))
        poi_table[!is.na(typ_clean) & typ_clean == "zuhause" & poi_color != "red", poi_color := "green"]
      }
      
      if (!"lat" %in% names(poi_table)) poi_table[, lat := NA_real_]
      if (!"lon" %in% names(poi_table)) poi_table[, lon := NA_real_]
      if (!"range" %in% names(poi_table)) poi_table[, range := NA_real_]
      poi_table[, lat := suppressWarnings(as.numeric(lat))]
      poi_table[, lon := suppressWarnings(as.numeric(lon))]
      poi_table[, range := suppressWarnings(as.numeric(range))]
    } else {
      poi_warning <- "POI-Tabelle ohne 'id'/'name'-Spalten gefunden. Es wird eine leere Tabelle verwendet."
      poi_table   <- data.table(id = integer(), name = character(), poi_color = character(), lat = numeric(), lon = numeric(), range = numeric())
    }
  } else {
    poi_warning <- "POI-Tabelle nicht gefunden. Es wird eine leere Tabelle verwendet."
    poi_table   <- data.table(id = integer(), name = character(), poi_color = character(), lat = numeric(), lon = numeric(), range = numeric())
  }
  
  if (!is.null(poi_warning)) {
    showNotification(poi_warning, type = "warning")
  }
  
  # Tag-Spalte für Auswahl aufbereiten
  gps_index_initial[, tag := format(start_time, "%Y-%m-%d")]
  gps_index_rv <- reactiveVal(gps_index_initial)
  assign("gps_index", gps_index_initial, envir = .GlobalEnv)
  updateSelectInput(
    session,
    "day",
    choices  = unique(gps_index_initial$tag),
    selected = unique(gps_index_initial$tag)[1]
  )
  
  selected_table <- reactiveVal(NULL)
  
  day_subset <- reactive({
    req(input$day)
    gps_index_rv()[tag == input$day]
  })
  
  output$track_table <- renderDT({
    df <- copy(day_subset())
    if (nrow(df) == 0) return(NULL)
    
    setorder(df, start_time)
    df[, index := .I - 1]
    df[, `Fahrtzeit UTC` := paste(format(start_time, "%H:%M"), "–", format(end_time, "%H:%M"))]
    df[, `GPS [km]` := sprintf("%.1f", travel_distance_m / 1000)]
    df[, `OSRM [km]` := sprintf("%.1f", osrm_distance_m / 1000)]
    df[, `OSRM-Zeit` := osrm_duration_hm]
    df[, `Ist-Zeit` := real_duration_hm]
    
    usage_dropdowns <- vapply(seq_len(nrow(df)), function(i) {
      create_usage_dropdown(df$id[i], df$route_usage[i])
    }, character(1))
    
    continuation_checkboxes <- vapply(seq_len(nrow(df)), function(i) {
      checked_attr <- if (isTRUE(df$route_continued[i])) " checked" else ""
      sprintf('<input type="checkbox" class="toggle-checkbox continuation-checkbox" id="continued_%d"%s>', df$index[i], checked_attr)
    }, character(1))
    
    distance_controls <- vapply(seq_len(nrow(df)), function(i) {
      create_distance_control(df$id[i], df$distance_source[i], df$manual_distance_km[i])
    }, character(1))
    
    start_dropdowns <- vapply(seq_len(nrow(df)), function(i) {
      create_poi_dropdown(
        track_id = df$id[i],
        field = "start",
        selected_id = df$poi_start_id[i],
        coord_entry = df$start_coord[[i]],
        poi_dt = poi_table
      )
    }, character(1))
    
    end_dropdowns <- vapply(seq_len(nrow(df)), function(i) {
      create_poi_dropdown(
        track_id = df$id[i],
        field = "end",
        selected_id = df$poi_end_id[i],
        coord_entry = df$end_coord[[i]],
        poi_dt = poi_table
      )
    }, character(1))
    
    df[, StartOrt := start_dropdowns]
    df[, ZielOrt := end_dropdowns]
    df[, Kategorie := usage_dropdowns]
    df[, `Fortgesetzte Fahrt` := continuation_checkboxes]
    df[, `Kilometer-Auswahl` := distance_controls]
    
    df[, `Anzeigen` := sprintf('<input type="checkbox" class="toggle-checkbox anzeigen-checkbox" id="anzeigen_%d">', index)]
    df[, `OSRM-Route` := sprintf('<input type="checkbox" class="toggle-checkbox osrm-checkbox" id="osrm_%d">', index)]
    
    df <- df[, .(
      `Anzeigen`,
      `Fahrtzeit UTC`,
      StartOrt,
      ZielOrt,
      Kategorie,
      `Fortgesetzte Fahrt`,
      `Kilometer-Auswahl`,
      `GPS [km]`,
      `OSRM [km]`,
      `OSRM-Zeit`,
      `Ist-Zeit`,
      `OSRM-Route`
    )]
    
    datatable(
      df,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(dom = 'tip', paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      callback = JS(
        "function sendRowState() {\n  var data = {};\n  table.rows().every(function(rowIdx) {\n    var row = this.node();\n    var anzeigen = $(row).find('input.anzeigen-checkbox').prop('checked');\n    var osrm = $(row).find('input.osrm-checkbox').prop('checked');\n    var continued = $(row).find('input.continuation-checkbox').prop('checked');\n    var source = $(row).find('select.distance-source').val() || 'gps';\n    var manual = $(row).find('input.manual-distance').val();\n    data[rowIdx] = { anzeigen: !!anzeigen, osrm: !!osrm, continued: !!continued, distance_source: source, manual_distance: manual };\n  });\n  Shiny.setInputValue('row_state', data, {priority: 'event'});\n}\n\nfunction toggleManualInput(selectEl) {\n  if (!selectEl) {\n    return;\n  }\n  var select = $(selectEl);\n  var manualInput = select.closest('.distance-control').find('input.manual-distance');\n  if (select.val() === 'manual') {\n    manualInput.prop('disabled', false);\n  } else {\n    manualInput.prop('disabled', true);\n  }\n}\n\nsetTimeout(sendRowState, 0);\n\ntable.on('change', 'input.toggle-checkbox, select.distance-source, input.manual-distance', function() {\n  if ($(this).hasClass('distance-source')) {\n    toggleManualInput(this);\n  }\n  sendRowState();\n});\n\ntable.on('draw.dt', function() {\n  table.rows().every(function() {\n    var row = this.node();\n    var select = $(row).find('select.distance-source')[0];\n    toggleManualInput(select);\n  });\n  sendRowState();\n});\n\ntable.on('change', 'select.poi-select', function() {\n  var select = $(this);\n  var trackId = parseInt(select.data('track-id'), 10);\n  var field = select.data('field');\n  var value = select.val();\n  if (isNaN(trackId)) {\n    return;\n  }\n  Shiny.setInputValue('poi_update', {\n    track_id: trackId,\n    field: field,\n    value: value || ''\n  }, {priority: 'event'});\n});\n\ntable.on('change', 'select.usage-select', function() {\n  var select = $(this);\n  var trackId = parseInt(select.data('track-id'), 10);\n  var value = select.val() || 'offen';\n  if (isNaN(trackId)) {\n    return;\n  }\n  Shiny.setInputValue('usage_update', {\n    track_id: trackId,\n    value: value\n  }, {priority: 'event'});\n});"
      )
    ) %>%
      formatStyle("Fahrtzeit UTC", target = 'row', fontWeight = 'bold')
  })
  
  observeEvent(day_subset(), {
    df <- day_subset()
    if (nrow(df) == 0) {
      selected_table(NULL)
    } else {
      selected_table(data.table(
        id = df$id,
        anzeigen = FALSE,
        osrm = FALSE,
        continued = df$route_continued,
        distance_source = df$distance_source,
        manual_distance = df$manual_distance_km
      ))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$row_state, {
    df <- day_subset()
    if (nrow(df) == 0) {
      selected_table(NULL)
      return()
    }
    
    raw <- input$row_state
    anzeigen_flags <- rep(FALSE, nrow(df))
    osrm_flags <- rep(FALSE, nrow(df))
    continued_flags <- df$route_continued
    source_values <- df$distance_source
    manual_values <- df$manual_distance_km
    
    if (!is.null(raw)) {
      for (name in names(raw)) {
        idx_zero <- suppressWarnings(as.integer(name))
        if (is.na(idx_zero)) next
        row_idx <- idx_zero + 1
        if (row_idx < 1 || row_idx > nrow(df)) next
        
        entry <- raw[[name]]
        if (is.null(entry)) next
        
        anzeigen_flags[row_idx] <- isTRUE(entry$anzeigen)
        osrm_flags[row_idx]     <- isTRUE(entry$osrm)
        continued_flags[row_idx] <- isTRUE(entry$continued)
        
        if (!is.null(entry$distance_source)) {
          value <- as.character(entry$distance_source)
          if (!value %in% distance_source_values) {
            value <- "gps"
          }
          source_values[row_idx] <- value
        }
        
        if (!is.null(entry$manual_distance)) {
          manual_num <- suppressWarnings(as.numeric(entry$manual_distance))
          if (is.na(manual_num)) {
            manual_values[row_idx] <- NA_real_
          } else {
            manual_values[row_idx] <- manual_num
          }
        }
      }
    }
    
    selected_table(data.table(
      id = df$id,
      anzeigen = anzeigen_flags,
      osrm = osrm_flags,
      continued = continued_flags,
      distance_source = source_values,
      manual_distance = manual_values
    ))
    
    current_index <- copy(gps_index_rv())
    idx_map <- match(df$id, current_index$id)
    valid_rows <- which(!is.na(idx_map))
    if (length(valid_rows) > 0) {
      current_index[idx_map[valid_rows], `:=`(
        route_continued = continued_flags[valid_rows],
        distance_source = source_values[valid_rows],
        manual_distance_km = manual_values[valid_rows]
      )]
      gps_index_rv(current_index)
      assign("gps_index", current_index, envir = .GlobalEnv)
      
      workspace_state <- workspace_state_rv()
      for (i in valid_rows) {
        track_id <- df$id[i]
        workspace_state <- upsert_workspace_state(
          workspace_state,
          track_id,
          continued = continued_flags[i],
          distance_source = source_values[i],
          manual_distance_km = manual_values[i]
        )
      }
      workspace_state_rv(workspace_state)
      save_workspace_state(workspace_state, workspace_state_path)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$poi_update, {
    update <- input$poi_update
    req(update$track_id, update$field)
    
    track_id <- as.integer(update$track_id)
    if (is.na(track_id)) return()
    
    field_name <- if (identical(update$field, "start")) "poi_start_id" else "poi_end_id"
    score_name <- if (identical(update$field, "start")) "poi_start_score" else "poi_end_score"
    coord_col  <- if (identical(update$field, "start")) "start_coord" else "end_coord"
    
    new_id <- suppressWarnings(as.integer(update$value))
    if (is.na(new_id)) {
      new_id <- NA_integer_
    }
    
    current_index <- copy(gps_index_rv())
    if (!track_id %in% current_index$id) return()
    
    current_index[id == track_id, (field_name) := new_id]
    
    new_score <- NA_real_
    if (!is.na(new_id) && nrow(poi_table) > 0) {
      poi_match <- poi_table[id == new_id]
      if (nrow(poi_match) > 0) {
        coord_list <- current_index[id == track_id][[coord_col]]
        coord_entry <- if (length(coord_list) > 0) coord_list[[1]] else NULL
        lat_lon <- extract_lat_lon(coord_entry)
        if (!any(is.na(lat_lon))) {
          poi_lat <- suppressWarnings(as.numeric(poi_match$lat[1]))
          poi_lon <- suppressWarnings(as.numeric(poi_match$lon[1]))
          if (!any(is.na(c(poi_lat, poi_lon)))) {
            dist_val <- distHaversine(c(poi_lon, poi_lat), c(lat_lon[2], lat_lon[1]))
            poi_range <- suppressWarnings(as.numeric(poi_match$range[1]))
            if (!is.na(poi_range) && poi_range > 0) {
              new_score <- round((1 - dist_val / poi_range) * 100, 1)
            }
          }
        }
      }
    }
    
    if (!is.na(new_score)) {
      new_score <- max(min(new_score, 100), 0)
    }
    
    current_index[id == track_id, (score_name) := new_score]
    gps_index_rv(current_index)
    assign("gps_index", current_index, envir = .GlobalEnv)
  })
  
  observeEvent(input$usage_update, {
    update <- input$usage_update
    req(update$track_id)
    
    track_id <- suppressWarnings(as.integer(update$track_id))
    if (is.na(track_id)) return()
    
    usage_value <- update$value
    if (is.null(usage_value) || !usage_value %in% route_usage_values) {
      usage_value <- "offen"
    }
    
    current_index <- copy(gps_index_rv())
    if (!track_id %in% current_index$id) return()
    
    current_index[id == track_id, route_usage := usage_value]
    gps_index_rv(current_index)
    assign("gps_index", current_index, envir = .GlobalEnv)
    
    workspace_state <- upsert_workspace_state(
      workspace_state_rv(),
      track_id,
      usage_value = usage_value
    )
    workspace_state_rv(workspace_state)
    save_workspace_state(workspace_state, workspace_state_path)
  })
  
  poi_name_lookup <- function(poi_id) {
    if (is.null(poi_id) || all(is.na(poi_id))) {
      return("(unbekannt)")
    }
    
    poi_id_num <- suppressWarnings(as.integer(poi_id[1]))
    if (is.na(poi_id_num)) {
      return("(unbekannt)")
    }
    
    match_row <- poi_table[id == poi_id_num]
    if (nrow(match_row) > 0 && !is.na(match_row$name[1])) {
      return(as.character(match_row$name[1]))
    }
    
    sprintf("POI #%s", poi_id_num)
  }
  
  observeEvent(input$daily_report, {
    df <- copy(day_subset())
    if (nrow(df) == 0) {
      cat(sprintf("\n[Dienstkostenabrechnung] %s: Keine Fahrten gefunden.\n", input$day))
      return()
    }
    
    dienst_df <- df[route_usage == "dienst"]
    if (nrow(dienst_df) == 0) {
      cat(sprintf("\n[Dienstkostenabrechnung] %s: Keine Fahrten für die Dienstkostenabrechnung markiert.\n", input$day))
      return()
    }
    
    start_names <- vapply(dienst_df$poi_start_id, poi_name_lookup, character(1))
    end_names   <- vapply(dienst_df$poi_end_id, poi_name_lookup, character(1))
    
    gps_distances <- suppressWarnings(as.numeric(dienst_df$travel_distance_m) / 1000)
    osrm_distances <- suppressWarnings(as.numeric(dienst_df$osrm_distance_m) / 1000)
    manual_entries <- suppressWarnings(as.numeric(dienst_df$manual_distance_km))
    distance_sources <- dienst_df$distance_source
    
    gps_distances[is.na(gps_distances)] <- 0
    osrm_distances[is.na(osrm_distances)] <- 0
    
    distance_sources[is.na(distance_sources) | !(distance_sources %in% distance_source_values)] <- "gps"
    
    distance_values <- numeric(nrow(dienst_df))
    distance_values[distance_sources == "gps"] <- gps_distances[distance_sources == "gps"]
    distance_values[distance_sources == "osrm"] <- osrm_distances[distance_sources == "osrm"]
    
    manual_idx <- which(distance_sources == "manual")
    if (length(manual_idx) > 0) {
      manual_clean <- manual_entries[manual_idx]
      manual_clean[is.na(manual_clean)] <- 0
      distance_values[manual_idx] <- manual_clean
    }
    
    continued_flags <- dienst_df$route_continued
    continued_flags <- as.logical(continued_flags)
    continued_flags[is.na(continued_flags)] <- FALSE
    
    group_starts <- !continued_flags
    if (length(group_starts) > 0) {
      group_starts[1] <- TRUE
    }
    group_id <- cumsum(group_starts)
    
    duration_minutes <- suppressWarnings(as.numeric(difftime(dienst_df$end_time, dienst_df$start_time, units = "mins")))
    duration_minutes[is.na(duration_minutes)] <- 0
    
    source_descriptions <- vapply(seq_len(nrow(dienst_df)), function(i) {
      src <- distance_sources[i]
      label <- distance_source_labels[[src]]
      if (identical(src, "manual")) {
        entry <- manual_entries[i]
        if (is.na(entry)) {
          paste(label, "(kein Wert)")
        } else {
          sprintf("%s (%.1f km)", label, entry)
        }
      } else {
        label
      }
    }, character(1))
    
    dienst_df[, `:=`(
      group_id = group_id,
      start_name = start_names,
      end_name = end_names,
      distance_value = distance_values,
      source_description = source_descriptions,
      duration_minutes = duration_minutes
    )]
    
    report_dt <- dienst_df[, .(
      Datum = format(start_time[1], "%Y-%m-%d"),
      `Fahrtzeit` = paste(format(start_time[1], "%H:%M"), "–", format(end_time[.N], "%H:%M")),
      Start = start_name[1],
      Ziel = end_name[.N],
      Kilometer = sprintf("%.1f", sum(distance_value, na.rm = TRUE)),
      `Kilometer-Quelle` = {
        unique_sources <- unique(source_description)
        unique_sources <- unique_sources[nzchar(unique_sources)]
        if (length(unique_sources) == 0) "(keine Auswahl)" else paste(unique_sources, collapse = ", ")
      },
      `Ist-Zeit` = format_minutes_to_hm(sum(duration_minutes, na.rm = TRUE))
    ), by = group_id]
    
    total_distance <- sum(distance_values, na.rm = TRUE)
    total_minutes <- sum(duration_minutes, na.rm = TRUE)
    
    cat("\n================ Dienstkostenabrechnung =================\n")
    cat(sprintf("Tag: %s\n", input$day))
    print(report_dt)
    cat(sprintf("Gesamtstrecke: %.1f km\n", total_distance))
    cat(sprintf("Gesamte Fahrzeit: %s Stunden\n", format_minutes_to_hm(total_minutes)))
    cat("========================================================\n")
  })
  
  output$map <- renderLeaflet({
    leaflet() |> addTiles()
  })
  
  observeEvent(input$show_poi, {
    proxy <- leafletProxy("map") |> clearGroup("poi_markers")
    if (isTRUE(input$show_poi) && nrow(poi_table) > 0 && all(c("lat", "lon") %in% names(poi_table))) {
      colors <- poi_table$poi_color
      if (length(colors) == 0L) {
        colors <- rep("blue", nrow(poi_table))
      }
      proxy |> addCircleMarkers(
        lng = poi_table$lon,
        lat = poi_table$lat,
        color = colors,
        fillColor = colors,
        radius = 6,
        stroke = TRUE,
        weight = 2,
        fillOpacity = 0.8,
        opacity = 0.9,
        label = poi_table$name,
        group = "poi_markers"
      )
    }
  }, ignoreNULL = FALSE)
  
  observe({
    proxy <- leafletProxy("map")
    proxy <- proxy |> clearGroup("gps_tracks")
    proxy <- proxy |> clearGroup("osrm_routes")
    
    selection <- selected_table()
    if (is.null(selection) || nrow(selection) == 0) return()
    
    all_coords <- list()
    current_index <- gps_index_rv()
    
    for (i in seq_len(nrow(selection))) {
      sel <- selection[i]
      id <- sel$id
      
      if (isTRUE(sel$anzeigen)) {
        coords <- get_cached_track_coords(id, gps_data)
        if (!is.null(coords) && nrow(coords) > 1) {
          proxy <- add_speed_segments_to_map(proxy, coords)
          all_coords[[length(all_coords) + 1]] <- coords
        }
      }
      
      if (isTRUE(sel$osrm)) {
        coords_dt <- get_cached_osrm_coords(id, current_index)
        if (!is.null(coords_dt)) {
          proxy <- proxy |> addPolylines(
            lng = coords_dt$lon,
            lat = coords_dt$lat,
            color = "black",
            weight = 2,
            opacity = 0.6,
            dashArray = "5,5",
            group = "osrm_routes"
          )
          all_coords[[length(all_coords) + 1]] <- coords_dt
        }
      }
    }
    
    if (length(all_coords) > 0) {
      all_coords_dt <- rbindlist(all_coords, fill = TRUE)
      valid_coords <- all_coords_dt[!is.na(lat) & !is.na(lon)]
      if (nrow(valid_coords) > 0) {
        proxy |> fitBounds(
          min(valid_coords$lon, na.rm = TRUE),
          min(valid_coords$lat, na.rm = TRUE),
          max(valid_coords$lon, na.rm = TRUE),
          max(valid_coords$lat, na.rm = TRUE)
        )
      }
    }
  })
}

# ===========================
# App starten
# ===========================

shinyApp(ui, server)
