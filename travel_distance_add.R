# gps_add_travel_distance_and_durations.R
# Erweiterte Version mit Prüfung auf Vorhandensein von osrm_duration_s

library(data.table)
library(geosphere)

# --- Sicherheitsprüfung ---
if (!exists("gps_index", envir = .GlobalEnv) || !exists("gps_data", envir = .GlobalEnv)) {
  stop("❌ gps_index oder gps_data nicht im Environment gefunden. Bitte lade zuerst die GPS-Daten.")
}

gps_index <- get("gps_index", envir = .GlobalEnv)
gps_data  <- get("gps_data",  envir = .GlobalEnv)

# --- Helper-Funktion: GPRMC-Zeilen zu Koordinaten ---
parse_coords <- function(nmea_lines) {
  gprmc <- grep("^\\$G[NP]RMC", nmea_lines, value = TRUE)
  if (length(gprmc) < 2) return(NULL)
  
  coords <- lapply(gprmc, function(line) {
    parts <- strsplit(line, ",")[[1]]
    if (length(parts) < 7 || parts[3] != "A") return(NULL)
    
    raw_lat <- parts[4]
    raw_lon <- parts[6]
    if (nchar(raw_lat) < 4 || nchar(raw_lon) < 5) return(NULL)
    
    lat <- as.numeric(substr(raw_lat, 1, 2)) + as.numeric(substr(raw_lat, 3, nchar(raw_lat))) / 60
    if (parts[5] == "S") lat <- -lat
    
    lon <- as.numeric(substr(raw_lon, 1, 3)) + as.numeric(substr(raw_lon, 4, nchar(raw_lon))) / 60
    if (parts[7] == "W") lon <- -lon
    
    list(lat = lat, lon = lon)
  })
  
  coords <- Filter(Negate(is.null), coords)
  if (length(coords) < 2) return(NULL)
  return(coords)
}

# --- Distanzberechnung mit Jitter-Filter ---
calc_distance <- function(coords, min_move = 5) {
  if (is.null(coords) || length(coords) < 2) return(0)
  total <- 0
  prev <- coords[[1]]
  for (i in 2:length(coords)) {
    next_pt <- coords[[i]]
    dist <- distHaversine(c(prev$lon, prev$lat), c(next_pt$lon, next_pt$lat))
    if (!is.na(dist) && dist >= min_move) {
      total <- total + dist
      prev <- next_pt
    }
  }
  total
}

# --- Initialisierung ---
n <- nrow(gps_index)
distances <- numeric(n)
start_coords_list <- vector("list", n)
end_coords_list   <- vector("list", n)
gap_to_previous   <- rep(NA_real_, n)
osrm_duration_hm  <- character(n)
real_duration_hm  <- character(n)

last_end_coord <- NULL

# --- Hauptschleife ---
for (i in seq_len(n)) {
  id <- gps_index$id[i]
  key <- paste0("track_", id)
  lines <- gps_data[[key]]
  coords <- parse_coords(lines)
  
  if (is.null(coords)) {
    distances[i] <- 0
    start_coords_list[[i]] <- list(lat = NA, lon = NA)
    end_coords_list[[i]]   <- list(lat = NA, lon = NA)
    real_duration_hm[i]    <- NA
  } else {
    distances[i] <- calc_distance(coords)
    start_coords_list[[i]] <- coords[[1]]
    end_coords_list[[i]]   <- coords[[length(coords)]]
    
    real_sec <- as.numeric(difftime(gps_index$end_time[i], gps_index$start_time[i], units = "secs"))
    real_duration_hm[i] <- sprintf("%02d:%02d", real_sec %/% 3600, (real_sec %% 3600) %/% 60)
  }
  
  if (!is.null(last_end_coord) && !is.na(last_end_coord$lat)) {
    gap <- distHaversine(
      c(last_end_coord$lon, last_end_coord$lat),
      c(start_coords_list[[i]]$lon, start_coords_list[[i]]$lat)
    )
    gap_to_previous[i] <- round(gap, 1)
  }
  
  if (!"osrm_duration_s" %in% names(gps_index) || length(gps_index$osrm_duration_s) < i || is.na(gps_index$osrm_duration_s[i])) {
    osrm_duration_hm[i] <- NA
  } else {
    dur_s <- gps_index$osrm_duration_s[i]
    osrm_duration_hm[i] <- sprintf("%02d:%02d", dur_s %/% 3600, (dur_s %% 3600) %/% 60)
  }
  
  last_end_coord <- end_coords_list[[i]]
}

# --- gps_index aktualisieren ---
gps_index[, travel_distance_m := round(distances, 1)]
gps_index[, start_coord := start_coords_list]
gps_index[, end_coord := end_coords_list]
gps_index[, gap_to_previous_m := gap_to_previous]
gps_index[, osrm_duration_hm := osrm_duration_hm]
gps_index[, real_duration_hm := real_duration_hm]

assign("gps_index", gps_index, envir = .GlobalEnv)
message("✅ gps_index wurde erfolgreich um travel_distance_m, start/end_coord, gap, osrm_duration_hm und real_duration_hm erweitert.")
