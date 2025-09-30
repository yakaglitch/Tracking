# ============================================
# Ergänze gps_index mit OSRM Routing-Daten
# ============================================
library(data.table)
library(httr)
library(jsonlite)

# Sicherstellen, dass gps_index geladen ist
if (!exists("gps_index", envir = .GlobalEnv)) stop("gps_index nicht geladen")

gps_index <- get("gps_index", envir = .GlobalEnv)

# Neue Spalten vorbereiten
gps_index[, `:=`(
  osrm_distance_m = NA_real_,
  osrm_duration_s = NA_real_,
  osrm_route_coords = vector("list", .N)
)]

# Hilfsfunktion für OSRM-Abfrage
query_osrm_route <- function(lat1, lon1, lat2, lon2) {
  url <- sprintf(
    "http://localhost:5000/route/v1/driving/%.6f,%.6f;%.6f,%.6f?overview=full&geometries=geojson",
    lon1, lat1, lon2, lat2
  )
  
  res <- tryCatch(GET(url), error = function(e) return(NULL))
  if (is.null(res) || status_code(res) != 200) return(NULL)
  
  json <- tryCatch(fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) return(NULL))
  if (!is.list(json) || !"routes" %in% names(json) || length(json$routes) == 0) return(NULL)
  
  r <- json$routes[[1]]
  coords_latlon <- lapply(r$geometry$coordinates, function(x) c(lat = x[2], lon = x[1]))
  
  list(
    distance = r$distance,
    duration = r$duration,
    coords   = coords_latlon
  )
}

# Schleife über Zeilen mit gültigen Start- und Endpunkten
for (i in seq_len(nrow(gps_index))) {
  start <- gps_index$start_coord[[i]]
  end   <- gps_index$end_coord[[i]]
  
  # Prüfe Gültigkeit
  if (is.null(start) || is.null(end) || any(is.na(start)) || any(is.na(end))) {
    next
  }
  
  # Koordinaten extrahieren
  lat1 <- as.numeric(start["lat"])
  lon1 <- as.numeric(start["lon"])
  lat2 <- as.numeric(end["lat"])
  lon2 <- as.numeric(end["lon"])
  
  if (any(is.na(c(lat1, lon1, lat2, lon2)))) next
  
  res <- query_osrm_route(lat1, lon1, lat2, lon2)
  
  if (!is.null(res)) {
    gps_index[i, osrm_distance_m := round(res$distance, 1)]
    gps_index[i, osrm_duration_s := round(res$duration, 1)]
    gps_index[i, osrm_route_coords := list(res$coords)]
    cat(sprintf("✅ Zeile %d: %.1f m, %.1f s\n", i, res$distance, res$duration))
  } else {
    cat(sprintf("⚠️  Zeile %d: Keine gültige OSRM-Route\n", i))
  }
}

# Ergebnis speichern ins Environment
assign("gps_index", gps_index, envir = .GlobalEnv)
message("✅ OSRM-Auswertung abgeschlossen.")
