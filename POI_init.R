# poi_initializer.R
# Liest die POI_R.xlsx Datei ein und bereitet sie für die weitere Verwendung vor

library(readxl)
library(data.table)

# --- Pfad zur Excel-Datei anpassen ---
file_path <- "POI_R.xlsx"

# --- Excel einlesen (nur erstes Blatt, alle Spalten als Text) ---
poi_raw <- read_excel(file_path, col_types = "text")

# --- In data.table konvertieren ---
poi <- as.data.table(poi_raw)

# --- Spalten bereinigen und typisieren ---
setnames(poi, tolower(names(poi)))  # Spaltennamen klein
setnames(poi, c("id", "name", "lat", "lon", "kategorie", "typ", "range"))  # zur Sicherheit

# Komma in Punkt konvertieren und numerisch casten
poi[, lat := as.numeric(gsub(",", ".", lat, fixed = TRUE))]
poi[, lon := as.numeric(gsub(",", ".", lon, fixed = TRUE))]
poi[, id  := as.integer(id)]

# --- Ergebnis prüfen ---
print(poi)

# --- Global speichern (optional) ---
assign("poi_table", poi, envir = .GlobalEnv)
message("✅ POI-Tabelle erfolgreich geladen: ", nrow(poi), " Einträge")




# track_poi_by_range.R
# POI-Zuordnung: nur wenn innerhalb definierter Range, inkl. Match-Qualität (je näher am Zentrum, desto höher)

library(data.table)
library(geosphere)

# Sicherheitsprüfung
if (!exists("gps_index", envir = .GlobalEnv) || !exists("poi_table", envir = .GlobalEnv)) {
  stop("❌ gps_index oder poi_table nicht gefunden.")
}

gps_index <- get("gps_index", envir = .GlobalEnv)
poi_table <- get("poi_table", envir = .GlobalEnv)

# Prüfen, ob 'range' vorhanden und numerisch ist
if (!"range" %in% names(poi_table)) stop("❌ Spalte 'range' fehlt in poi_table")
poi_table[, range := as.numeric(range)]

# Initialisierung
n <- nrow(gps_index)
start_poi_ids   <- rep(NA_integer_, n)
end_poi_ids     <- rep(NA_integer_, n)
start_poi_score <- rep(NA_real_, n)
end_poi_score   <- rep(NA_real_, n)

# Funktion zur POI-Suche innerhalb Range + Matchscore (0–100%)
find_matching_poi <- function(lat, lon, poi_dt) {
  dists <- distHaversine(matrix(c(poi_dt$lon, poi_dt$lat), ncol = 2), c(lon, lat))
  poi_dt[, dist_m := dists]
  in_range <- poi_dt[dist_m <= range]
  if (nrow(in_range) == 0L) return(list(id = NA_integer_, score = NA_real_))
  
  best <- in_range[which.min(dist_m)]
  score <- (1 - best$dist_m / best$range) * 100
  return(list(id = best$id, score = round(score, 1)))
}

# Schleife über alle Tracks
for (i in seq_len(n)) {
  start <- gps_index$start_coord[[i]]
  end   <- gps_index$end_coord[[i]]
  
  if (!is.null(start) && !any(is.na(start))) {
    res <- find_matching_poi(start$lat, start$lon, poi_table)
    start_poi_ids[i]   <- res$id
    start_poi_score[i] <- res$score
  }
  
  if (!is.null(end) && !any(is.na(end))) {
    res <- find_matching_poi(end$lat, end$lon, poi_table)
    end_poi_ids[i]   <- res$id
    end_poi_score[i] <- res$score
  }
}

# Ergebnis speichern
gps_index[, `:=`(
  poi_start_id = start_poi_ids,
  poi_end_id   = end_poi_ids,
  poi_start_score = start_poi_score,
  poi_end_score   = end_poi_score
)]

assign("gps_index", gps_index, envir = .GlobalEnv)
message("✅ POI-Zuordnung abgeschlossen (nur innerhalb Range, inkl. Score).")
