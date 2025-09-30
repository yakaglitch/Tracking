# gps_logger_data_loader_pre_shiny.R
# Wähle interaktiv Root-Ordner (z. B. K:/) ODER direkt Monatsordner (z. B. K:/2025-09)
# Lese rekursiv alle gültigen Dateien aus allen vorhandenen YYYY-MM-Ordnern

library(fs)
library(data.table)

convert_nmea_coord <- function(value, hemisphere) {
  if (!nzchar(value)) return(NA_real_)
  value_num <- suppressWarnings(as.numeric(value))
  if (is.na(value_num)) return(NA_real_)

  if (nchar(value) <= 4L) return(NA_real_)

  if (hemisphere %in% c("N", "S")) {
    deg <- as.integer(substr(value, 1, 2))
    mins <- as.numeric(substr(value, 3, nchar(value)))
  } else {
    deg <- as.integer(substr(value, 1, 3))
    mins <- as.numeric(substr(value, 4, nchar(value)))
  }

  if (is.na(deg) || is.na(mins)) return(NA_real_)

  coord <- deg + mins / 60
  if (hemisphere %in% c("S", "W")) coord <- -coord
  coord
}

parse_rmc_lines <- function(nmea_lines, tz) {
  gprmc <- grep("^\\$G[NP]RMC", nmea_lines, value = TRUE)
  if (length(gprmc) == 0) return(NULL)

  entries <- lapply(seq_along(gprmc), function(idx) {
    parts <- strsplit(gprmc[idx], ",", fixed = TRUE)[[1]]
    if (length(parts) < 10) return(NULL)
    if (parts[3] != "A") return(NULL)

    time_str <- parts[2]
    date_str <- parts[10]

    if (!nzchar(time_str) || !nzchar(date_str)) return(NULL)

    if (nchar(time_str) < 6) return(NULL)
    hour <- suppressWarnings(as.integer(substr(time_str, 1, 2)))
    minute <- suppressWarnings(as.integer(substr(time_str, 3, 4)))
    sec <- suppressWarnings(as.numeric(substr(time_str, 5, nchar(time_str))))

    day <- suppressWarnings(as.integer(substr(date_str, 1, 2)))
    month <- suppressWarnings(as.integer(substr(date_str, 3, 4)))
    year <- suppressWarnings(as.integer(substr(date_str, 5, 6)))

    if (any(is.na(c(hour, minute, sec, day, month, year)))) return(NULL)

    year <- year + ifelse(year < 80, 2000L, 1900L)
    base_time <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year, month, day, hour, minute, floor(sec)), tz = tz)
    if (is.na(base_time)) return(NULL)
    timestamp <- base_time + (sec - floor(sec))

    lat <- convert_nmea_coord(parts[4], parts[5])
    lon <- convert_nmea_coord(parts[6], parts[7])

    if (is.na(lat) || is.na(lon)) return(NULL)

    speed_knots <- suppressWarnings(as.numeric(parts[8]))
    speed_kmh <- if (!is.na(speed_knots)) speed_knots * 1.852 else NA_real_

    data.table(
      timestamp = timestamp,
      lat = lat,
      lon = lon,
      speed_knots = speed_knots,
      speed_kmh = speed_kmh,
      source_index = idx
    )
  })

  coords <- rbindlist(entries, fill = TRUE)
  if (nrow(coords) == 0) return(NULL)
  setorder(coords, timestamp, source_index)
  coords[, time_offset_s := as.numeric(timestamp - timestamp[1])]
  coords[, timestamp_ms := format(timestamp, "%Y-%m-%d %H:%M:%OS3", tz = tz)]
  setcolorder(coords, c("timestamp", "timestamp_ms", "lat", "lon", "speed_knots", "speed_kmh", "source_index", "time_offset_s"))
  coords
}

downsample_track <- function(track_dt, target_hz) {
  if (is.null(track_dt) || nrow(track_dt) == 0) return(track_dt)
  if (!is.finite(target_hz) || target_hz <= 0) return(track_dt)

  secs <- as.numeric(track_dt$timestamp)
  if (anyNA(secs)) return(track_dt)

  origin <- secs[1]
  bins <- floor((secs - origin) * target_hz + 1e-9)
  keep <- !duplicated(bins)
  reduced <- track_dt[keep]
  reduced[, time_offset_s := as.numeric(timestamp - timestamp[1])]
  tz <- attr(track_dt$timestamp, "tzone")
  if (is.null(tz)) tz <- ""
  reduced[, timestamp_ms := format(timestamp, "%Y-%m-%d %H:%M:%OS3", tz = tz)]
  reduced
}

prompt_target_frequency <- function(default_hz = 1) {
  freq_values <- c("1 Hz" = 1, "5 Hz" = 5, "10 Hz" = 10)
  default_label <- names(freq_values)[match(default_hz, freq_values)]

  if (!interactive()) {
    message("ℹ️ Skript läuft nicht interaktiv – es wird standardmäßig ", default_hz, " Hz verwendet.")
    return(default_hz)
  }

  if (rstudioapi::isAvailable() && rstudioapi::hasFun("selectList")) {
    selection <- rstudioapi::selectList(
      choices = names(freq_values),
      title = "Wähle die maximale Ziel-Abtastrate für die Weiterverarbeitung",
      selected = default_label,
      multiple = FALSE
    )

    if (length(selection) == 0) {
      message("ℹ️ Keine Auswahl getroffen – es wird standardmäßig ", default_hz, " Hz verwendet.")
      return(default_hz)
    }

    chosen <- freq_values[[selection]]
    message("ℹ️ Gewählte maximale Ziel-Abtastrate: ", chosen, " Hz.")
    return(chosen)
  }

  old_menu_opt <- getOption("menu.graphics")
  on.exit(options(menu.graphics = old_menu_opt), add = TRUE)
  options(menu.graphics = FALSE)

  repeat {
    selection <- utils::menu(
      choices = names(freq_values),
      title = paste0(
        "Wähle die maximale Ziel-Abtastrate für die Weiterverarbeitung\n",
        "(0 oder ESC für Abbruch – dann wird der Standardwert ", default_hz, " Hz genutzt)"
      )
    )

    if (selection <= 0) {
      message("ℹ️ Keine Auswahl getroffen – es wird standardmäßig ", default_hz, " Hz verwendet.")
      return(default_hz)
    }

    chosen_hz <- unname(freq_values[selection])
    if (is.finite(chosen_hz) && chosen_hz > 0) {
      message("ℹ️ Gewählte maximale Ziel-Abtastrate: ", chosen_hz, " Hz.")
      return(chosen_hz)
    }

    message("⚠️ Ungültige Auswahl. Bitte erneut versuchen.")
  }
}

# Sicherstellen, dass rstudioapi verfügbar ist
if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
  stop("Dieses Skript muss in RStudio mit aktivem rstudioapi laufen.")
}

# ---- 1. Verzeichnisauswahl ----
root <- rstudioapi::selectDirectory("Wähle Verzeichnis mit YYYY-MM Unterordnern oder direkt einen YYYY-MM-Ordner")
if (is.null(root)) stop("Abbruch: Kein Ordner gewählt.")

# ---- 1a. Maximale Ziel-Frequenz wählen ----
target_hz <- prompt_target_frequency(default_hz = 1)

# ---- 2. Regex für Ordner und Dateien ----
tz_local <- Sys.timezone()
re_dir <- "^[0-9]{4}-[0-9]{2}$"
re_file <- "^[0-3][0-9][0-2][0-9][0-5][0-9][0-5][0-9]$"

# ---- 3. Erkenne Struktur: Root oder Einzelmonat ----
if (grepl(re_dir, path_file(root))) {
  ym_dirs <- root
} else {
  all_subdirs <- dir_ls(root, recurse = FALSE, type = "directory")
  ym_dirs <- all_subdirs[grepl(re_dir, path_file(all_subdirs))]
}
if (length(ym_dirs) == 0L) stop("❌ Keine YYYY-MM Ordner gefunden im gewählten Verzeichnis.")

# ---- 4. Einlesen der Dateien ----
all_rows <- list()
data_list <- list()
id <- 1L

for (ym in ym_dirs) {
  ym_name <- path_file(ym)
  yr <- as.integer(substr(ym_name, 1, 4))
  mo <- as.integer(substr(ym_name, 6, 7))
  
  files <- dir_ls(ym, type = "file")
  valid_files <- files[grepl(re_file, path_ext_remove(path_file(files)))]
  if (length(valid_files) == 0L) next
  
  info <- file_info(valid_files)
  base <- path_ext_remove(path_file(valid_files))
  
  for (i in seq_along(valid_files)) {
    day <- as.integer(substr(base[i], 1, 2))
    hr  <- as.integer(substr(base[i], 3, 4))
    mn  <- as.integer(substr(base[i], 5, 6))
    sc  <- as.integer(substr(base[i], 7, 8))
    
    ts_start <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", yr, mo, day, hr, mn, sc), tz = tz_local)
    content <- readLines(valid_files[i], warn = FALSE, encoding = "UTF-8")
    
    # --- Neue Endzeit extrahieren ---
    last_rmc <- tail(grep("^\\$G[NP]RMC", content, value = TRUE), 1)
    ts_end <- as.POSIXct(NA, tz = tz_local)
    
    if (length(last_rmc) > 0) {
      parts <- strsplit(last_rmc, ",")[[1]]
      if (length(parts) >= 2 && grepl("^[0-9]{6}", parts[2])) {
        end_hr <- as.integer(substr(parts[2], 1, 2))
        end_mn <- as.integer(substr(parts[2], 3, 4))
        end_sc <- as.integer(substr(parts[2], 5, 6))
        
        ts_end <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                                     yr, mo, day, end_hr, end_mn, end_sc), tz = tz_local)
      }
    }
    
    data_list[[paste0("track_", id)]] <- content
    
    all_rows[[length(all_rows) + 1]] <- data.table(
      id = id,
      file_name = path_file(valid_files[i]),
      full_path = valid_files[i],
      size = as.numeric(info$size[i]),
      mtime = as.POSIXct(info$modification_time[i], tz = tz_local),
      start_time = ts_start,
      end_time = ts_end
    )
    id <- id + 1L
  }
}

if (length(all_rows) == 0L) stop("❌ Keine gültigen Track-Dateien gefunden.")

# ---- 5. Environment speichern ----
gps_index <- rbindlist(all_rows)
gps_data <- data_list
assign("gps_index", gps_index, envir = .GlobalEnv)
assign("gps_data", gps_data, envir = .GlobalEnv)

message("✅ Fertig. ", length(gps_data), " Dateien geladen aus ", length(ym_dirs), " Monatsordner(n).")

# ---- 6. Daten reduzieren und zusammenführen ----
empty_template <- data.table(
  track_id = integer(),
  timestamp = as.POSIXct(character(), tz = tz_local),
  timestamp_ms = character(),
  lat = numeric(),
  lon = numeric(),
  speed_knots = numeric(),
  speed_kmh = numeric(),
  source_index = integer(),
  time_offset_s = numeric()
)

column_order <- names(empty_template)

parsed_list <- vector("list", length(gps_data))
resampled_list <- vector("list", length(gps_data))

for (idx in seq_along(gps_data)) {
  key <- names(gps_data)[idx]
  lines <- gps_data[[idx]]
  parsed <- parse_rmc_lines(lines, tz_local)

  if (!is.null(parsed) && nrow(parsed) > 0) {
    parsed_with_id <- copy(parsed)
    parsed_with_id[, track_id := as.integer(gsub("^track_", "", key))]
    setcolorder(parsed_with_id, column_order)
    parsed_list[[idx]] <- parsed_with_id

    reduced <- downsample_track(parsed_with_id, target_hz)
    if (!is.null(reduced) && nrow(reduced) > 0) {
      setcolorder(reduced, column_order)
      resampled_list[[idx]] <- reduced
    } else {
      resampled_list[[idx]] <- empty_template
    }
  } else {
    parsed_list[[idx]] <- empty_template
    resampled_list[[idx]] <- empty_template
  }
}

gps_points_raw <- rbindlist(parsed_list, fill = TRUE)
gps_points_resampled <- rbindlist(resampled_list, fill = TRUE)

assign("gps_points_raw", gps_points_raw, envir = .GlobalEnv)
assign("gps_points_resampled", gps_points_resampled, envir = .GlobalEnv)
assign("gps_resample_hz", target_hz, envir = .GlobalEnv)

message(
  "✅ Reduzierter Datensatz mit ", target_hz, " Hz erstellt (",
  nrow(gps_points_resampled), " Punkte).",
  " Vollständige Punkte befinden sich in 'gps_points_raw'.",
  " Zusätzliche Zeitstempel mit Millisekunden sind in 'timestamp_ms' verfügbar."
)
