# gps_logger_data_loader_pre_shiny.R
# Wähle interaktiv Root-Ordner (z. B. K:/) ODER direkt Monatsordner (z. B. K:/2025-09)
# Lese rekursiv alle gültigen Dateien aus allen vorhandenen YYYY-MM-Ordnern

library(fs)
library(data.table)

# Sicherstellen, dass rstudioapi verfügbar ist
if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
  stop("Dieses Skript muss in RStudio mit aktivem rstudioapi laufen.")
}

# ---- 1. Verzeichnisauswahl ----
root <- rstudioapi::selectDirectory("Wähle Verzeichnis mit YYYY-MM Unterordnern oder direkt einen YYYY-MM-Ordner")
if (is.null(root)) stop("Abbruch: Kein Ordner gewählt.")

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
