# Tracking
# GPS-Logger Analyse- und Visualisierungsplattform

Dieses Repository enthält eine Sammlung von R-Skripten und einer interaktiven **Shiny-App** zur Verarbeitung, Analyse und Visualisierung von GPS-Daten.  
Die Lösung ist modular aufgebaut und deckt den gesamten Workflow ab – vom **Einlesen der Rohdaten im NMEA-Format**, über die **Berechnung von Distanzen und Fahrtdauern**, bis hin zur **OSRM-Routenabfrage** und einer interaktiven **Darstellung auf einer Leaflet-Karte** mit farblicher Geschwindigkeitskodierung.

Die Dokumentation ist bewusst **sehr ausführlich** gehalten, damit sie sowohl als Einstiegshilfe für neue Anwender als auch als Referenz für Entwickler dient, die den Code erweitern oder in andere Projekte integrieren möchten.

---

## Inhaltsverzeichnis

1. [Projektüberblick](#projektüberblick)  
2. [Hintergrund und Motivation](#hintergrund-und-motivation)  
3. [Technischer Gesamtüberblick](#technischer-gesamtüberblick)  
4. [Installation und Setup](#installation-und-setup)  
5. [Datenformate und Verzeichnisstruktur](#datenformate-und-verzeichnisstruktur)  
6. [Detaillierte Beschreibung der Skripte](#detaillierte-beschreibung-der-skripte)  
   - gps_logger_data_loader_pre_shiny.R  
   - gps_add_travel_distance_and_durations.R  
   - OSRM-Erweiterungsskript  
   - gps_logger_shiny_app.R  
7. [Interne Datenstrukturen](#interne-datenstrukturen)  
8. [Shiny-App im Detail](#shiny-app-im-detail)  
9. [Visualisierung und Farbcodierung](#visualisierung-und-farbcodierung)  
10. [Troubleshooting und bekannte Probleme](#troubleshooting-und-bekannte-probleme)  
11. [Erweiterungsmöglichkeiten](#erweiterungsmöglichkeiten)  
12. [Roadmap](#roadmap)  
13. [Lizenz](#lizenz)  
14. [Danksagung](#danksagung)  

---

## Projektüberblick

Dieses Projekt entstand aus dem praktischen Bedarf, **GPS-Datenlogger-Dateien** effizient zu verarbeiten, zu analysieren und übersichtlich darzustellen.  
Klassische GPS-Logger speichern ihre Aufzeichnungen meist im **NMEA-0183-Format**, welches einerseits standardisiert ist, andererseits aber viele unnötige oder redundante Informationen enthält.  

Die hier entwickelten Skripte übernehmen die folgenden Aufgaben:

- **Einlesen und Indizieren** von Logger-Dateien aus einer strukturierten Ordnerhierarchie.  
- **Berechnung von Fahrtdistanzen** auf Basis der Koordinaten und Filterung kleiner Jitter-Bewegungen.  
- **Vergleich realer Fahrtdauer** mit theoretischen OSRM-Routendauern.  
- **Interaktive Exploration** aller Fahrten über eine moderne Shiny-Weboberfläche.  
- **Visualisierung der Geschwindigkeit** über eine farbcodierte Polyline auf einer Leaflet-Karte.  
- **Integration externer Points of Interest (POI)**, um Start- und Zielorte automatisch zu identifizieren.  

Das Projekt ist **modular aufgebaut**:  
- Wer nur die Rohdaten extrahieren will, nutzt ausschließlich den **Datenlader**.  
- Wer Entfernungen und Zeiten braucht, erweitert mit dem **Distanz-Skript**.  
- Wer Routinginformationen benötigt, aktiviert zusätzlich die **OSRM-Erweiterung**.  
- Wer alles visuell auswerten möchte, startet die **Shiny-App**.  

---

## Hintergrund und Motivation

GPS-Datenlogger sind im Alltag nützlich, aber ihre Rohdaten sind schwer interpretierbar:  
- Das NMEA-Format ist zeilenbasiert und erfordert **Parsing**.  
- Jede Fahrt verteilt sich auf mehrere Dateien, deren Namen nicht sofort eine Zeitspanne erkennen lassen.  
- Ohne zusätzliche Berechnung ist weder die **Fahrtdistanz** noch die **Reisezeit** direkt ablesbar.  

Außerdem interessiert häufig der Vergleich zwischen:  
- der tatsächlich gefahrenen Strecke (GPS-Haversine-Distanz)  
- und der optimalen Route nach Straßennetz (OSRM-Routing).  

Ziel dieses Projektes ist daher:  
1. **Automatisierung** des Lade- und Analyseprozesses.  
2. **Zentrale Datenstruktur** für alle Fahrten (`gps_index`).  
3. **Interaktive Auswertung** über eine Shiny-App mit Karte und Tabelle.  
4. **Vergleichbarkeit** zwischen GPS-Realität und OSRM-Theorie.  

---

## Technischer Gesamtüberblick

Das Projekt besteht aus vier Hauptkomponenten:

1. **Datenlader (`gps_logger_data_loader_pre_shiny.R`)**  
   - Wählt Root- oder Monatsordner aus.  
   - Lädt rekursiv alle gültigen Dateien.  
   - Erstellt die zentralen Strukturen `gps_index` und `gps_data`.  

2. **Distanz- und Dauerberechnung (`gps_add_travel_distance_and_durations.R`)**  
   - Extrahiert Koordinaten aus NMEA-GPRMC-Zeilen.  
   - Berechnet die Haversine-Distanz (mit Jitter-Filter).  
   - Bestimmt reale Fahrtdauer und Start-/End-Koordinaten.  

3. **OSRM-Erweiterungsskript**  
   - Fragt einen lokalen oder entfernten OSRM-Server ab.  
   - Ergänzt `gps_index` um OSRM-Distanzen, -Zeiten und vollständige Routenkoordinaten.  

4. **Shiny-App (`gps_logger_shiny_app.R`)**  
   - Interaktive Oberfläche mit Tabelle und Leaflet-Karte.  
   - Auswahl einzelner Tage und Fahrten.  
   - Checkboxen für GPS-Track und OSRM-Route.  
   - Farbliche Kodierung der Geschwindigkeit.  
   - Automatisches Zooming auf alle geladenen Strecken.  

---

## Installation und Setup

### Voraussetzungen

- **R** (>= 4.2 empfohlen)  
- **RStudio** (wegen `rstudioapi::selectDirectory`)  
- Lokale Installation eines **OSRM-Servers** (optional, nur für Routing-Features)  
- Internetverbindung für die Installation der R-Pakete  

### Notwendige R-Pakete

```r
install.packages(c(
  "fs",
  "data.table",
  "DT",
  "stringr",
  "RColorBrewer",
  "geosphere",
  "shiny",
  "leaflet",
  "httr",
  "jsonlite"
))
