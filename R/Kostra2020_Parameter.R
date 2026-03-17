#' Extraktion der Kostra-DWD-2020 Parameter für bestimmte Standorte
#'
#' @description
#' Die geschätzten Parameter aus KOSTRA-DWD-2020 werden aus dem
#' Climate Data Center des Deutschen Wetterdienstes für bestimmte Standorte bezogen.
#'
#' @param Standorte data.frame. Definition der Standorte, für die KOSTRA-Daten
#'     extrahiert werden sollen. Der Dataframe muss die folgenden drei Spalten
#'     aufweisen: Stationskennung in Spalte `"stations_id`, geographische Länge
#'     in Spalte `geoLaenge` und geographische Breite in Spakte `geoBreite`.
#'     Die Koordinaten müssen in WGS84 vorliegen.
#' @param Temp_Pfad character. Ordnerpfad, in den die KOSTRA-Daten heruntergeladen
#'     werden.
#'
#' @return data.frame. Stationskennung, Koordinaten und geschätzte Parameter
#'     gem. KOSTRA-DWD-2020 für definierte Standorte (pro Zeile).
#'     Spaltenweise Angabe der nachfolgenden Parameter:
#'     Theta (1. Koutsoyiannis-Parameter),
#'     Eta (2. Koutsoyiannis-Parameter),
#'     Mu (GEV-Lokationsparameter),
#'     Sigma (GEV-Skalenparameter),
#'     Gamma (GEV-Formparameter).
#' @export
#'
#' @examples
#' Station <- data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
#' Kostra2020_Parameter(Station)
Kostra2020_Parameter <- function(Standorte,
                                 Temp_Pfad = "./") {

  if (missing(Standorte)) {
    stop("Das Standorte Input ist nicht vorhanden! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  } else if (class(Standorte) != "data.frame") {
    stop("Das Standorte Input sollte als data.frame sein! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  } else if (dim(Standorte)[1] < 1) {
    stop("Das Standorte Input soll mindestens eine Zeile enthalten!")
  } else if (dim(Standorte)[2] < 3) {
    stop("Das Standorte Input soll mindestens 3 Spalten enthalten (bzw. fuer Stations_id, geoLaenge,geoBreite)!")
  } else if (any(is.na(Standorte) == TRUE)) {
    stop("Das Standorte Input enthaelt mindestens einen fehlenden Wert! Entfernen Sie die fehlenden Werte")
  } else if (any(c("Stations_id", "geoLaenge", "geoBreite") %in% names(Standorte) == FALSE)) stop("Das Standorte Input sollte alle der folgenden Spaltennamen enthalten: Stations_id, geoLaenge, geoBreite!")

  if (dir.exists(Temp_Pfad) == FALSE) warning("Das Temp_Pfad Input existiert nicht! Es wird ein weiterer temporaerer Pfad erstellt.")

  KOSTRA_Link <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2020/asc/"
  KOSTRA_htmllines <- readLines(KOSTRA_Link, warn = FALSE)

  # find lines with "_ASC.zip"
  zip_lines <- grep("_ASC\\.zip", KOSTRA_htmllines, value = TRUE)

  # extract the filenames
  KOSTRA_AllFiles <- gsub('.*href="([^"]+_ASC\\.zip)".*', "\\1", zip_lines)

  KOSTRA_ParFile <- grep("Parameter", KOSTRA_AllFiles, value = TRUE)

  # Koordinatenreferenzsystem für die Ausgabedaten
  Kostra_CRS <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

  # Reprojektion der Koordinaten der Stationen in das Ausgabekoordinatensystem.
  station_shp <- terra::vect(Standorte, geom = c("geoLaenge", "geoBreite"), crs = "+proj=longlat +datum=WGS84")
  station_shp <- terra::project(station_shp, Kostra_CRS)

  if (dir.exists(Temp_Pfad) == TRUE) {
    temp <- tempfile(tmpdir = Temp_Pfad, fileext = ".zip")
  } else {
    temp <- tempfile(fileext = ".zip")
  }

  utils::download.file(paste0(KOSTRA_Link, KOSTRA_ParFile), temp)
  utils::unzip(temp, exdir = Temp_Pfad)

  unlink(temp)

  fnames <- list.files(path = Temp_Pfad, pattern = "^Parameter_KOSTRA-DWD-2020_.*asc$")

  r <- terra::rast(fnames)
  terra::crs(r) <- Kostra_CRS

  Kostra_Parameter <- data.frame(
    "ID" = Standorte$Stations_id,
    "geoBreite" = Standorte$geoBreite,
    "geoLaenge" = Standorte$geoLaenge,
    "Theta" = terra::extract(r[[1]], station_shp)[, -1],
    "Eta" = terra::extract(r[[2]], station_shp)[, -1],
    "Mu" = terra::extract(r[[3]], station_shp)[, -1],
    "Sigma" = terra::extract(r[[4]], station_shp)[, -1],
    "Gamma" = terra::extract(r[[5]], station_shp)[, -1]
  )

  unlink(fnames)

  Kostra_Parameter
}
