#' Sprungkorrektur bei kleinen Dauerstufen (D <= 30 Minuten)
#'
#' @description
#' Wenn es einen Sprung in den kurzen Dauerstufen der jährlichen Serie gibt,
#' wird der Sprung eliminiert und eine korrigierte jährlichen Serie zurückgegeben.
#'
#' @param Serie data.frame. Jährliche maximale Serie mit den betrachteten Jahren als Zeilen,
#'     und den betrachteten Dauerstufen als Spalten.
#' @param wechselDatum POSIXct or Date. Zeitpunkt, zu dem der Sensor von einem analogen
#'     auf einen digitalen Sensor umgestellt wurde.
#'
#' @details
#' Die Instationarität der kurzen Dauerstufen der jährlichen Serie, bedingt durch den Wechsel
#' der Sensoren von analoger zu digitaler Technologie, wird korrigiert.
#'
#' Analogsensoren: Regenschreiber, Unbekannt oder unbekannt
#' Digitalsensoren: H3, Tropfengeber, Wippengeber, Pluvio oder PLUVIO
#'
#' 1. Zuerst wird ein Test auf Instationarität für die kurzen Dauerstufen
#' (D <= 30 Minuten) der jährliche Serie durchgeführt.
#'
#' 2. Wenn die Instationarität vom Typ "Sprung" ist, wird eine Sprungkorrektur angewendet.
#'
#' @return data.frame. Korrigierte jährliche Serien (im gleichen Format wie `Serie`).
#' @export
#'
#' @examples
#' Sprung_Korrektur(Serie = Goerlitz_maxSerie, wechselDatum = as.Date("1992-12-31"))
Sprung_Korrektur <- function(Serie,
                             wechselDatum) {

  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if (missing(Serie)) {
    stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und Spalte die Dauer entsprechen.")
  } else if (class(Serie) != "data.frame") {
    stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und Spalte die Dauer entsprechen.")
  } else if (dim(Serie)[1] < 5) {
    stop("Das Serie Input enthaelt weniger als 5 Jahren! Um Fehler zu vermeiden, sind mindestens 5 Jahren erforderlich.")
  } else if (dim(Serie)[2] == 1) {
    stop("Das Serie Input enthaelt nur ein Dauer! Um Fehler zu vermeiden, sind mindestens 2 Dauern erforderlich.")
  } else if (any(apply(Serie, 1, function(i) all(is.na(i) == TRUE) == TRUE))) {
    stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlende Zeile.")
  } else if (any(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE))) {
    stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")
  } else if (length(which(as.numeric(colnames(Serie)) <= 30)) < 1) {
    stop("Das Serie Input sollte mindestens eine Spalte mit einer Dauer kleiner oder gleich 30 Minuten enthalten! Die Funktion wird nur auf die Spalten angewendet, deren Namen kuerzer oder gleich 30 Minuten sind.")
  } else if (length(which(as.numeric(rownames(Serie)) > 1800)) < 1) stop("Das Serie Input die Jahre als Zeilennamen enthalten. Achten Sie darauf, dass die Jahreszahlen vierstellig sind (z.B. groesser als 1800).")

  # Bedingung 2: Das Input wechselDatum sollte nur ein Element vom Typ Date() und nicht Nullsein!
  if (class(wechselDatum) != "Date") {
    stop("Das Input wechselDatum sollte vom Date() Typ sein!")
  } else if (length(wechselDatum) != 1) stop("Das Input wechselDatum sollte nur 1 Element haben!")

  Wechseljahr <- lubridate::year(wechselDatum)
  alleJahren <- as.numeric(rownames(Serie))
  Dauern <- as.numeric(colnames(Serie))

  if (Wechseljahr > alleJahren[1] & Wechseljahr < alleJahren[length(alleJahren)]) {

    if (Wechseljahr %in% alleJahren == FALSE) {

      Wechseljahr <- alleJahren[which(alleJahren >= Wechseljahr)[1]]
    }

    sensor_vec <- rep("analog", length(Serie[, 1]))
    sensor_vec[which(rownames(Serie) == Wechseljahr):length(sensor_vec)] <- "digital"

    # Pruefung auf "Sprung" in Spalten mit Dauern <= 30 min und wenn mindestens ein "Sprung" entdeckt wird, korrigiere alle AMS für Dauern <= 30 min
    Sprung_detected <- any(apply(Serie[, which(Dauern <= 30)], 2, function(x) {
      SIG.TEST <- Trend_vs_Sprung(Zeit = 1:length(x[which(is.na(x) == FALSE)]), Serienwerte = x[which(is.na(x) == FALSE)], Sensor = sensor_vec[which(is.na(x) == FALSE)])
      return(SIG.TEST$AicRes == "Sprung")
    }))

    if (Sprung_detected == TRUE) {

      Serie[, which(Dauern <= 30)] <- apply(Serie[, which(Dauern <= 30)], 2, function(x) {
        x_out <- x
        x_out[which(is.na(x) == FALSE)] <- Sprung_Elimination(x[which(is.na(x) == FALSE)], sensor_vec[which(is.na(x) == FALSE)], ZielSensor = sensor_vec[length(sensor_vec)])$SerieNeu
        return(round(x_out, 3))
      })
    }
  }

  return(Serie)
}
