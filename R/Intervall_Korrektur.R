#' Berücksichtigung der Intervalllänge
#'
#' @description
#' Korrektur der Regenhöhen für Dauerstufen bis zur vierfachen Intervalllänge.
#' Für weitere Informationen siehe Kap. 5.3 und Tab. 1 des Arbeitsblattes DWA-A 531 (2025).
#'
#' @param Serie data.frame. Jährliche maximale Serie mit den betrachteten Jahren als Zeilen,
#'     und den betrachteten Dauerstufen als Spalten.
#' @param Intervall numeric. Zeitintervall der Niederschlagsmessungen (entweder in Minuten oder Stunden).
#'     Ist in den gleichen Einheiten anzugeben, wie die Dauerstufen der verwendeten Serie.
#'
#' @return data.frame. Nach Intervalllänge korrigierte jährliche maximale Serie.
#' @export
#'
#' @examples
#' Intervall_Korrektur(Serie = Goerlitz_maxIntSerie)
Intervall_Korrektur <- function(Serie,
                                Intervall = 5) {

  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if (missing(Serie)) {
    stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Maximum Serie an (in mm/h oder mm), wobei die Zeilen die Jahren und die Spalten die Regendauer entsprechen.")
  } else if (class(Serie) != "data.frame") {
    stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Maximum Serie an (in mm/h oder mm), wobei die Zeilen die Jahren und die Spalten die Regendauer entsprechen.")
  } else if (any(apply(Serie, 1, function(i) all(is.na(i) == TRUE) == TRUE))) {
    stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlenden Zeilen.")
  } else if (any(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE))) stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")

  # Bedingung 2: Das Input Intervall sollte nur ein Element vom Typ numerisch und nicht Null oder negativ sein!
  if (class(Intervall) != "numeric") {
    stop("Das Input Intervall sollte vom numerischen Typ sein!")
  } else if (length(Intervall) != 1) {
    stop("Das Input Intervall sollte nur 1 Element haben!")
  } else if (Intervall <= 0) stop("Das Input Intervall kann nicht 0 oder negativ sein!")

  Anzahl_der_Intervalle <- as.numeric(colnames(Serie)) / Intervall

  if (any(Anzahl_der_Intervalle == 1) == TRUE) Serie[, which(Anzahl_der_Intervalle == 1)] <- Serie[, which(Anzahl_der_Intervalle == 1)] * 1.14
  if (any(Anzahl_der_Intervalle == 2) == TRUE) Serie[, which(Anzahl_der_Intervalle == 2)] <- Serie[, which(Anzahl_der_Intervalle == 2)] * 1.07
  if (any(Anzahl_der_Intervalle == 3) == TRUE) Serie[, which(Anzahl_der_Intervalle == 3)] <- Serie[, which(Anzahl_der_Intervalle == 3)] * 1.04
  if (any(Anzahl_der_Intervalle == 4) == TRUE) Serie[, which(Anzahl_der_Intervalle == 4)] <- Serie[, which(Anzahl_der_Intervalle == 4)] * 1.03

  return(Serie)
}
