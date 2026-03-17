#' Sprung-Elimination
#'
#' @param Serie numeric. Vektor der Serienwerte einer gegebenen Dauerstufe.
#' @param Sensor character. Vektor der Sensorangaben mit identischer Länge wie Serie.
#' @param ZielSensor character. Sensor, auf dessen Serien-Mittelwert die Serienwerte angehoben oder abgesenkt werden sollen.
#'
#' @details
#' Das Verfahren ermittelt die sensorspezifischen Mittelwerte der Serienwerte,
#' subtrahiert diese von den Serienwerten und addiert dann den Serien-Mittelwert des Ziel-Sensors.
#' Ist `Zielsensor` nicht in `Sensor` enthalten, wird `Serie` unverändert zurückgegeben.
#'
#' @return data.frame mit den Spalten SenorZ und SerieNeu, Anzahl der Reihen entspricht Laenge von Serie. SensorZ ist der ZielSensor, SerieNeu die auf den ZielSensor angehobenen Serienwerte.
#' @keywords internal
#'
#' @examples
#' # synthetische Daten, Sensorwechsel ab dem 51. Wert
#' n1 <- 50
#' n2 <- 50
#' m1 <- 10
#' m2 <- 20
#'
#' x <- data.frame("Sensor" = c(rep("analog", n1), rep("digital", n2)),
#'                 "Wert" = c(rnorm(n1, m1), rnorm(n2, m2)))
#'
#' Sprung_Elimination(Serie = x$Wert, Sensor = x$Sensor)$SerieNeu
Sprung_Elimination <- function(Serie,
                               Sensor,
                               ZielSensor = Sensor[length(Sensor)]) {

  Sensor <- factor(Sensor)
  LEV <- levels(Sensor)

  if (length(LEV) == 1 | !(ZielSensor %in% LEV)) {

    return(data.frame(SensorZ = Sensor, SerieNeu = Serie))
  }

  mo <- stats::lm(Serie ~ factor(Sensor))
  SensorZ <- rep(ZielSensor, length(Sensor))
  PR0 <- stats::predict(mo)
  PR1 <- stats::predict(mo, new = data.frame(Sensor = SensorZ))

  # Bedingung fuer die Aufnahme von Stationen, bei denen der Mittelwert des analogen PR0 groeßer ist als der Mittelwert des digitalen PR1
  if (mean(mo$model[mo$model[, 2] == "analog", ]$Serie, na.rm = TRUE) > mean(mo$model[mo$model[, 2] == "digital", ]$Serie, na.rm = TRUE)) {
    SerieNeu <- Serie
    # print("Sprungkorrektur: Analog > Digital, keine Korrektur!")
  } else {
    # sensorspez. Mittelwert abziehen, zielsensor-Mittelwert aufaddieren
    SerieNeu <- Serie - PR0 + PR1
    # print("Sprungkorrektur durchgefuehrt!")
  }

  data.frame(SensorZ, SerieNeu)
}
