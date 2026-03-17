#' Trend vs. Sprung
#'
#' @param Zeit numeric. Zeitvektor der Serienwerte einer gegebenen Dauerstufe.
#' @param Serienwerte numeric. Vektor der Serienwerte einer gegebenen Dauerstufe.
#' @param Sensor character. Vektor der Sensorangaben mit identischer Länge wie Serie.
#'     Wenn `Sensor = NULL`, wird nur getestet, ob der Trend stationär ist.
#' @param ifTS logical. TRUE, wenn auf gleichzeitiges Auftreten von Trend und Sprung getestet werden soll.
#' @param ifAnova logical. TRUE, wenn die Trendstrukturprüfung zusätzlich auch
#'     durch Devianzanalyse vorgenommen werden soll.
#' @param skaliereZeit logical. TRUE, wenn der Zeitvektor in der Form `(Zeit - min(Zeit)) / length(Zeit) - 0.5`
#'     in den Wertebereich zwischen -0.5 und 0.5 transformiert werden soll.
#'     Der berechnete Trend-Parameter bezieht sich dann auf die transformierte Zeit.
#'     Für den Optimierungsalgorithmus ist es in der Regel einfacher, das globale Optimum zu finden,
#'     wenn die Eingangsdaten des Zeitvektors einen kleinen Werteraum umspannen.
#'
#' @details
#' Die Generalisierte Extremwertverteilung GEV wird mittels der Maximum-Likelihood-Methode
#' an die Serienwerte angepasst, wobei bezogen auf den Lokationsparameter die vier Modellformen
#' "Stat" (d.h. stationär), "Trend" oder "Sprung" angepasst werden. Für alle vier Modelle werden
#' die Informationskriterien AIC und BIC ermittelt und anhand des minimalen IC-Wertes wird ausgewertet,
#' welches der vier Modelle die Daten am besten beschreibt. Optional kann zusätzlich auch ein
#' partieller Devianztest mittels `ifAnova = TRUE` durchgeführt werden, wobei dieser das
#' Nullmodell möglicherweise zu häufig zugunsten des komplexeren verwirft.
#'
#' @return AicRes character. "Stat|Trend|Sprung", Ergebnis auf Grundlage des AIC-Kriteriums.
#' @return BicRes character. "Stat|Trend|Sprung", Ergebnis auf Grundlage des BIC-Kriteriums.
#' @return Aic.<Modell> numeric. AIC-Wert für die einzelnen Modelle Stat|Trend|Sprung.
#' @return Bic.<Modell> numeric. BIC-Wert für die einzelnen Modelle Stat|Trend|Sprung.
#' @return AnovaRes character. "Stat|Trend|Sprung", Ergebnis auf Grundlage des Devianztests.
#' @return Anova.<Modell> numeric. p-Values für Übergang von Nullmodell zu einem der drei anderen Modelle Stat|Trend|Sprung.
#' @keywords internal
#'
#' @examples
#' n <- 100
#' set.seed(1234)
#'
#' # synthetische GEV-verteilte Daten, stationär
#' xStat <- evd::rgev(n, 10, 2, 0.1)
#'
#' # synthetische GEV-verteilte Daten, trendbehaftet
#' xTrend <- evd::rgev(n, 10, 2, 0.1) + 1:n * 0.05
#'
#' # synthetische GEV-verteilte Daten, sprungbehaftet
#' xSprung <- c(evd::rgev(n / 2, 10, 1, 0.1), evd::rgev(n / 2, 20, 2, 0.1))
#'
#' Sensor <- factor(c(rep("analog", n / 2), rep("digital", n / 2)))
#' x <- data.frame(Jahr = 1:n, Sensor = Sensor, xStat, xTrend, xSprung)
#'
#' Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xStat"], Sensor = x[, "Sensor"])
#' Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xTrend"], Sensor = x[, "Sensor"])
#' Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xSprung"], Sensor = x[, "Sensor"])
Trend_vs_Sprung <- function(Zeit,
                            Serienwerte,
                            Sensor = NULL,
                            ifTS = FALSE,
                            ifAnova = FALSE,
                            skaliereZeit = TRUE) {

  Fgev <- function(...) {
    res <- list(mo = "", Aic = as.numeric(NA), Bic = as.numeric(NA))
    mo <- try(evd::fgev(..., std.err = FALSE), silent = TRUE)
    if (!is.character(mo)) {
      mo$Aic <- stats::AIC(mo)
      mo$Bic <- stats::AIC(mo, k = length(mo$est))
    }
    mo
  }
  Anova <- function(...) {
    res <- try(stats::anova(...)[2, 5])
    if (is.character(res)) res <- as.numeric(NA)
    res
  }

  # if(!is.null(PrintLine))print(PrintLine)

  # data
  DTR <- data.frame(Tr = Zeit)
  if (skaliereZeit) DTR <- data.frame(Tr = (Zeit - min(Zeit)) / length(Zeit) - 0.5)
  DSP <- DTRSP <- ""
  if (!is.null(Sensor)) {
    DSP <- try(data.frame(stats::model.matrix(~ factor(Sensor), data.frame(Sensor)))[, -1], silent = TRUE) # ohne intercept
    DTRSP <- try(data.frame(DTR, DSP), silent = TRUE)
  }

  # model
  mo <- mTr <- mSp <- mTrSp <- list(mo = "", Aic = as.numeric(NA), Bic = as.numeric(NA))
  m0 <- Fgev(Serienwerte)
  mTr <- Fgev(Serienwerte, nsloc = DTR)
  if (!is.character(DSP)) mSp <- Fgev(Serienwerte, nsloc = DSP)
  if (ifTS & !is.character(DTRSP)) mTrSp <- Fgev(Serienwerte, nsloc = DTRSP)

  # IC
  if (ifTS) {
    AicVec <- c(Stat = m0$Aic, Trend = mTr$Aic, Sprung = mSp$Aic, TrendSprung = mTrSp$Aic)
    BicVec <- c(Stat = m0$Bic, Trend = mTr$Bic, Sprung = mSp$Bic, TrendSprung = mTrSp$Bic)
  } else {
    AicVec <- c(Stat = m0$Aic, Trend = mTr$Aic, Sprung = mSp$Aic)
    BicVec <- c(Stat = m0$Bic, Trend = mTr$Bic, Sprung = mSp$Bic)
  }
  AicRes <- names(which.min(AicVec))
  BicRes <- names(which.min(BicVec))

  res <- data.frame(AicRes, BicRes, Aic = t(AicVec), Bic = t(BicVec))

  # anova
  if (ifAnova) {
    AnovaVec <- c(Trend = Anova(mTr, m0), Sprung = Anova(mSp, m0))
    if (ifTS) AnovaVec <- c(AnovaVec, TrendSprung = Anova(mTrSp, m0))
    AnovaRes <- names(which.min(AnovaVec))
    res <- cbind(res, data.frame(AnovaRes, Anova = t(AnovaVec)))
  }

  # out
  res
}
