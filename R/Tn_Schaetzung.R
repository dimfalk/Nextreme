#' Schätzung der Wiederkehrintervalle für definierte Niederschlagshöhen und Dauerstufen
#'
#' @description
#' Berechnung der Wiederkehrintervalle (in Jahren) für bestimmte Niederschlagshöhen (in mm)
#' und -dauern (z.B. 5, 10, 60 und 120 min), wenn die Parameter, die die Extremwerte
#' beschreiben, bereits bekannt sind.
#'
#' @param extrem.Parameter data.frame. Koutsoyiannis- (Theta, Eta) und GEV-Parameter (
#'     Mu, Sigma, Gamma) auf Basis der verwendeten Serie (einzeilig).
#' @param hN numeric. Niederschlagshöhe (in mm) für jede unter `Dauern` verwendete
#'     Dauerstufe, für die das Wiederkehrintervall geschätzt werden soll.
#' @param Dauern numeric. Dauerstufe, auf die sich die Niederschlagshöhe `hN` bezieht.
#'     Die Dauerstufe ist in Minuten anzugeben.
#' @param methGEV character. Typ der Generalisierten Extremwertverteilung (GEV),
#'     die an die jährliche Serie angepasst werden soll:
#'     `"GEV"` für Typ 2 oder Typ 3 (Formparameter \code{!= 0}; Fréchet & Weibull) und
#'     `"GUM"` für Typ 1 (Formparameter \code{== 0}; Gumbel)
#'
#' @return data.frame. Spaltenweise Angabe zur verwendeten Niederschlagshöhe (hN),
#'      der gewählten Dauerstufe (D) sowie der resultierenden Wiederkehrintervalle (Tn).
#' @export
#'
#' @examples
#' # Berechnung der Starkregenparameter für die Station Görlitz im Zeitraum 1991-2020,
#' # ohne Intervall-oder Sprungkorrektur über alle Dauerstufen mit der GEV-Verteilung
#' # und dem Formparameter von -0.1
#' Dauern <- c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080)
#' extremParameter <- Parameter_Schaetzung(Goerlitz_maxIntSerie, Dauern)
#' extremParameter
#'
#' # Am 18.07.2010 wurden an der Station Görlitz 58.6 mm innerhalb von 6 Stunden gemessen.
#' # Basierend auf den geschätzten Parametern beträgt die berechnete Wiederkehrperiode:
#' Tn_Schaetzung(extremParameter,
#'               hN = 58.6,
#'               Dauern = 360,
#'               methGEV = "GEV")
#'
#' # Auf Grundlage dieser Parameter werden die entsprechenden Wiederkehrintervalle für
#' # die Niederschlagsmenge von 40 mm in den Dauerstufen 60, 120 und 240 Minuten bestimmt:
#' Tn_Schaetzung(extremParameter,
#'               hN = c(40, 40, 40),
#'               Dauern = c(60, 120, 360),
#'               methGEV = "GEV")
#'
#' # Auf Grundlage dieser Parameter werden die entsprechenden Wiederkehrintervalle für
#' # die Niederschlagsmengen von 50, 90, 95 mm in den Dauerstufen 240, 720 und 1440 Minuten bestimmt:
#' Tn_Schaetzung(extremParameter,
#'               hN = c(50, 90, 95),
#'               Dauern = c(240, 720, 1440),
#'               methGEV = "GEV")
Tn_Schaetzung <- function(extrem.Parameter,
                          hN = c(5, 10, 12, 20, 30, 40, 60, 70, 100, 100, 120, 150),
                          Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                          methGEV = "GEV") {

  # Bedingung 1: Das Input extrem.Parameter sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.

  if (missing(extrem.Parameter)) {
    stop("Das extrem.Parameter Input ist nicht vorhanden! Bitte geben Sie die GEV- und Koutsoyiannis-Parametern als einzeiliger data.frame.")
  } else if (class(extrem.Parameter) != "data.frame") {
    stop("Das extrem.Parameter Input sollte als data.frame sein! Bitte geben Sie die GEV- und Koutsoyiannis-Parameter als einzeiliger data.frame.")
  } else if (dim(extrem.Parameter)[1] != 1) {
    stop("Das extrem.Parameter Input soll nur eine Zeile enthalten!")
  } else if (dim(extrem.Parameter)[2] < 5) {
    stop("Das extrem.Parameter Input soll mindestens 5 Spalten enthalten (bzw. fuer Mu, Sigma, Gamma, Theta und Eta)!")
  } else if (any(is.na(extrem.Parameter[1, ]) == TRUE)) {
    stop("Das extrem.Parameter Input enthaelt mindestens einen fehlenden Wert! Entfernen Sie die fehlenden Werte")
  } else if (any(c("Mu", "Sigma", "Gamma", "Theta", "Eta") %in% names(extrem.Parameter) == FALSE)) {
    stop("Das extrem.Parameter Input sollte alle der folgenden Spaltennamen enthalten: Mu, Sigma, Gamma, Theta, Eta!")
  } else if (extrem.Parameter$Mu <= 0) {
    stop("Der Lokationsparameter Mu ist kleiner oder gleich Null!")
  } else if (extrem.Parameter$Sigma <= 0) {
    stop("Der Skalenparameter Sigma ist kleiner oder gleich Null!")
  } else if (extrem.Parameter$Theta < 0) {
    stop("Der erste Koutsoyiannis Parameter Theta ist kleiner als Null!")
  } else if (extrem.Parameter$Eta < 0 & extrem.Parameter$Eta > 1) stop("Der zweite Koutsoyiannis Parameter Eta ist kleiner als Null oder grosser als 1!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if (class(Dauern) != "numeric") {
    stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  } else if (length(Dauern) == 0) {
    stop("Das Dauern Input ist leer")
  } else if (any(is.na(Dauern) == TRUE)) {
    stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Dauern <= 0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")

  # Bedingung 3: Das Input hN sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if (class(hN) != "numeric") {
    stop("Das hN Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  } else if (length(hN) == 0) {
    stop("Das hN Input ist leer!")
  } else if (any(is.na(hN) == TRUE)) {
    stop("Das hN Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(hN <= 0)) stop("Das hN Input enthaelt negative oder Null Werte!")

  # Bedingung 4: Laenge der Dauer sollte mit der Laenge der hN uebereinstimmen
  if (length(hN) != length(Dauern)) stop("Die Laenge des hN Inputs sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")


  # Bedingung 5: methGEV sollte nur ein Element vom Typ Charakter sein!
  if (class(methGEV) != "character") {
    stop("Das methGEV Input sollte vom Charaktertyp sein!")
  } else if (length(methGEV) != 1) stop("Das methGEV Input sollte nur 1 Element haben!")

  Dauern_inStunden <- Dauern / 60
  bD <- (Dauern_inStunden + extrem.Parameter$Theta) ^ extrem.Parameter$Eta

  alle.Inten <- (hN / Dauern * 60) * bD

  if (methGEV == "GEV") {

    pars <- lmomco::pargev(lmomco::lmom.ub(1:10))
    pars$para[1] <- extrem.Parameter$Mu
    pars$para[2] <- extrem.Parameter$Sigma
    pars$para[3] <- extrem.Parameter$Gamma

    probs <- lmomco::cdfgev(alle.Inten, pars)

  } else if (methGEV == "GUM") {

    pars <- lmomco::pargum(lmomco::lmom.ub(1:10))
    pars$para[1] <- extrem.Parameter$Mu
    pars$para[2] <- extrem.Parameter$Sigma

    probs <- lmomco::cdfgev(alle.Inten, pars)

  } else {
    stop(paste0("Die gegebene Chararakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))
  }

  returnPeriod1 <- (1 / (1 - probs))

  returnPeriod <- sapply(returnPeriod1, function(Tn) 1 / (log(Tn) - log(Tn - 1)))
  returnPeriod <- round(returnPeriod, 2)
  returnPeriod[which(returnPeriod > 10)] <- floor(returnPeriod[which(returnPeriod > 10)])

  Output <- data.frame(hN = hN, D = Dauern, Tn = returnPeriod)

  return(Output)
}
