#' Schätzung der Starkregenhöhen für bestimmte Dauerstufen und Wiederkehrintervalle
#'
#' @description
#' Berechnung der Starkniederschlagsintensitäten (in mm/h) bzw. -höhen (in mm) für
#' bestimmte Dauerstufen (z.B. 5, 10, 60 und 120 min) und Wiederkehrintervalle
#' (z.B. 5, 10 und 100 Jahre), wenn die Parameter, die die Extremwerte
#' beschreiben, bereits bekannt sind.
#'
#' @param extrem.Parameter data.frame. Koutsoyiannis- (Theta, Eta) und GEV-Parameter (
#'     Mu, Sigma, Gamma) auf Basis der verwendeten Serie (einzeilig).
#' @param Dauern numeric. Dauerstufe, auf die sich die Niederschlagshöhe `hN` bezieht.
#'     Die Dauerstufe ist in Minuten anzugeben.
#' @param Tn numeric. Wiederkehrintervalle, für die die Niederschlagshöhe berechnet werden soll.
#'     Die Wiederkehrintervalle ist in Jahren anzugeben.
#' @param methGEV character. Typ der Generalisierten Extremwertverteilung (GEV),
#'     die an die jährliche Serie angepasst werden soll:
#'     `"GEV"` für Typ 2 oder Typ 3 (Formparameter \code{!= 0}; Fréchet & Weibull) und
#'     `"GUM"` für Typ 1 (Formparameter \code{== 0}; Gumbel)
#' @param SerieTyp character. Kontrolle über die für die Augabe verwendeten Einheiten.
#'     `"VOL"` für Niederschlagshöhe (mm) und `"INT"` für Niederschlagsintensität (mm/h).
#'
#' @return data.frame. Geschätzte Niederschlagsintensitäten (IDF) bzw. -höhen (DDF)
#'     auf Grundlage der verwendeten Extremwertparameter. Dauerstufen in Spalten und
#'     Wiederkehrintervalle in Zeilen.
#' @export
#'
#' @examples
#' # Berechnung der dauerstufenÜbergreifenden Verteilungsparameter fÜr die
#' # Station Görlitz im Zeitraum 1991-2020, ohne Intervall- oder Sprungkorrektur,
#' # Über alle Dauerstufen mit auf -0.1 fixiertem Formparameter
#' Dauern <- c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080)
#' Tn <- c(1, 5, 10, 20, 50, 100)
#'
#' extremParameter <- Parameter_Schaetzung(Goerlitz_maxIntSerie,
#'                                         Dauern,
#'                                         methGEV = "GEV",
#'                                         formTyp = "FIX",
#'                                         Gamma = -0.1)
#' extremParameter
#'
#' # Berechnung der Niederschlagsintensität-Dauer-Wiederkehrintervall (IDF) Tabelle
#' # für definierte Wiederkehrintervalle und Dauerstufen auf Basis der berechneten Parameter
#' IDF <- Quantil_Schaetzung(extremParameter, Dauern, Tn, methGEV = "GEV", SerieTyp = "INT")
#'
#' # Berechnung der Niederschlagshöhe-Dauer-Wiederkehrintervall (DDF) Tabelle
#' # für definierte Wiederkehrintervalle und Dauerstufen auf Basis der berechneten Parameter
#' DDF <- Quantil_Schaetzung(extremParameter, Dauern, Tn, methGEV = "GEV", SerieTyp = "VOL")
Quantil_Schaetzung <- function(extrem.Parameter,
                               Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                               Tn = c(1, 5, 10, 20, 50, 100),
                               methGEV = "GEV",
                               SerieTyp = "VOL") {

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
    stop("Das extrem.Parameter Input enthaelt mindestens einen fehlenden Werte! Entfernen Sie die fehlenden Werte.")
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
  } else if (length(Dauern) == 1) {
    stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  } else if (any(is.na(Dauern) == TRUE)) {
    stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Dauern <= 0)) stop("Das Dauern Input enthaelt Negative oder Null Werte!")

  # Bedingung 3: Das Input Tn sollte als Zahlenvektor angegeben werden und mehr als 0 Element haben.
  if (class(Tn) != "numeric") {
    stop("Das Tn Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben.")
  } else if (length(Tn) == 0) {
    stop("Das Tn Input ist leer!")
  } else if (any(is.na(Tn) == TRUE)) {
    stop("Das Tn Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Tn <= 0)) stop("Das Tn Input enthaelt Negative oder Null Werte!")

  # Bedingung 4: methGEV sollte nur ein Element vom Typ Charakter sein!
  if (class(methGEV) != "character") {
    stop("Das methGEV Input sollte vom Charaktertyp sein!")
  } else if (length(methGEV) != 1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 5: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if (class(SerieTyp) != "character") {
    stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  } else if (length(SerieTyp) != 1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  Dauern_inStunden <- Dauern / 60
  bD <- (Dauern_inStunden + extrem.Parameter$Theta)^extrem.Parameter$Eta
  Tn_Input <- Tn
  Tn <- sapply(Tn, function(Tn) exp(1 / Tn) / (exp(1 / Tn) - 1))
  Tn <- round(Tn, 2)
  Tn[which(Tn > 10)] <- round(Tn[which(Tn > 10)], 0)
  Quantile <- (1 - (1 / Tn))


  if (methGEV == "GEV") {

    pars <- lmomco::pargev(lmomco::lmom.ub(1:10))
    pars$para[1] <- extrem.Parameter$Mu
    pars$para[2] <- extrem.Parameter$Sigma
    pars$para[3] <- extrem.Parameter$Gamma
    quan <- lmomco::quagev(Quantile, pars)

  } else if (methGEV == "GUM") {

    pars <- lmomco::pargum(lmomco::lmom.ub(1:10))
    pars$para[1] <- extrem.Parameter$Mu
    pars$para[2] <- extrem.Parameter$Sigma
    quan <- lmomco::quagum(Quantile, pars)

  } else {
    stop(paste0("Die gegebene Charakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))
  }

  IDF <- do.call(rbind, lapply(quan, function(qq) do.call(c, lapply(bD, function(beta) qq / beta))))
  IDF <- round(as.data.frame(IDF), 2)
  rownames(IDF) <- Tn_Input
  colnames(IDF) <- Dauern

  DDF <- do.call(cbind, lapply(1:length(Dauern), function(dur) IDF[, dur] * Dauern_inStunden[dur]))
  DDF <- round(as.data.frame(DDF), 2)
  rownames(DDF) <- Tn_Input
  colnames(DDF) <- Dauern

  if (SerieTyp == "INT") {
    OUTPUT <- IDF
  } else if (SerieTyp == "VOL") {
    OUTPUT <- DDF
  } else {
    stop(paste0("Die gegebene Charakter fuer den SerieTyp [", SerieTyp, "] existiert nicht! Bitte INT fuer die Regenintensitaet-Dauer-Jaehrlichkeit Tabelle or VOL fuer die Regenhoehe-Dauer-Jaehrlichkeit Tabelle eingeben."))
  }

  return(OUTPUT)
}
