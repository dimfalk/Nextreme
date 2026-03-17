#' Schätzung der Extremwert-Parameter auf Basis der jährlichen Serie
#'
#' @param Serie data.frame. Jährliche maximale Serie mit den betrachteten Jahren als Zeilen,
#'     und den betrachteten Dauerstufen als Spalten. Die Werte sind entweder als
#'     Niederschlagsintensität (in mm/h) oder als Niederschlagshöhe (in mm) anzugegeben.
#'     Die Einheit ist über das `SerieTyp` Argument zu definieren.
#' @param Dauern numeric. Dauerstufen, die für die Berechnung der jährlichen Serien
#'     verwendet wurden, in der gleichen Einheit (entweder Minuten oder Stunden) wie das Intervall.
#' @param methGEV character. Typ der Generalisierten Extremwertverteilung (GEV),
#'     die an die jährliche Serie angepasst werden soll:
#'     `"GEV"` für Typ 2 oder Typ 3 (Formparameter \code{!= 0}; Fréchet & Weibull) und
#'     `"GUM"` für Typ 1 (Formparameter \code{== 0}; Gumbel)
#' @param formTyp character. Kontrolliert, wie der Formparameter der GEV
#'     (nur bei `methGEV = "GEV"`) geschätzt werden soll:
#'     `"CON"` berechnet die Formparameter auf der Basis der L-Momente,
#'     `"FIX"`  erzwingt einen bestimmten Wert für den Formparameter.
#' @param Gamma numeric. Fixierter Wert des GEV-Formparameters.
#'     Nur relevant, wenn `methGEV = "GEV"` und `formTyp = "FIX"`.
#' @param SerieTyp character. Information über die in `Serie` verwendeten Einheiten.
#'     `"VOL"` für Niederschlagshöhe (mm) und `"INT"` für Niederschlagsintensität (mm/h).
#'
#' @details
#' Berechnung der Koutsoyiannis- und GEV-Parameter auf Basis der jährlichen Serien
#' für unterschiedliche Dauerstufen.
#'
#' 1. Die Koutsoyiannis-Parameter normalisieren die Intensitäten über alle
#' Dauerstufen und werden auf Grundlage der Kruskal-Wallis-Statistik geschätzt.
#'
#' 2. Die GEV-Parameter werden nach Methode der L-Momente geschätzt
#' (mit Ausnahme des Formparameters, der auf einen bestimmten Wert fixiert werden kann).
#'
#' @return data.frame. Koutsoyiannis- (Theta, Eta) und GEV-Parameter (Mu, Sigma, Gamma)
#' auf Basis der verwendeten Serie (einzeilig).
#' @export
#'
#' @examples
#' # Berechnung der dauerstufenÜbergreifenden Verteilungsparameter fÜr die
#' # Station GÖrlitz im Zeitraum 1991-2020, ohne Intervall- oder Sprungkorrektur
#' Dauern <- c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080)
#'
#' # Fall 1: GEV-Verteilung Über alle Dauerstufen mit auf -0.1 fixiertem Formparameter
#' Parameter_Schaetzung(Goerlitz_maxIntSerie, Dauern, methGEV = "GEV", formTyp = "FIX", Gamma = -0.1)
#'
#' # Fall 2: GEV-Verteilung Über alle Dauerstufen mit mittels L-Momenten geschätztem Formparameter
#' Parameter_Schaetzung(Goerlitz_maxIntSerie, Dauern, methGEV = "GEV", formTyp = "CON")
#'
#' # Fall 3: Gumbel-Verteilung Über alle Dauerstufen
#' Parameter_Schaetzung(Goerlitz_maxIntSerie, Dauern, methGEV = "GUM")
Parameter_Schaetzung <- function(Serie,
                                 Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                                 methGEV = "GEV",
                                 formTyp = "FIX",
                                 Gamma = -0.1,
                                 SerieTyp = "INT") {

  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if (missing(Serie)) {
    stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  } else if (class(Serie) != "data.frame") {
    stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  } else if (dim(Serie)[1] < 5) {
    stop("Das Serie Input enthaelt weniger als 5 Jahren! Um Fehler zu vermeiden, sind mindestens 5 Jahren erforderlich.")
  } else if (dim(Serie)[2] == 1) {
    stop("Das Serie Input enthaelt nur ein Dauer! Um Fehler zu vermeiden, sind mindestens 2 Dauern erforderlich.")
  } else if (any(apply(Serie, 1, function(i) all(is.na(i) == TRUE) == TRUE))) {
    warning("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Die fehlenden Zeilen werden aus dem Datenframe entfernt.")
    Serie <- Serie[which(apply(Serie, 1, function(i) all(is.na(i) == TRUE) == TRUE) == FALSE), ]
  } else if (any(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE))) {
    warning("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten! Die fehlenden Spalten werden aus dem Datenframe entfernt.")
    Serie <- Serie[, which(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE) == FALSE)]
    Dauern <- Dauern[which(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE) == FALSE)]
  } else if (any(Serie <= 0)) stop("Das Serie Input enthaelt negative oder Null Werte!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if (class(Dauern) != "numeric") {
    stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  } else if (length(Dauern) == 1) {
    stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  } else if (any(is.na(Dauern) == TRUE)) {
    stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Dauern <= 0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")

  # Bedingung 3: Laenge und Namen der Dauer sollte mit der Anzahl und Namen der Spalten in der jaehrlichenSerie uebereinstimmen
  if (dim(Serie)[2] != length(Dauern)) {
    stop("Die Anzahl der Spalten der jaehrlichenSerie sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")
  } else if (identical(as.numeric(colnames(Serie)), Dauern) == F) stop("Die Spaltennamen der jaehrlichenSerie sollten mit den bei Vektor Dauern angegebenen Dauern identisch sein.")

  # Bedingung 4: Das Input Gamma sollte nur ein Element vom Typ numerisch und nicht Null sein!
  if (class(Gamma) != "numeric") {
    stop("Formparameter Gamma sollte vom numerischen Typ sein!")
  } else if (length(Gamma) != 1) {
    stop("Formparameter Gamma sollte nur 1 Element haben!")
  } else if (Gamma == 0) stop("Der Formparameter Gamma kann nicht 0 sein! Bitte verwenden Sie methGEV als GUM, wenn Sie den Formparameter auf 0 setzen wollen.")

  # Bedingung 5: methGEV sollte nur ein Element vom Typ Charakter sein!
  if (class(methGEV) != "character") {
    stop("Das methGEV Input sollte vom Charaktertyp sein!")
  } else if (length(methGEV) != 1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 6: formTyp sollte nur ein Element vom Typ Charakter sein!
  if (class(formTyp) != "character") {
    stop("Das formTyp Input sollte vom Charaktertyp sein!")
  } else if (length(formTyp) != 1) stop("Das formTyp Input sollte nur 1 Element haben!")

  # Bedingung 7: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if (class(SerieTyp) != "character") {
    stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  } else if (length(SerieTyp) != 1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  Dauern_inStunden <- round(Dauern / 60, 3)
  j <- 1:length(Dauern_inStunden)
  nD <- apply(Serie[, 1:(length(Dauern_inStunden))], 2, function(i) {
    if (length(which(is.na(i) == TRUE)) > 0) {
      out <- length(i[-which(is.na(i) == TRUE)])
    } else {
      out <- length(i)
    }
    return(out)
  })

  ##### Converting to Intensities
  if (SerieTyp == "INT") {
    Serie <- Serie
  } else if (SerieTyp == "VOL") {
    Serie <- round(Serie / as.data.frame(t(replicate(dim(Serie)[1], Dauern_inStunden))), 3)
  } else {
    stop(paste0("Die gegebene Charakter fuer den SerieTyp [", SerieTyp, "] existiert nicht! Bitte INT fuer die Regenintensitaet (mm/h) or VOL fuer die Regenhoehe (mm/h) eingeben."))
  }

  ##### Intensitaeten sollten in mm/h vorliegen
  Inten.Daten <- do.call(cbind, lapply(1:length(Dauern_inStunden), function(i) sort(round(Serie[, i], 3), na.last = TRUE, decreasing = TRUE)))
  Partition <- round(as.numeric(min(nD[1], na.rm = TRUE)), 0)

  # SCHRITT 1: Berechnung der Koutsoyiannis-Parameter
  m <- Partition * length(Dauern_inStunden)

  Theta.Werte <- stats::optimize(kw_koupar1,
                                 lower = 0,
                                 upper = 1,
                                 Inten.Daten = Inten.Daten,
                                 Dauern = Dauern_inStunden,
                                 Partition = Partition,
                                 nD = nD,
                                 m = m,
                                 maximum = FALSE)

  Eta.Werte <- stats::optimize(kw_koupar2,
                               lower = 0,
                               upper = 1,
                               Dauern = Dauern_inStunden,
                               Theta = Theta.Werte$minimum,
                               Inten.Daten = Inten.Daten,
                               Partition = Partition,
                               nD = nD,
                               m = m,
                               maximum = FALSE)

  output <- data.frame(Theta = Theta.Werte$minimum, Eta = Eta.Werte$minimum, KW = Eta.Werte$objective)
  bD <- (Dauern_inStunden + output$Theta) ^ output$Eta
  alle.Inten <- do.call(c, lapply(1:length(Dauern_inStunden), function(i) Inten.Daten[, i] * bD[i]))

  # falls vorhanden, fehlende Werte entfernen
  if (length(which(is.na(alle.Inten) == TRUE)) > 0) alle.Inten <- alle.Inten[-which(is.na(alle.Inten) == TRUE)]

  # SCHRITT 2: Berechnung der GEV-Parameter
  lmoments <- lmomco::lmom.ub(unlist(alle.Inten))

  if (methGEV == "GEV") {
    if (formTyp == "FIX") {
      gev.par <- pargev2(lmoments, kappa = Gamma)
    } else if (formTyp == "CON") {
      gev.par <- lmomco::pargev(lmoments)
    } else {
      stop(paste0("Die gegebene Chararakter fuer den Formtyp [", formTyp, "] existiert nicht! Bitte FIX oder CON eingeben."))
    }

    extrem.Parameter <- data.frame(Mu = gev.par$para[1],
                                   Sigma = gev.par$para[2],
                                   Gamma = gev.par$para[3],
                                   Theta = output$Theta,
                                   Eta = output$Eta,
                                   KW = output$KW)

  } else if (methGEV == "GUM") {

    gum.par <- lmomco::pargum(lmoments)

    extrem.Parameter <- data.frame(Mu = gum.par$para[1],
                                   Sigma = gum.par$para[2],
                                   Gamma = 0,
                                   Theta = output$Theta,
                                   Eta = output$Eta,
                                   KW = output$KW)
  } else {
    stop(paste0("Die gegebene Chararakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))
  }

  rownames(extrem.Parameter) <- NULL

  return(extrem.Parameter)
}
