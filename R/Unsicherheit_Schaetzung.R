#' Schätzung der Stichprobenunsicherheit durch Bootstrapping
#'
#' @description
#' Die Stichprobenunsicherheit der Extremwertparameter und der resultierenden Quantile
#' wird auf Grundlage des Bootstrapping-Algorithmus berechnet.
#' Für weitere Informationen siehe Kap. 6.3 des Arbeitsblattes DWA-A 531 (2025).
#'
#' 1. Die jährlichen Serie wird `nBoots`-mal mit Zurücklegen neu gesampelt.
#' Die neu gesampelte Serie wird für jede Dauerstufe selektiert, sodass `nBoots`
#' neue jährliche Serien entstehen.
#'
#' 2. Für jede jährlichen Serie werden die Parameter berechnet. Die Konfidenzgrenzen
#' für jeden Parameter werden aus `nBoots` Realisierungen berechnet.
#'
#' 3. Für jeden Parametersatz wird die Niederschlagshöhe bzw. -intensität für die
#' gewünschten Dauerstufen und Wiederkehrintervalle berechnet, wobei die
#' Konfidenzgrenzen für jeden Wert aus `nBoots` Realisierungen errechnet werden.
#'
#' @param Serie data.frame. Jährliche maximale Serie mit den betrachteten Jahren
#'     als Zeilen, und den betrachteten Dauerstufen als Spalten. Die Werte sind
#'     entweder als Niederschlagsintensität (in mm/h) oder als Niederschlagshöhe
#'     (in mm) anzugegeben. Die Einheit ist über das `SerieTyp` Argument zu definieren.
#' @param Dauern numeric. Dauerstufen, die für die Berechnung der jährlichen Serien
#'     verwendet wurden, in der gleichen Einheit (entweder Minuten oder Stunden)
#'     wie das Intervall.
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
#' @param SerieTyp character. Kontrolle über die für die Augabe verwendeten Einheiten.
#'     `"VOL"` für Niederschlagshöhe (mm) und `"INT"` für Niederschlagsintensität (mm/h).
#' @param Tn numeric. Wiederkehrintervalle, für die die Niederschlagshöhe berechnet
#'     werden soll. Die Wiederkehrintervalle ist in Jahren anzugeben.
#' @param nBoots numeric. Anzahl der zufälligen Realisierungen unter Verwendung
#'     von resampelten Variationen von `Serie`.
#' @param rSeed numeric. Zufallszahl zur Steuerung des Random Number Generators,
#'     um Ergebnisse reproduzierbar zu halten.
#' @param Konfidenzgrenzen numeric. Vektor mit zwei Elementen, Festlegung der
#'     unteren und oberen Konfidenzintervallgrenze in Form von Perzentilen.
#'
#' @return list. Geschätzte Niederschlagsintensitäten (IDF) bzw. -höhen (DDF)
#'     auf Grundlage der verwendeten Extremwertparameter im ersten Eintrag `"QUA_INFO"`.
#'     Koutsoyiannis- (Theta, Eta) und GEV-Parameter (Mu, Sigma, Gamma) auf Basis
#'     der verwendeten Serie im zweiten Eintrag `"PAR_INFO"`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Beispiel 1
#' # Berechnung der Stichprobenunsicherheit durch 50 Realisierungen für die
#' # jährlichen Serien in Görlitz von 1991 bis 2020:
#' Unsicherheit <- Unsicherheit_Schaetzung(Goerlitz_maxIntSerie,
#'                                         Tn = 100,
#'                                         nBoots = 50,
#'                                         rSeed = 15,
#'                                         SerieTyp = "VOL")
#'
#' # Extraktion der Quantilsinformation
#' HN_KI <- Unsicherheit$QUA_INFO
#' dauern <- c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080)
#'
#' # geschätztes Konfidenzintervall für Tn = 100 und die gewählten Dauerstufen darstellen:
#' library(scales)
#' plot(dauern, HN_KI$Mittelwert["100", ],
#'   type = "l", lwd = 2, lty = 1, log = "xy",
#'   ylim = range(HN_KI$`95%`["100", ], HN_KI$`5%`["100", ]), col = "royalblue",
#'   ylab = "Hn [mm]", xlab = "Dauer [min]", main = "Station Görlitz"
#' )
#' polygon(c(dauern, rev(dauern)),
#'   c(HN_KI$`5%`["100", ], rev(HN_KI$`95%`["100", ])),
#'   col = alpha("royalblue", 0.5), border = NA
#' )
#' legend("topleft", c("95%KI", "Mittelwert"),
#'   col = c(alpha("royalblue", 0.5), "royalblue"), lty = c(1, 1),
#'   lwd = c(10, 2), title = "Legende", bty = "n"
#' )
#'
#' # Relative Unsicherheit für Tn = 100 und die gewählten Dauerstufen darstellen:
#' barplot(unlist(HN_KI$rel.Unsicherheit["100", ]),
#'   ylab = expression("100 x(K"[o] ~ -~ K[u] ~ ")/K"), xlab = "Dauern [min]",
#'   main = "Tn = 100 Jahre", col = hcl.colors(12, palette = "viridis")
#' )
#'
#' # Beispiel 2
#' # Berechnung der Stichprobenunsicherheit durch 100 Realisierungen für die
#' # jährlichen Serien in Görlitz von 1991 bis 2020:
#' # Betrachtung der Wiederkehrintervalle von 20, 50 und 100 Jahren.
#' Unsicherheit <- Unsicherheit_Schaetzung(Goerlitz_maxIntSerie,
#'                                         Tn = c(20, 50, 100),
#'                                         nBoots = 100,
#'                                         rSeed = 15,
#'                                         SerieTyp = "VOL")
#'
#' # Extraktion der Parameterinformationen
#' PAR_KI <- Unsicherheit$PAR_INFO
#' PAR_KI
#'
#' # Extraktion der Quantilsinformation
#' hN_KI <- Unsicherheit$QUA_INFO
#'
#' # Relative Unsicherheit für die gewählten Wiederkehrintervalle und Dauerstufen:
#' barplot(as.matrix(hN_KI$rel.Unsicherheit),
#'   beside = TRUE,
#'   ylab = expression("100 x(K"[o] ~ -~ K[u] ~ ")/K"), ylim = c(0, 50), xaxt = "n",
#'   xlab = "Dauern [min]", main = "Station Goerlitz",
#'   col = c("royalblue1", "royalblue3", "royalblue4")
#' )
#' legend("top",
#'   legend = rownames(hN_KI$rel.Unsicherheit),
#'   fill = c("royalblue1", "royalblue3", "royalblue4"), bty = "n", title = "Ta"
#' )
#' # alternative Darstellung
#' barplot(as.matrix(t(hN_KI$rel.Unsicherheit)),
#'   beside = TRUE,
#'   ylab = expression("100 x(K"[o] ~ -~ K[u] ~ ")/K"), ylim = c(0, 50),
#'   xaxt = "n", main = "Station Goerlitz", col = hcl.colors(12, palette = "viridis")
#' )
#' axis(1, at = c(7, 20, 34), rownames(hN_KI$rel.Unsicherheit))
#' legend_order <- matrix(1:12, ncol = 6, byrow = TRUE)
#' legend("top",
#'   legend = dauern[legend_order],
#'   fill = hcl.colors(12, palette = "viridis")[legend_order],
#'   bty = "n", title = "Daurn [min]", cex = 0.6, ncol = 6
#' )
#' }
Unsicherheit_Schaetzung <- function(Serie,
                                    Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                                    methGEV = "GEV",
                                    formTyp = "FIX",
                                    Gamma = -0.1,
                                    SerieTyp = "VOL",
                                    Tn = c(2, 5, 10, 20, 50, 100),
                                    nBoots = 100,
                                    rSeed = 1232,
                                    Konfidenzgrenzen = c(0.05, 0.95)) {

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
    stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlende Zeile.")
  } else if (any(apply(Serie, 2, function(i) all(is.na(i) == TRUE) == TRUE))) {
    stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")
  } else if (any(Serie <= 0)) stop("Das Serie Input enthaelt negative oder Null Werte!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if (class(Dauern) != "numeric") {
    stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  } else if (length(Dauern) == 1) {
    stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  } else if (any(is.na(Dauern) == TRUE)) {
    stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Dauern <= 0)) stop("Das Dauern Input enthaelt Negative oder Null Werte!")

  # Bedingung 3: Laenge und Namen der Dauer sollte mit der Anzahl und Namen der Spalten in der jaehrlichenSerie uebereinstimmen
  if (dim(Serie)[2] != length(Dauern)) {
    stop("Die Anzahl der Spalten der jaehrlichenSerie sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")
  } else if (identical(as.numeric(colnames(Serie)), Dauern) == FALSE) stop("Die Spaltennamen der jaehrlichenSerie sollten mit den bei Vektor Dauern angegebenen Dauern identisch sein.")

  # Bedingung 4: Das Tn Dauern sollte als Zahlenvektor angegeben werden und mehr als 0 Element haben.
  if (class(Tn) != "numeric") {
    stop("Das Tn Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben.")
  } else if (length(Tn) == 0) {
    stop("Das Tn Input ist leer!")
  } else if (any(is.na(Tn) == TRUE)) {
    stop("Das Tn Input hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Tn <= 0)) stop("Das Tn Input enthaelt negative oder Null Werte!")

  # Bedingung 5: Das Input Gamma sollte nur ein Element vom Typ numerisch und nicht Null sein!
  if (class(Gamma) != "numeric") {
    stop("Formparameter Gamma sollte vom numerischen Typ sein!")
  } else if (length(Gamma) != 1) {
    stop("Formparameter Gamma sollte nur 1 Element haben!")
  } else if (Gamma == 0) stop("Der Formparameter Gamma kann nicht 0 sein! Bitte verwenden Sie methGEV als GUM, wenn Sie den Formparameter auf 0 setzen wollen.")

  # Bedingung 6: methGEV sollte nur ein Element vom Typ Charakter sein!
  if (class(methGEV) != "character") {
    stop("Das methGEV Input sollte vom Charaktertyp sein!")
  } else if (length(methGEV) != 1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 7: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if (class(SerieTyp) != "character") {
    stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  } else if (length(SerieTyp) != 1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  # Bedingung 8: formTyp sollte nur ein Element vom Typ Charakter sein!
  if (class(formTyp) != "character") {
    stop("Das formTyp Input sollte vom Charaktertyp sein!")
  } else if (length(formTyp) != 1) stop("Das formTyp Input sollte nur 1 Element haben!")

  # Bedingung 9: Das Input Konfidenzgrenzen sollte als Zahlenvektor angegeben werden und nur 2 Elemente haben.
  if (class(Konfidenzgrenzen) != "numeric") {
    stop("Das Konfidenzgrenzen Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  } else if (length(Konfidenzgrenzen) != 2) {
    stop("Das Konfidenzgrenzen Input hat nicht 2 Elemente! Bitte geben Sie nur 2 Elemente.")
  } else if (any(is.na(Konfidenzgrenzen) == TRUE)) {
    stop("Das Dauern Konfidenzgrenzen hat fehlende Werte! Bitte entfernen Sie diese.")
  } else if (any(Konfidenzgrenzen <= 0)) {
    stop("Das Dauern Konfidenzgrenzen enthaelt Negative oder Null Werte!")
  } else if (any(Konfidenzgrenzen >= 1)) stop("Das Dauern Konfidenzgrenzen enthaelt Werte groesser als oder gleich mit 1!")

  # Bedingung 10: Das Input rSeed sollte nur ein Element vom Typ numerisch und nicht fehlende sein!
  if (class(rSeed) != "numeric") {
    stop("Das Input rSeed sollte vom numerischen Typ sein!")
  } else if (length(rSeed) != 1) stop("Das Input rSeed sollte nur 1 Element haben!")

  # Bedingung 11: Das Input nBoots sollte nur ein Element vom Typ numerisch und nicht kleiner als 1 sein!
  if (class(nBoots) != "numeric") {
    stop("Das Input nBoots sollte vom numerischen Typ sein!")
  } else if (length(nBoots) != 1) {
    stop("Das Input nBoots sollte nur 1 Element haben!")
  } else if (nBoots <= 1) stop("Das Input nBoots darf nicht kleiner als 1 sein!")

  All_Boots <- lapply(1:nBoots, function(Boot) {
    set.seed(rSeed + Boot)
    newAMS <- Serie[sample(1:dim(Serie)[1], size = dim(Serie)[1], replace = TRUE), ]
    newStats <- Parameter_Schaetzung(newAMS, Dauern = Dauern, methGEV = methGEV, Gamma = Gamma, formTyp = formTyp)
    newQuans <- Quantil_Schaetzung(newStats, Dauern = Dauern, Tn = Tn, methGEV = methGEV, SerieTyp = SerieTyp)
    return(list(Pars = unlist(newStats), Quas = unlist(newQuans)))
  })

  PAR_Boots <- do.call(rbind, lapply(All_Boots, function(i) i$Pars))
  PAR_Boots <- as.data.frame(PAR_Boots[, which(colnames(PAR_Boots) %in% c("Mu", "Sigma", "Gamma", "Theta", "Eta"))])
  QUA_Boots <- do.call(rbind, lapply(All_Boots, function(i) i$Quas))

  QUA_INFO <- lapply(Konfidenzgrenzen, function(x) {
    data <- as.data.frame(matrix(apply(QUA_Boots, 2, function(sim) stats::quantile(sim, prob = x, na.rm = TRUE)), nrow = length(Tn), ncol = length(Dauern), byrow = FALSE))
    rownames(data) <- Tn
    colnames(data) <- Dauern
    return(data)
  })

  names(QUA_INFO) <- paste0(Konfidenzgrenzen * 100, "%")
  QUA_INFO$Mittelwert <- as.data.frame(matrix(apply(QUA_Boots, 2, mean, na.rm = T), nrow = length(Tn), ncol = length(Dauern), byrow = FALSE))
  rownames(QUA_INFO$Mittelwert) <- Tn
  colnames(QUA_INFO$Mittelwert) <- Dauern
  QUA_INFO$rel.Unsicherheit <- 100 * (QUA_INFO[[2]] - QUA_INFO[[1]]) / QUA_INFO$Mittelwert

  PAR_INFO <- lapply(Konfidenzgrenzen, function(x) {
    data <- as.data.frame(matrix(apply(PAR_Boots, 2, function(sim) stats::quantile(as.numeric(sim), prob = x, na.rm = TRUE)), nrow = 1, byrow = FALSE))
    colnames(data) <- c("Mu", "Sigma", "Gamma", "Theta", "Eta")
    return(data)
  })

  names(PAR_INFO) <- paste0(Konfidenzgrenzen * 100, "%")
  PAR_INFO$Mittelwert <- as.data.frame(matrix(apply(PAR_Boots, 2, function(i) mean(as.numeric(i), na.rm = TRUE)), nrow = 1, byrow = FALSE))
  colnames(PAR_INFO$Mittelwert) <- c("Mu", "Sigma", "Gamma", "Theta", "Eta")
  PAR_INFO$rel.Unsicherheit <- 100 * (PAR_INFO[[2]] - PAR_INFO[[1]]) / PAR_INFO$Mittelwert
  PAR_INFO <- do.call(rbind, PAR_INFO)

  CI_INFO <- list(QUA_INFO = QUA_INFO, PAR_INFO = PAR_INFO)

  return(CI_INFO)
}
