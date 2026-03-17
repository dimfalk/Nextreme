#' Optimierung des 2. Koutsoyiannis-Parameters
#'
#' @description
#' Optimierung des 2. Koutsoyiannis-Parameters zur Skalierung der Intensitäten nach Dauerstufe entsprechend der robusten Methode (basierend auf der Kruskal-Wallis-Statistik) nach Koutsoyiannis et al. 1998.
#'
#' @param Eta Zweiter Koutsoyiannis-Parameter.
#' @param Theta Erster Koutsoyiannis-Parameter.
#' @param Dauern Dauerstufen in Stunden, die für die Berechnung der jährlichen Serien verwendet worden sind.
#' @param Inten.Daten Maximale Niederschlagsintensitäten (in mm/h) der extrahierten jährlichen Serien, angegeben für jede Dauerstufe und jedes Jahr. Format ist `data.frame(ncol = Dauer, nrow = Jahre)`.
#' @param Partition Anzahl der Extremwerte pro Dauerstufe, die in die Berechnung der Gesamtintensität einbezogen werden sollen.
#' @param nD Anzahl der Jahre bzw. Anzahl der Extremewerte für jede Dauerstufe.
#' @param m Höchster Rang für die Niederschlagsintensitäten.
#'
#' @details Optimierung der Koutsoyiannis-Parameter durch Minimierung der Kruskal-Wallis-Statistik (KW):
#' \deqn{KW =  \frac{12}{m(m+1)} \sum_{D=1}^{k} n_D \left( \bar{r}_D - \frac{m+1}{2} \right)^2}
#' wo:
#' \itemize{
#'   \item \eqn{m} Höchster Rang für die Niederschlagsintensitäten: Anzahl aller Beobachtungen für die gesamten Dauerstufen.
#'   \item \eqn{k} Anzahl aller Dauerstufen, die für die Berechnung der jährlichen Serien verwendet worden sind.
#'   \item \eqn{n_D} Anzahl der Jahre bzw. Anzahl der Extremewerte für jede Dauerstufe \eqn{D}.
#'   \item \eqn{\bar{r}_D} Mittlerer Rang für die Stichprobe jede Dauerstufe \eqn{D}.
#' }
#' \deqn{KW\left( \theta,\eta \right) \rightarrow Min}
#' wo:
#' \itemize{
#'   \item \eqn{\theta} Erster Koutsoyiannis-Parameter.
#'   \item \eqn{\eta} Zweiter Koutsoyiannis-Parameter.
#'   }
#' @return KW Kruskal-Wallis Teststatistik
#' @keywords internal
kw_koupar2 <- function(Eta,
                       Theta = Theta,
                       Dauern = Dauern,
                       Inten.Daten = Inten.Daten,
                       Partition = Partition,
                       nD = nD,
                       m = m) {

  bD <- (Dauern + Theta)^Eta
  alle.Inten <- do.call(c, lapply(1:length(Dauern), function(i) Inten.Daten[1:Partition, i] * bD[i]))
  if (length(which(is.na(alle.Inten) == TRUE)) > 0) alle.Inten <- alle.Inten[-which(is.na(alle.Inten) == TRUE)]
  alle.sortiert <- sort(alle.Inten, decreasing = TRUE)
  rank.all <- 1:length(alle.sortiert)

  mittlere.rD <- do.call(c, lapply(1:length(Dauern), function(i) {
    Inten.jeDauer <- Inten.Daten[1:Partition, i]
    if (length(which(is.na(Inten.jeDauer) == TRUE)) > 0) Inten.jeDauer <- Inten.jeDauer[-which(is.na(Inten.jeDauer) == TRUE)]
    Inten.jeDauer <- Inten.jeDauer * bD[i]
    mittlere.Rang <- mean(unlist(sapply(Inten.jeDauer, function(k) mean(which(alle.sortiert == k)))))
    return(round(mittlere.Rang, 0))
  }))

  zweiterTeil <- sapply(1:length(Dauern), function(i) nD[i] * ((mittlere.rD[i] - ((m + 1) / 2))^2))

  KW <- (12 / (m * (m + 1))) * sum(zweiterTeil)

  return(KW)
}
