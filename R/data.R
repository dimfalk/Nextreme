#' Data: Niederschlagszeitreihe Görlitz
#'
#' @description
#' Niederschlagshöhe (in mm) an der DWD-Station 01684 Görlitz als 5-minütliche
#' Zeitreihe für den Zeitraum 1991-01-01 00:00 UTC bis 2020-12-31 23:55 UTC. \cr
#' Y = 51.1621° N; X = 14.9506° E; Z = 239 m a.s.l.
#'
#' @format A data frame with 3.155.904 rows and 2 variables:
#' \describe{
#'   \item{Datum}{POSIXct object. Index of the timeseries.}
#'   \item{RH}{numeric. Precipitation depth (in mm) as coredata of the timeseries.}
#' }
#' @examples
#' head(Regendaten_01684)
#' tail(Regendaten_01684)
"Regendaten_01684"

#' Data: Jährliche maximale Niederschlagshöhe
#'
#' @description
#' Beispielausgabe der Funktion `jaehrliche_maxSerie(..., SerieTyp = "VOL")`
#' unter Verwendung des Beispieldatensatzes `Regendaten_01684`.
#'
#' @format A data frame with 30 rows and 12 variables:
#' \describe{
#'   \item{5}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 5 Minuten.}
#'   \item{10}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 10 Minuten.}
#'   \item{15}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 15 Minuten.}
#'   \item{30}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 30 Minuten.}
#'   \item{60}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 60 Minuten.}
#'   \item{120}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 120 Minuten.}
#'   \item{360}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 360 Minuten.}
#'   \item{720}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 720 Minuten.}
#'   \item{1440}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 1440 Minuten.}
#'   \item{2880}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 2880 Minuten.}
#'   \item{4320}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 4320 Minuten.}
#'   \item{10080}{Jährliche maximale Niederschlagshöhe für die Dauerstufe von 10080 Minuten.}
#' }
#' @examples
#' head(Goerlitz_maxSerie)
#' tail(Goerlitz_maxSerie)
"Goerlitz_maxSerie"

#' Data: Jährliche maximale Niederschlagsintensität
#'
#' @description
#' Beispielausgabe der Funktion `jaehrliche_maxSerie(..., SerieTyp = "INT")`
#' unter Verwendung des Beispieldatensatzes `Regendaten_01684`.
#'
#' @format A data frame with 30 rows and 12 variables:
#' \describe{
#'   \item{5}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 5 Minuten.}
#'   \item{10}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 10 Minuten.}
#'   \item{15}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 15 Minuten.}
#'   \item{30}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 30 Minuten.}
#'   \item{60}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 60 Minuten.}
#'   \item{120}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 120 Minuten.}
#'   \item{360}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 360 Minuten.}
#'   \item{720}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 720 Minuten.}
#'   \item{1440}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 1440 Minuten.}
#'   \item{2880}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 2880 Minuten.}
#'   \item{4320}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 4320 Minuten.}
#'   \item{10080}{Jährliche maximale Niederschlagsintensität für die Dauerstufe von 10080 Minuten.}
#' }
#' @examples
#' head(Goerlitz_maxIntSerie)
#' tail(Goerlitz_maxIntSerie)
"Goerlitz_maxIntSerie"
