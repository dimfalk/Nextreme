#' Data: Niederschlagszeitreihe Görlitz
#'
#' Die Beispiel-Niederschlagszeitreihe fuer die Station Goerlitz:
#' in 5min-Zeitschritten und als mm/5min
#' Start Time 1991-01-01 00:00
#' End Time 2020-12-31 23:55
#' Time Zone UTC
#' Missing values as NA
#' @format A data frame with 3155904 rows and 2 variables:
#' \describe{
#'   \item{Datum}{A POSIXct value describing the time step as YearMonthDayHourMinute}
#'   \item{RH}{A numeric measurement for the precipitation in mm/5min}
#' }
#' @examples
#' head(Regendaten_01684)
#' tail(Regendaten_01684)
"Regendaten_01684"

#' Data: Jaehrliche maximale Regenhoehe
#'
#' Das Beispiel der jaehrlichen maximalen Regenhoehe fuer die Station Goerlitz.
#' Regenhoehe ist mm/Dauer gegeben
#' fuer Dauern: 5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320 und 10080 Minuten.
#' Beobachtungen sind fuer die Jahre von 1991 bis 2020.
#' @format A data frame with 30 rows and 12 variables:
#' \describe{
#'   \item{5}{jaehrliche maximale Regenhoehe fuer 5 Minuten Dauer}
#'   \item{10}{jaehrliche maximale Regenhoehe fuer 10 Minuten Dauer}
#'   \item{15}{jaehrliche maximale Regenhoehe fuer 15 Minuten Dauer}
#'   \item{30}{jaehrliche maximale Regenhoehe fuer 30 Minuten Dauer}
#'   \item{60}{jaehrliche maximale Regenhoehe fuer 60 Minuten Dauer}
#'   \item{120}{jaehrliche maximale Regenhoehe fuer 120 Minuten Dauer}
#'   \item{360}{jaehrliche maximale Regenhoehe fuer 360 Minuten Dauer}
#'   \item{720}{jaehrliche maximale Regenhoehe fuer 720 Minuten Dauer}
#'   \item{1440}{jaehrliche maximale Regenhoehe fuer 1440 Minuten Dauer}
#'   \item{2880}{jaehrliche maximale Regenhoehe fuer 2880 Minuten Dauer}
#'   \item{4320}{jaehrliche maximale Regenhoehe fuer 4320 Minuten Dauer}
#'   \item{10080}{jaehrliche maximale Regenhoehe fuer 10080 Minuten Dauer}
#' }
#' @examples
#' head(Goerlitz_maxSerie)
#' tail(Goerlitz_maxSerie)
"Goerlitz_maxSerie"

#' Data: Jaehrliche maximale Regenintensitaet
#'
#' Das Beispiel der jaehrlichen maximalen Regenintensitaet fuer die Station Goerlitz
#' Regenintensitaet ist in mm/h gegeben
#' fuer Dauern: 5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320 und 10080 Minuten.
#' Beobachtungen sind fuer die Jahre von 1991 bis 2020.
#' @format A data frame with 30 rows and 12 variables:
#' \describe{
#'   \item{5}{jaehrliche maximale Regenintensitaet fuer 5 Minuten Dauer}
#'   \item{10}{jaehrliche maximale Regenintensitaet fuer 10 Minuten Dauer}
#'   \item{15}{jaehrliche maximale Regenintensitaet fuer 15 Minuten Dauer}
#'   \item{30}{jaehrliche maximale Regenintensitaet fuer 30 Minuten Dauer}
#'   \item{60}{jaehrliche maximale Regenintensitaet fuer 60 Minuten Dauer}
#'   \item{120}{jaehrliche maximale Regenintensitaet fuer 120 Minuten Dauer}
#'   \item{360}{jaehrliche maximale Regenintensitaet fuer 360 Minuten Dauer}
#'   \item{720}{jaehrliche maximale Regenintensitaet fuer 720 Minuten Dauer}
#'   \item{1440}{jaehrliche maximale Regenintensitaet fuer 1440 Minuten Dauer}
#'   \item{2880}{jaehrliche maximale Regenintensitaet fuer 2880 Minuten Dauer}
#'   \item{4320}{jaehrliche maximale Regenintensitaet fuer 4320 Minuten Dauer}
#'   \item{10080}{jaehrliche maximale Regenintensitaet fuer 10080 Minuten Dauer}
#' }
#' @examples
#' head(Goerlitz_maxIntSerie)
#' tail(Goerlitz_maxIntSerie)
"Goerlitz_maxIntSerie"
