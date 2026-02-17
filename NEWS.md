# Nextreme 1.2.12

## 2026-02-15 | @dimfalk

1. Nextreme vignette is now made available in /vignettes/Nextreme.Rmd

2. Nextreme vignette and package manual are now available as pdf files in /inst/doc/

3. minor code and text restyling of Nextreme vignette to enhance readability

4. README.Rmd is now available

5. all functions are now consistently exported using the `@export` roxygen2 tag

6. major code re-styling making use of `{styler}` to enhance readability

7. `{scales}` dependency removed

8. `Kostra2020_Parameter()` and `Kostra2020_hN_Schaetzung()`: downloaded files are now cleaned after execution

9. `Trend_vs_Sprung()`: `sil = T` was changed to `silent = TRUE` as suggested by R CMD Check

10. `Parameter_Schaetzung()`: Non-ASCII characters were removed from documentation as suggested by R CMD Check

11. `Unsicherheit_Schaetzung()` and `Tn_Schaetzung()`: modified argument order to support recognition



# Nextreme 1.2.1

## 2025-05-28 | @shehuBora

1. **jaehrliche_maxSerie.R** (vorgeschlagen am 18.03.2024 von Bora Shehu und Angelika Palarz):

	 Die Berechnung der Jahresmaxima wurde komplett überarbeitet, um folgende Punkte zu berücksichtigen: 
	 
	 a) fehlende Werte - so wird kein Fehler mehr angezeigt, wenn in der Zeitreihe fehlende Werte vorhanden sind.
	 
	 b) jährliche Minimun an verfügbaren Messungen - hierzu wurden zwei Bedingungen implementiert:
	 
	    - zwischen März und Oktober mindestens 172 Tage mit vollständigen Beobachtungen.
	    - zwischen April und September mindestens 127 Tage mit vollständigen Beobachtungen.
			     
   c) Unabhängigkeit von Ereignissen - es wurde eine Bedingung für Maximalwerte (mit einer Dauer von mehr als 4 Stunden) hinzugefügt, die am Ende des Kalenderjahres auftreten. 
      
      - Wenn die Bedingung erfüllt ist, wird die Fortsetzung des Ereignisses im nächsten Jahr auf 0 gesetzt. 
      - Die Fortsetzung des Ereignisses basiert auf einer Trockenheitsdauer (`DSDmin`) von 4 Stunden. 

2. **Sprung_Elimination.R** (vorgeschlagen am 16.12.2024 von Thomas Junghänel und Winfrid Willems):
   
   Bedingung eingeführt, um Stationen einzubeziehen, bei denen der Mittelwert von analogem `PR0` größer ist als der Mittelwert von digitalem `PR1`.
			 
3. **Sprung_Korrektur.R** (vorgeschlagen am 16.12.2024 von Thomas Junghänel und Winfrid Willems):

	 Es wird nun geprüft, ob in Spalten mit einer Dauer <= 30 Minuten ein "Sprung" vorliegt. 
	 Falls mindestens ein "Sprung" festgestellt wird, werden alle AMS-Werte für Dauerstufen <= 30 Minuten entsprechend korrigiert. 
			 
4. **[alle].R** (vorgeschlagen am 19.09.2024 von Thomas Junghänel):
			 
	 Jede importierte Funktion aus externen Packages (z.B. {lubridate}, {terra}) wurde in das folgende Format überführt:
	 
	 `lubridate::year()`
	 
	 `require(lubridate)` entfällt somit.
			 
5. **README.md** hinzugefügt (vorgeschlagen am 19.09.2024 von Thomas Junghänel):

   - Paketbeschreibung
   - Installationsanweisungen
   - Grundlegende Anwendungsbeispiele
			
6. **NEWS.md** hinzugefügt (vorgeschlagen am 23.05.2025 von Bora Shehu):

	 Enthält Hauptänderungen seit erstem Release von Nextreme_1.1.0
			 
7. **Kostra2020_hN_Schaetzung.R** und **Kostra2020_Parameter.R** (vorgeschlagen am 23.05.2025 von Bora Shehu):

   Die Funktionen aus dem Package {rdwd} zum Lesen der KOSTRA-Rasterdaten wurden ersetzt, die Abhängigkeit entfällt.

8. Die Funktionsbeschreibung, das Handbuch und die Einleitung wurden aufgrund der Kommentare des DWD aktualisiert (01.06.2025)



# Nextreme 1.2.0

## 2024-10-07 | @shehuBora

1. **kw_koupar1.R** und **kw_koupar2.R** (vorgeschlagen am 07.10.2024 von Jennifer Ostermöller).

   Die Berechnung des Rangs für die Schätzung des Koustoyiannis-Parameters wurde geändert:
   
   `mittlere.Rang <- mean(unlist(sapply(Inten.jeDauer, function(k) mean(which(alle.sortiert == k)))))`
			 
2. **Kostra2020_hN_Schaetzung.R** und **Kostra2020_Parameter.R** (vorgeschlagen am 17.07.2024 von Thomas Junghänel, Jennifer Oestermöller und Angelika Palarz)
	 
	 Die Projektion der KOSTRA-DWD-2020-Daten wurde aktualisiert:
	 
	 `Kostra_CRS <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"`

