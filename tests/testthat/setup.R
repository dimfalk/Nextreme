## run before tests, but not loaded via `load_all()` and not installed with package

# ------------------------------------------------------------------------------

# Intervall_Korrektur(Goerlitz_maxIntSerie) |> saveRDS("serie_korr_ref.rds")

serie_korr_ref <- test_path("testdata", "serie_korr_ref.rds") |> readRDS()

# ------------------------------------------------------------------------------

# Parameter_Schaetzung(serie_korr_ref,
#                      Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
#                      methGEV = "GEV",
#                      formTyp = "FIX",
#                      Gamma = -0.1,
#                      SerieTyp = "INT") |> saveRDS("N_pars.rds")

N_pars <- test_path("testdata", "N_pars.rds") |> readRDS()

# ------------------------------------------------------------------------------

# Quantil_Schaetzung(N_pars,
#                    Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
#                    Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
#                    methGEV = "GEV",
#                    SerieTyp = "VOL") |> saveRDS("H_quas.rds")

H_quas <- test_path("testdata", "H_quas.rds") |> readRDS()

# ------------------------------------------------------------------------------

# Unsicherheit_Schaetzung(serie_korr_ref,
#                         Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
#                         methGEV = "GEV",
#                         formTyp = "FIX",
#                         Gamma = -0.1,
#                         SerieTyp = "VOL",
#                         Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
#                         nBoots = 100,
#                         rSeed = 42,
#                         Konfidenzgrenzen = c(0.025, 0.975)) |> saveRDS("H_quas_uc.rds")

H_quas_uc <- test_path("testdata", "H_quas_uc.rds") |> readRDS()

# ------------------------------------------------------------------------------

# station <- data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
#
# Kostra2020_Parameter(Standorte = station) |> saveRDS("N_pars_KOSTRA.rds")

N_pars_KOSTRA <- test_path("testdata", "N_pars_KOSTRA.rds") |> readRDS()

# ------------------------------------------------------------------------------

# station <- data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
#
# Kostra2020_hN_Schaetzung(Standorte = station,
#                          Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
#                          Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
#                          Temp_Pfad = "./",
#                          Unsicherheit = TRUE) |> saveRDS("H_quas_KOSTRA.rds")

H_quas_KOSTRA <- test_path("testdata", "H_quas_KOSTRA.rds") |> readRDS()
