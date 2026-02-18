test_that("Function still works as intended.", {

  n <- 100

  set.seed(42)

  # synthetische GEV-verteilte Daten; stationaer/mit Trend/mit Sprung
  x <- data.frame("Jahr" = 1:n,
                  "Sensor" = factor(c(rep("analog", n / 2), rep("digital", n / 2))),
                  "xStat" = evd::rgev(n, 10, 2, 0.1),
                  "xTrend" = evd::rgev(n, 10, 2, 0.1) + 1:n * 0.05,
                  "xSprung" = c(evd::rgev(n / 2, 10, 1, 0.1), evd::rgev(n / 2, 20, 2, 0.1)))

  x1 <- Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xStat"], Sensor = x[, "Sensor"])
  x2 <- Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xTrend"], Sensor = x[, "Sensor"])
  x3 <- Trend_vs_Sprung(Zeit = x[, "Jahr"], Serienwerte = x[, "xSprung"], Sensor = x[, "Sensor"])

  expect_equal(x1[["AicRes"]], "Stat")
  expect_equal(x2[["AicRes"]], "Trend")
  expect_equal(x3[["AicRes"]], "Sprung")
})
