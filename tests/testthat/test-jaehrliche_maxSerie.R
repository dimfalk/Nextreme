test_that("Function output and reference objects are equal.", {

  x <- jaehrliche_maxSerie(Regendaten_01684,
                           Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                           DSDmin = 240,
                           Intervall = 5,
                           SerieTyp = "INT")

  expect_equal(x, Goerlitz_maxIntSerie)

  y <- jaehrliche_maxSerie(Regendaten_01684,
                           Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                           DSDmin = 240,
                           Intervall = 5,
                           SerieTyp = "VOL")

  expect_equal(y, Goerlitz_maxSerie)
})
