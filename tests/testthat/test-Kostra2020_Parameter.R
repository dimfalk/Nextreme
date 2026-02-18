test_that("Function still works as intended.", {

  station <- data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)

  x <- Kostra2020_Parameter(Standorte = station)

  expect_equal(class(x), "data.frame")

  expect_equal(dim(x), c(1, 8))

  expect_equal(colnames(x), c("ID", "geoBreite", "geoLaenge", "Theta", "Eta", "Mu", "Sigma", "Gamma"))

  expect_equal(x, N_pars_KOSTRA)
})
