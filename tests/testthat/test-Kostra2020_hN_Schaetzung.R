test_that("Function still works as intended.", {

  station <- data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)

  x <- Kostra2020_hN_Schaetzung(Standorte = station,
                                            Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                                            Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
                                            Temp_Pfad = "./",
                                            Unsicherheit = TRUE)

  expect_equal(class(x), "list")

  expect_equal(length(x), 2)

  expect_equal(names(x), c("Kostra_HN", "Kostra_UC"))

  expect_equal(class(x[[1]]), "data.frame")

  expect_equal(class(x[[2]]), "data.frame")

  expect_equal(dim(x[[1]]), c(1, 111))

  expect_equal(dim(x[[2]]), c(1, 111))

  expect_equal(x, H_quas_KOSTRA)
})
