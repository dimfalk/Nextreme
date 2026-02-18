test_that("Input data was not modified.", {

  expect_equal(class(Regendaten_01684), "data.frame")

  expect_equal(dim(Regendaten_01684), c(3155904, 2))

  expect_equal(colnames(Regendaten_01684), c("Datum", "RH"))

  expect_equal(range(Regendaten_01684$Datum), structure(c(662688000, 1609458900),
                                                        class = c("POSIXct", "POSIXt"),
                                                        tzone = "UTC"))

  expect_equal(as.numeric(unique(diff(Regendaten_01684$Datum))), 5)

  expect_equal(range(Regendaten_01684$RH, na.rm = TRUE), c(0, 15.938))

  expect_equal(round(mean(Regendaten_01684$RH, na.rm = TRUE), 8), 0.00616832)

  expect_equal(sum(is.na(Regendaten_01684$RH)), 120999)
})
