test_that("Function output and reference object are equal.", {

  x <- Quantil_Schaetzung(N_pars,
                          Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                          Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
                          methGEV = "GEV",
                          SerieTyp = "VOL")

  expect_equal(class(x), "data.frame")

  expect_equal(dim(x), c(9, 12))

  expect_equal(colnames(x), c("5", "10", "15", "30", "60", "120", "360", "720",
                              "1440", "2880", "4320", "10080"))

  expect_equal(rownames(x), c("1", "2", "3", "5", "10", "20", "30", "50", "100"))

  expect_equal(x, H_quas)
})
