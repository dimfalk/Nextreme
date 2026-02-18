test_that("Function still works as intended.", {

  x1 <- Tn_Schaetzung(N_pars, hN = 58.6, Dauern = 360, methGEV = "GEV")

  expect_equal(class(x1), "data.frame")

  expect_equal(dim(x1), c(1, 3))

  expect_equal(colnames(x1), c("hN", "D", "Tn"))

  expect_equal(x1[["hN"]], 58.6)

  expect_equal(x1[["D"]], 360)

  expect_equal(x1[["Tn"]], 23)

  x2 <- Tn_Schaetzung(N_pars_KOSTRA, hN = 58.6, Dauern = 360, methGEV = "GEV")

  expect_equal(x2[["Tn"]], 21)
})
