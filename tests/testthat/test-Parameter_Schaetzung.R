test_that("Function output and reference object are equal.", {

  x <- Parameter_Schaetzung(serie_korr_ref,
                            Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                            methGEV = "GEV",
                            formTyp = "FIX",
                            Gamma = -0.1,
                            SerieTyp = "INT")

  expect_equal(class(x), "data.frame")

  expect_equal(dim(x), c(1, 6))

  expect_equal(colnames(x), c("Mu", "Sigma", "Gamma", "Theta", "Eta", "KW"))

  expect_equal(x, N_pars)
})
