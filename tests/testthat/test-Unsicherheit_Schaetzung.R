test_that("Function output and reference object are equal.", {

  uc <- Unsicherheit_Schaetzung(serie_korr_ref,
                                Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
                                methGEV = "GEV",
                                formTyp = "FIX",
                                Gamma = -0.1,
                                SerieTyp = "VOL",
                                Tn = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
                                nBoots = 100,
                                rSeed = 42,
                                Konfidenzgrenzen = c(0.025, 0.975))

  expect_equal(class(uc), "list")

  expect_equal(length(uc), 2)

  expect_equal(names(uc), c("QUA_INFO", "PAR_INFO"))

  expect_equal(class(uc[[1]]), "list")

  expect_equal(length(uc[[1]]), 4)

  expect_equal(names(uc[[1]]), c("2.5%", "97.5%", "Mittelwert", "rel.Unsicherheit"))

  expect_equal(class(uc[[2]]), "data.frame")

  expect_equal(dim(uc[[2]]), c(4, 5))

  expect_equal(colnames(uc[[2]]), c("Mu", "Sigma", "Gamma", "Theta", "Eta"))

  expect_equal(rownames(uc[[2]]), c("2.5%", "97.5%", "Mittelwert", "rel.Unsicherheit"))

  expect_equal(class(uc[[1]][[3]]), "data.frame")

  expect_equal(dim(uc[[1]][[3]]), c(9, 12))

  expect_equal(colnames(uc[[1]][[3]]), c("5", "10", "15", "30", "60", "120", "360",
                                         "720", "1440", "2880", "4320", "10080"))

  expect_equal(rownames(uc[[1]][[3]]), c("1", "2", "3", "5", "10", "20", "30", "50", "100"))

  expect_equal(uc, H_quas_uc)
})
