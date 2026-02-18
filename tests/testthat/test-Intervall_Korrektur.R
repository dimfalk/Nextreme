test_that("Function output and reference object are equal.", {

  x <- Intervall_Korrektur(Goerlitz_maxIntSerie)

  expect_equal(x, serie_korr_ref)
})
