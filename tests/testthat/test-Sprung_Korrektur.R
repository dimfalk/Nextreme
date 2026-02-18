test_that("Function output and reference object are equal.", {

  x <- Sprung_Korrektur(serie_korr_ref, wechselDatum = as.Date("1991-01-01"))

  expect_equal(x, serie_korr_ref)
})
