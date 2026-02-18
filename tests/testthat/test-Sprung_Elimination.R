test_that("Function still works as intended.", {

  n1 <- 50
  n2 <- 50
  m1 <- 10
  m2 <- 20

  set.seed(42)

  x <- data.frame("Sensor" = c(rep("analog", n1), rep("digital", n2)),
                  "Wert" = c(rnorm(n1, m1), rnorm(n2, m2)))

  res <- Sprung_Elimination(x$Wert, x$Sensor)

  expect_equal(class(res), "data.frame")

  expect_equal(dim(res), c(100, 2))

  expect_equal(colnames(res), c("SensorZ", "SerieNeu"))

  expect_equal(round(sum(res[["SerieNeu"]]), 2), 2010.07)
})
