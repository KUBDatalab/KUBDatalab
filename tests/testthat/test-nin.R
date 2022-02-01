context("test-nin")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)

test_that("negated %in% works", {
  t1 <- sample(1:10, 5, replace =T)
  t2 <- sample(5:15, 10, replace =T)
  expect_equal(t1 %nin% t2, !(t1 %in% t2))
})
