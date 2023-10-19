context("test-geom_spiral")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)

test_that("geom_spiral handles NULL data", {
  expect_error(geom_spiral(data = NULL))
})

test_that("geom_spiral returns ggplot object", {
  p <- ggplot(mtcars, aes(x = mpg)) + geom_spiral()
  expect_is(p, "ggplot")
})

test_that("geom_spiral produces correct number of facets", {
  p <- ggplot(mtcars, aes(x = mpg)) + geom_spiral() + facet_wrap(~cyl)
  expect_equal(length(p$data$cyl), 3)
})
