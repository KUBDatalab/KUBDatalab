context("test-theme")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)

test_that("theme_kubdatalab works", {
  thm <- theme_kubdatalab()
  expect_s3_class(thm, "theme")
  ## font
  expect_equal(thm$text$family, "")
  expect_null(thm$plot.title$family)
  expect_null(thm$legend.title$family)
  expect_null(thm$legend.text$family)
  ## size
  expect_equal(thm$text$size, 14)
  expect_equal(thm$plot.title$size, 18)
  expect_equal(thm$plot.subtitle$size, 12)
  expect_equal(thm$axis.text$size, 10)
  expect_equal(thm$axis.title$size, 14)
  expect_equal(thm$legend.text$size, 9)
  expect_equal(thm$legend.title$size, 10)
  ## color
  expect_equal(thm$text$colour, "#fee8c8")
  expect_equal(thm$plot.title$colour, "#FFD235")
  expect_equal(thm$plot.subtitle$colour, "#fee8c8")
  expect_equal(thm$axis.text$colour, "#fee8c8")
  expect_equal(thm$axis.title$colour, "#fee8c8")
  expect_equal(thm$legend.text$colour, "#ffffff")
  expect_equal(thm$legend.title$colour, "#ffffff")
  expect_equal(thm$legend.position, "bottom")

  ## ticks == TRUE
  thm <- theme_kubdatalab(ticks = TRUE)
  expect_s3_class(thm, "theme")
  ## font
  expect_equal(thm$text$family, "")
  expect_null(thm$plot.title$family)
  expect_null(thm$legend.title$family)
  expect_null(thm$legend.text$family)
  ## size
  expect_equal(thm$text$size, 14)
  expect_equal(thm$plot.title$size, 18)
  expect_equal(thm$plot.subtitle$size, 12)
  expect_equal(thm$axis.text$size, 10)
  expect_equal(thm$axis.title$size, 14)
  expect_equal(thm$legend.text$size, 9)
  expect_equal(thm$legend.title$size, 10)
  ## ticks
  expect_equal(thm$axis.ticks$size, 0.15)
  ## color
  expect_equal(thm$text$colour, "#fee8c8")
  expect_equal(thm$plot.title$colour, "#FFD235")
  expect_equal(thm$plot.subtitle$colour, "#fee8c8")
  expect_equal(thm$axis.text$colour, "#fee8c8")
  expect_equal(thm$axis.title$colour, "#fee8c8")
  expect_equal(thm$legend.text$colour, "#ffffff")
  expect_equal(thm$legend.title$colour, "#ffffff")
  expect_equal(thm$legend.position, "bottom")
})
