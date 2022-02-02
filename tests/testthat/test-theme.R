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

test_that("ucph_pal raises warning with large number, x > 10", {
  expect_warning(ucph_pal()(11))
  expect_warning(ucph_pal(reverse = TRUE)(11))
  expect_error(ucph_pal(n = 11))
})

test_that("scale_colour_ucph equals scale_color_ucph", {
  expect_eqNe(scale_color_ucph(), scale_colour_ucph())
})

test_that("scale_colour_ucph works", {
  expect_is(scale_color_ucph(), "ScaleDiscrete")
})

test_that("scale_fill_ucph works", {
  expect_is(scale_fill_ucph(), "ScaleDiscrete")
})

## Colors are correct ----
## dark mellem lys
test_that("ucph palette dark outputs correct colours", {
  expect_equal(ucph_pal("dark")(5),
               c(      "#901a1E", # Rød
                       "#122947", # Blå
                       "#0a5963", # Petroleum
                       "#39641c", # Grøn
                       "#3d3d3d"  # Grå
               ))
})

test_that("ucph palette mellem outputs correct colours",{
  expect_equal(ucph_pal("mellem")(6),
               c(
                 "#c73028", # Rød
                 "#425570", # Blå
                 "#197f8e", # Petroleum
                 "#4b8325", # Grøn
                 "#666666", # Grå
                 "#fefaf2"  # Champagne
               ))
})

test_that("ucph palette lys outputs correct colours",{
  expect_equal(ucph_pal("lys")(6),
               c(
                 "#dB3B0A", # Rød
                 "#bac7d9", # Blå
                 "#b7d7de", # Petroleum
                 "#becaa8", # Grøn
                 "#e1dfdf", # Grå
                 "#ffbd38"  # Gul
               ))
})

test_that("ucph_pal warnings given if maximum values are supplied", {
  expect_warning(ucph_pal("lys")(7))
  expect_warning(ucph_pal("dark")(6))
  expect_warning(ucph_pal("mellem")(7))
})

