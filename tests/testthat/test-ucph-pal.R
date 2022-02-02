context("test-ucph-pal")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)



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

test_that("ucph_pal warnings given if larger than maximum values are supplied", {
  expect_warning(ucph_pal("lys")(7))
  expect_warning(ucph_pal("dark")(6))
  expect_warning(ucph_pal("mellem")(7))

  expect_warning(ucph_pal("lys", reverse = TRUE)(7))
  expect_warning(ucph_pal("dark", reverse = TRUE)(6))
  expect_warning(ucph_pal("mellem", reverse = TRUE)(7))

  expect_warning(ucph_pal("lys")(n = 7))
  expect_warning(ucph_pal("dark")(n = 6))
  expect_warning(ucph_pal("mellem")(n = 7))
})
