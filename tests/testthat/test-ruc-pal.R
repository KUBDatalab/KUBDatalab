context("test-ruc-pal")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)



test_that("scale_colour_ruc equals scale_color_ruc", {
  expect_eqNe(scale_color_ruc(), scale_colour_ruc())
})

test_that("scale_colour_ruc works", {
  expect_is(scale_color_ruc(), "ScaleDiscrete")
})

test_that("scale_fill_ruc works", {
  expect_is(scale_fill_ruc(), "ScaleDiscrete")
})

## Colors are correct ----
test_that("ruc palette outputs correct colours", {
  expect_equal(ruc_pal("lys")(5),
               c(
                 "#B7C9D3", # Lys grå - pantone 5445C
                 "#71b790", # Lys grøn - pantone 2248C
                 "#59cbe8", # Lys blå - Pantone 305C
                 "#ef95cf", # Lys rød - Pantone 223C
                 "#f6eb61"  # Lys gul - Pantone 100C
               ))
  expect_equal(ruc_pal("dark")(5),
               c(
                 "#425563", # Mørk grå - Pantone 7545C
                 "#00685e", # Støvet grøn - Pantone 329C
                 "#10069f", # Mørk blå - Pantone 072C
                 "#f65275", # Orange rød - pantone 184c
                 "#f4da40"  # Varm gul - pantone 7404C
               ))
})




test_that("ruc_pal warnings given if larger than maximum values are supplied", {
  expect_warning(ruc_pal()(6))
  expect_warning(ruc_pal("lys")(6))
  expect_warning(ruc_pal("dark")(6))

  expect_warning(ruc_pal(palette = "lys", reverse = TRUE)(6))
  expect_warning(ruc_pal(palette = "dark", reverse = TRUE)(6))

  expect_warning(ruc_pal(palette = "lys")(n = 6))
  expect_warning(ruc_pal(palette = "dark")(n = 6))

})
