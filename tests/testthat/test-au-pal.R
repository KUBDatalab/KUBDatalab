context("test-au-pal")

expect_eqNe <- function(...) expect_equal(..., check.environment = FALSE)



test_that("scale_colour_au equals scale_color_au", {
  expect_eqNe(scale_color_au(), scale_colour_au())
})

test_that("scale_colour_au works", {
  expect_is(scale_color_au(), "ScaleDiscrete")
})

test_that("scale_fill_au works", {
  expect_is(scale_fill_au(), "ScaleDiscrete")
})

## Colors are correct ----
## dark mellem lys
test_that("au palette dark outputs correct colours", {
  expect_equal(au_pal("sek_klare")(10),
               c(      "#003d73", # Blå - Pantone 287 EC
                       "#655a9f", # Lilla - Pantone 2665 EC
                       "#37a0cb", # Cyan - Pantone Process Cyan EC
                       "#00aba4", # Turkis - Pantone 326 EC
                       "#8bad3f", # Grøn - Pantone 376 EC
                       "#fabb00", # Gul - Pantone 7408 EC
                       "#ee7f00", # Orange - Pantone 144 EC
                       "#e2001a", # Rød - Pantone 485 EC
                       "#e2007a", # Magenta - Pantone Process Magenta EC
                       "#878787"  # Grå - Pantone Cool Gray 7 EC
               ))
})

test_that("ucph palette mellem outputs correct colours",{
  expect_equal(au_pal("sek_dark")(10),
               c(
                 "#002546", # Blå (mørk) - Pantone 289 EC
                 "#281c41", # Lilla (mørk) - Pantone 5265
                 "#003e5c", # Cyan (mørk) - Pantone 3025 EC
                 "#004543", # Turkis (mørk) - Pantone 567 EC
                 "#425821", # Grøn (mørk) - Pantone 574 EC
                 "#634b03", # Gul (mørk) - Pantone 455 EC
                 "#5f3408", # Orange (mørk) - Pantone 463 EC
                 "#5b0c0c", # Rød (mørk) - Pantone 490 EC
                 "#5f0030", # Magenta (mørk) - Pantone 229 EC
                 "#4b4b4a"  # Grå (mørk) - Pantone 11 C
               ))
})



test_that("au_pal warnings given if larger than maximum values are supplied", {
  expect_warning(au_pal("lys")(11))
  expect_warning(au_pal("dark")(11))

  expect_warning(au_pal("lys", reverse = TRUE)(11))
  expect_warning(au_pal("dark", reverse = TRUE)(11))

  expect_warning(au_pal("lys")(n = 11))
  expect_warning(au_pal("dark")(n = 11))

})
