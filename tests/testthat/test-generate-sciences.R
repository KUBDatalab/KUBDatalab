context("test-generate-sciences")



test_that("generate_sciences error given if larger than maximum value are supplied", {
  expect_error(generate_sciences(10000000000))
  expect_error(generate_sciences(n = 10000000000))
})

test_that("generate_sciences returns unique results",{
  expect_equal(nrow(distinct(generate_sciences(1000000, subjects = F))), 1000000)
  expect_equal(nrow(distinct(generate_sciences(1000000, subjects = T))), 1000000)
})

test_that("generate_sciences returns tibble", {
  expect_type(generate_sciences(10), "list")
  expect_s3_class(generate_sciences(10), "data.frame")
  expect_s3_class(generate_sciences(10), "tbl")
})

test_that("generate_sciences returns correct dimensions", {
  expect_equal(dim(generate_sciences(47)), c(47,2))
  expect_equal(dim(generate_sciences(n = 10)), c(10,2))
  expect_equal(dim(generate_sciences(n = 10, subjects = T)), c(10,2))
  expect_equal(dim(generate_sciences(n = 10, subjects = F)), c(10,1))
  expect_equal(dim(generate_sciences(10, subjects = T)), c(10,2))
  expect_equal(dim(generate_sciences(10, subjects = F)), c(10,1))
  expect_equal(dim(generate_sciences(10, T)), c(10,2))
  expect_equal(dim(generate_sciences(10, F)), c(10,1))
})

