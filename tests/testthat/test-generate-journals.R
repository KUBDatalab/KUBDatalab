context("test-generate-journals")



test_that("generate_journals error given if larger than maximum value are supplied", {
  expect_error(generate_journals(nrow(journal_patterns)*nrow(sciences)*nrow(adjectives) + 1))
  expect_error(generate_journals(n = nrow(journal_patterns)*nrow(sciences)*nrow(adjectives) + 1))
})

test_that("generate_journals returns unique results",{
  expect_equal(nrow(distinct(generate_journals(10000, subjects = F))), 10000)
  expect_equal(nrow(distinct(generate_journals(10000, subjects = T))), 10000)
})

test_that("generate_journals returns tibble", {
  expect_type(generate_journals(10), "list")
  expect_s3_class(generate_journals(10), "data.frame")
  expect_s3_class(generate_journals(10), "tbl")
})

test_that("generate_journals returns correct dimensions", {
  expect_equal(dim(generate_journals(47)), c(47,2))
  expect_equal(dim(generate_journals(n = 10)), c(10,2))
  expect_equal(dim(generate_journals(n = 10, subjects = T)), c(10,2))
  expect_equal(dim(generate_journals(n = 10, subjects = F)), c(10,1))
  expect_equal(dim(generate_journals(10, subjects = T)), c(10,2))
  expect_equal(dim(generate_journals(10, subjects = F)), c(10,1))
  expect_equal(dim(generate_journals(10, T)), c(10,2))
  expect_equal(dim(generate_journals(10, F)), c(10,1))
})

