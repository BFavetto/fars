library(fars)

test_that("File name",{
  filename <- make_filename(2013)
  expect_equal(filename,"accident_2013.csv.bz2")
})

test_that("File name",{
  filename <- make_filename(2014)
  expect_equal(filename,"accident_2014.csv.bz2")
})

test_that("File name",{
  filename <- make_filename(2015)
  expect_equal(filename,"accident_2015.csv.bz2")
})
