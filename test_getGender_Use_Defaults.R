#install.packages("testthat")
library(testthat)



test_that("getGender using default.honorifics case insensitve single match",{
  expect_equal(getGender("smith, mr john"), "male")
  expect_equal(getGender("smith, master john"), "male")
  expect_equal(getGender("smith, mrs joan"), "female")
  expect_equal(getGender("smith, miss joan"), "female")
  expect_equal(getGender("smith, ms joan"), "female")
  expect_equal(getGender("smith, prof joan"), "female")
  expect_equal(getGender("smith, dr joan"), "female")
  expect_equal(getGender("master, mrs joan"), "female")
})

test_that("getGender using default.honorifics case insensitve muti match",{
  expect_equal(getGender(c("smith, mr john","smith, mrs joan","smith, prof joan")), c("male","female","female"))
})

#test_that("getGender returns error when two honorifics and two commas",{
#  expect_equal(getGender("master, mrs, joan"), "Err 2")
#})

#test_that("getGender returns error when one honorifics and two commas",{
#  expect_equal(getGender("smith, mrs, joan"), "F")
#})
