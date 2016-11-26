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

test_that("getGender using default.honorifics case sensitve single match returns default",{
  expect_equal(getGender("smith, mr j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, master j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, mrs j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, miss j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, ms j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, prof j.",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, dr j.",ignore.case = FALSE), "U")
})

test_that("getGender using default.honorifics case sensitve single match returns match",{
  expect_equal(getGender("smith, Mr john",ignore.case = FALSE), "male")
  expect_equal(getGender("smith, Master john",ignore.case = FALSE), "male")
  expect_equal(getGender("smith, Mrs joan",ignore.case = FALSE), "female")
  expect_equal(getGender("smith, Miss joan",ignore.case = FALSE), "female")
  expect_equal(getGender("smith, Ms joan",ignore.case = FALSE), "female")
  expect_equal(getGender("smith, Prof joan",ignore.case = FALSE), "female")
  expect_equal(getGender("smith, Dr joan",ignore.case = FALSE), "female")
})

test_that("getGender using default.honorifics case sensitve muti match returns default",{
  expect_equal(getGender(c("smith, mr j.","smith, mrs j.","smith, prof j."),ignore.case = FALSE), c("U","U","U"))
})

test_that("getGender using default.honorifics case sensitve muti match returns mixed",{
  expect_equal(getGender(c("smith, Mr j.","smith, mrs j.","smith, Prof j."),ignore.case = FALSE), c("male","U","U"))
})

test_that("getGender using default.honorifics case sensitve muti match returns error where two matches",{
  expect_equal(getGender(c("smith, Mr john","smith, Mrs joan Mr","smith, Prof joan"),ignore.case = FALSE), c("M","Err 1","N"))
})

test_that("getGender using default.honorifics case sensitve returns default when passed empty string",{
  expect_equal(getGender("",ignore.case = FALSE), "U")
})

#test_that("getGender returns error when two honorifics and two commas",{
#  expect_equal(getGender("master, mrs, joan"), "Err 2")
#})

#test_that("getGender returns error when one honorifics and two commas",{
#  expect_equal(getGender("smith, mrs, joan"), "F")
#})
