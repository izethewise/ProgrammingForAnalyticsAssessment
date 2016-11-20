#install.packages("testthat")
library(testthat)



test_that("getGender using basic.honorifics case insensitve single match",{
  expect_equal(getGender("smith, mr john"), "M")
  expect_equal(getGender("smith, master john"), "M")
  expect_equal(getGender("smith, mrs joan"), "F")
  expect_equal(getGender("smith, miss joan"), "F")
  expect_equal(getGender("smith, ms joan"), "F")
  expect_equal(getGender("smith, prof joan"), "N")
  expect_equal(getGender("smith, dr joan"), "N")
  expect_equal(getGender("master, mrs joan"), "F")
})

test_that("getGender using basic.honorifics case insensitve muti match",{
  expect_equal(getGender(c("smith, mr john","smith, mrs joan","smith, prof joan")), c("M","F","N"))
})

test_that("getGender using basic.honorifics case sensitve single match returns default",{
  expect_equal(getGender("smith, mr john",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, master john",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, mrs joan",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, miss joan",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, ms joan",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, prof joan",ignore.case = FALSE), "U")
  expect_equal(getGender("smith, dr joan",ignore.case = FALSE), "U")
})

test_that("getGender using basic.honorifics case sensitve single match returns match",{
  expect_equal(getGender("smith, Mr john",ignore.case = FALSE), "M")
  expect_equal(getGender("smith, Master john",ignore.case = FALSE), "M")
  expect_equal(getGender("smith, Mrs joan",ignore.case = FALSE), "F")
  expect_equal(getGender("smith, Miss joan",ignore.case = FALSE), "F")
  expect_equal(getGender("smith, Ms joan",ignore.case = FALSE), "F")
  expect_equal(getGender("smith, Prof joan",ignore.case = FALSE), "N")
  expect_equal(getGender("smith, Dr joan",ignore.case = FALSE), "N")
})

test_that("getGender using basic.honorifics case sensitve muti match returns default",{
  expect_equal(getGender(c("smith, mr john","smith, mrs joan","smith, prof joan"),ignore.case = FALSE), c("U","U","U"))
})

test_that("getGender using basic.honorifics case sensitve muti match returns mixed",{
  expect_equal(getGender(c("smith, Mr john","smith, mrs joan","smith, Prof joan"),ignore.case = FALSE), c("M","U","N"))
})

test_that("getGender using basic.honorifics case sensitve muti match returns error where two matches",{
  expect_equal(getGender(c("smith, Mr john","smith, Mrs joan Mr","smith, Prof joan"),ignore.case = FALSE), c("M","Err 1","N"))
})

test_that("getGender using basic.honorifics case sensitve returns default when passed empty string",{
  expect_equal(getGender("",ignore.case = FALSE), "U")
})

test_that("getGender returns error when two honorifics and two commas",{
  expect_equal(getGender("master, mrs, joan"), "Err 2")
})

test_that("getGender returns error when one honorifics and two commas",{
  expect_equal(getGender("smith, mrs, joan"), "F")
})
