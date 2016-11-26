#install.packages("testthat")
library(testthat)

#create matrix of honorifics to pass to function
hons <- c("Mr","Male",  
          "Master","Male", 
          "Miss","Female",  
          "Ms","Female",
          "Mrs","Female" , 
          "Dr","Neutral"  , 
          "Prof","Neutral",
          "Sig","Male",
          "Mme","Female")
hons <- (matrix(hons,length(hons)/2,2,byrow = T)) 

test_that("getGender using passed in honorifics case insensitve single match",{
  expect_equal(getGender("smith, mr john", honorifics = hons), "Male")
  expect_equal(getGender("smith, master john", honorifics = hons), "Male")
  expect_equal(getGender("smith, mrs joan", honorifics = hons), "Female")
  expect_equal(getGender("smith, miss joan", honorifics = hons), "Female")
  expect_equal(getGender("smith, ms joan", honorifics = hons), "Female")
  expect_equal(getGender("smith, prof joan", honorifics = hons), "Neutral")
  expect_equal(getGender("smith, dr joan", honorifics = hons), "Neutral")
})

test_that("getGender using passed in honorifics case insensitve muti match",{
  expect_equal(getGender(c("smith, mr john","smith, mrs joan","smith, prof joan"), honorifics = hons), c("Male","Female","Neutral"))
})

test_that("getGender using passed in honorifics case sensitve single match returns default",{
  expect_equal(getGender("smith, mr j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, master j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, mrs j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, miss j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, ms j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, prof j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
  expect_equal(getGender("smith, dr j.", honorifics = hons,ignore.case = FALSE, default = "X"), "X")
})

test_that("getGender using passed in honorifics case sensitve single match returns match",{
  expect_equal(getGender("smith, Mr john", honorifics = hons,ignore.case = FALSE, default = "X"), "Male")
  expect_equal(getGender("smith, Master john", honorifics = hons,ignore.case = FALSE, default = "X"), "Male")
  expect_equal(getGender("smith, Mrs joan", honorifics = hons,ignore.case = FALSE, default = "X"), "Female")
  expect_equal(getGender("smith, Miss joan", honorifics = hons,ignore.case = FALSE, default = "X"), "Female")
  expect_equal(getGender("smith, Ms joan", honorifics = hons,ignore.case = FALSE, default = "X"), "Female")
  expect_equal(getGender("smith, Prof joan", honorifics = hons,ignore.case = FALSE, default = "X"), "Neutral")
  expect_equal(getGender("smith, Dr joan", honorifics = hons,ignore.case = FALSE, default = "X"), "Neutral")
})

test_that("getGender using passed in honorifics case sensitve muti match returns default",{
  expect_equal(getGender(c("smith, mr j.","smith, mrs j.","smith, prof j."), honorifics = hons,ignore.case = FALSE, default = "X"), c("X","X","X"))
})

test_that("getGender using passed in honorifics case sensitve muti match returns mixed",{
  expect_equal(getGender(c("smith, Mr j.","smith, mrs j.","smith, Prof j."), honorifics = hons,ignore.case = FALSE, default = "X"), c("Male","X","Neutral"))
})

test_that("getGender using passed in honorifics case sensitve muti match returns error where two matches",{
  expect_equal(getGender(c("smith, Mr john","smith, Mrs joan Mr","smith, Prof joan"), honorifics = hons,ignore.case = FALSE, default = "X"), c("Male","Err 1","Neutral"))
})
