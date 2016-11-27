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
