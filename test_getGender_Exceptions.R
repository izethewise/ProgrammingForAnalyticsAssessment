#install.packages("testthat")
library(testthat)

#create matrices of honorifics to pass to function
#honorifics for happy path
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

#honorifics to raise duplicates error
dups <- c("Mr","Male",  
          "Master","Male", 
          "Master","Male", 
          "Miss","Female",  
          "Ms","Female",
          "Mrs","Female" , 
          "Dr","Neutral"  ,  
          "Prof","Neutral",
          "Sig","Male",
          "Mme","Female")
dups <- (matrix(dups,length(dups)/2,2,byrow = T)) 


#honorifics to raise empty error
empt <- c("Mr","Male",  
          "Master","Male", 
          "Miss","Female",  
          "Ms","Female",
          "Mrs","" , 
          "Dr","Neutral"  ,  
          "Prof","Neutral",
          "Sig","Male",
          "Mme","Female")
empt <- (matrix(empt,length(empt)/2,2,byrow = T)) 


test_that("getGender raises error argument of incorrect type",{
  expect_error(getGender(c(1,2,3)), "Error in getGender: 'name' argument must be character or factor.")
  expect_error(getGender("Smith, Mrs J",honorifics = as.factor(hons)),"Error in getGender: 'honorifics' argument must coerce to character matrix nrow > 0; ncol == 2.")
  expect_error(getGender("Smith, Mrs J",honorifics = matrix("X",0,2)),"Error in getGender: 'honorifics' argument must coerce to character matrix nrow > 0; ncol == 2.")
  expect_error(getGender("Smith, Mrs J",honorifics = hons, default = 1),"Error in getGender: 'default' argument must be character.")
  expect_error(getGender("Smith, Mrs J",honorifics = hons, firstname.pos = 2.7),"Error in getGender: 'firstname.pos' argument must be integer.")
})

test_that("getGender raises error duplicated honorific",{
  expect_error(getGender("Smith, Mrs J",honorifics = dups),"Error in getGender: honorifics must be unique.")
})

test_that("getGender raises error honorific contains empty strings",{
  expect_error(getGender("Smith, Mrs J",honorifics = empt),"Error in getGender: honorifics must not contain empty strings.")
})

test_that("getGender raises error honorific contains zero rows",{
  expect_error(getGender("Smith, Mrs J",honorifics = empt),"Error in getGender: honorifics must not contain empty strings.")
})
