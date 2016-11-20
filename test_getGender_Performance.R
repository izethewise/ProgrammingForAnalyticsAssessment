#install.packages("testthat")
library(testthat)

n <- c(
"KIMBALL, Mrs Gertrude",                                        
"DYKER, Mr Adolf Fredrik",                                      
"SCOVELL, Mr Robert",                                           
"DEEBLE, Mr Alfred Arnold",                                     
"BUTTERWORTH, Mr John",                                         
"KANTOR, Mr Sinai",                                             
"PORTEUS, Mr Thomas",                                           
"AVERY, Mr James Frank",                                        
"TRIGGS, Mr Robert",                                            
"CECIL, Mr C.",                                                 
"STROM, Miss Telma Matilda",                                    
"SMITH, Mr Charles Edwin",                                      
"TAYLOR, Mr Percy Cornelius",                                   
"CARTER, Mrs Lucile",                                           
"MCNAMEE, Mr Neal",                                             
"ASSAM, Mr Ali",                                                
"HARRIS, Mr Edward",                                            
"RICHARDS, Mrs Emily",                                          
"HARTLEY, Mr Wallace Henry",                                    
"PARKES, Mr Francis 'Frank'"
)

n <- rep(n,5000)
b <- Sys.time()
l <- length(getGender(n))
a <- Sys.time()
msg <- a - b

test_that("getGender using basic.honorifics processes 100000 rows < minute",{
  expect_true(b + 60 > a)
  expect_equal(l, 100000)
  print(msg)
})

b <- Sys.time()
l <- length(getGender(n, honorifics = enhanced.honorifics()))
a <- Sys.time()
msg <- a - b

test_that("getGender using enhanced.honorifics processes 100000 rows < minute",{
  expect_true(b + 60 > a)
  expect_equal(l, 100000)
  print(msg)
})


