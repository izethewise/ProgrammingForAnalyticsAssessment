


getCategory <- function(myString,
                          regex.table,
                          ignore.case = TRUE,
                          default = "U")
{
  return.value <- default
  matched <- FALSE
  for (i in 1:nrow(regex.table)) {
    if (grepl(regex.table[i, 2], myString, ignore.case)) {
      if (matched) {
        return ("Err 1")
      } else {
        return.value <- regex.table[i, 1]
        matched <- TRUE
      }
    }
  }
  return (as.character(return.value))
}

transformTable <- function(honorific.defs) {
  h <- honorific.defs
  #get array of unique honorifics
  u <- as.character(unique(h[, 2]))
  #create 2 column matrix with a row for each honorific
  m <- matrix(u, nrow=length(u), ncol=2)
  #create lists of honorifics for each gender code
  x <- lapply(m[, 2], function(x)
    h[h[, 2] == as.character(x), 1])
  #convert lists of factors to lists of character arrays
  x <- lapply(x, function(x)
    as.character(x))
  #convert character arrays to regular expression
  x <- lapply(x, function(x)
    mkExp(x))
  #pivot to two column table using melt
  m[,2]<-unlist(x)
  return (m)
}

mkExp <- function(lst,
                  pref = "(^|[^A-Za-z])",
                  suff = "($|[^A-Za-z])")
{
  ret <- paste(pref,
               "(",
               paste(lst, collapse = "|"),
               ")",
               suff,
               sep = "")
  return (ret)
}
