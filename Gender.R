getGender <- function(name, honorifics = default.honorifics, ignore.case = TRUE, default = "U")
{
  # Matches honorific (mr, mrs, miss...) within name to corresponding gender code.
  #
  # Args:
  #   name: Character or factor variable of name being checked for gender.
  #   honorifics: Character matrix containing honorific and corresponding gender code.
  #     Matrix format: E.g.
  #     Mr     male
  #     Master male
  #     Mrs    female
  #     Miss   female
  #     etc.
  #     Default: see basic.honorifics function.
  #   ignore.case: If TRUE, mr or Mr will both return a match for Mr.
  #                If FALSE, mr will not return a match for Mr.
  #   default: If name cannot be matched, default is returned.
  #   
  # Returns:
  #   Where name can be matched to an entry in honorifics[,1] returns corresponding entry honorifics[,2]
  #     returns corresponding entry honorifics[,2]
  #     else returns default.
  
  # Convert input to correct data type and use default error handling.
  honorifics <- as.matrix(honorifics)
  
  # Section below validates input and halts exection if arguments not of correct type.
  if (!is.character(name) && !is.factor(name)) {
    stop("Error in getGender: 'name' argument must be character or factor.")
  }
  if (!is.character(honorifics) || !is.matrix(honorifics) || !ncol(honorifics) == 2 || !nrow(honorifics) > 0) {
    stop("Error in getGender: 'honorifics' argument must coerce to character matrix nrow > 0; ncol == 2.")
  }
  if (!is.logical(ignore.case)) {
    stop("Error in getGender: 'ignore.case' argument must be logical.")
  }
  if (!is.character(default)) {
    stop("Error in getGender: 'default' argument must be character.")
  }
  # Raise an error if honorifics are duplicated:
  #   duplication could create ambiguity if one honorific mapped to more than one gender code.
  a <- honorifics[,1]
  if (!length(a[duplicated(a)])==0) {
    stop("Error in getGender: honorifics must be unique.")
  }
  # Raise an error if honorifics contain empty strings.
  a <- trimws(honorifics)
  if (!length(a[a==""])==0) {
    stop("Error in getGender: honorifics must not contain empty strings.")
  }
  # End of validation section.
  
  # Convert matrix of honorifics and genders into matrix of genders and regular expressions.
  regex = transformTable(honorifics)
  
  # Apply matchName function to name argument and return result.
  return (
    as.character(
      sapply(name, function(x) matchName(x, regex, ignore.case, default)
      )
    )
  )
}


#  Default list of honorifics.
m <- c("Mr",      "male",
       "Master",  "male",
       "Miss",    "female",
       "Ms",      "female",
       "Mrs",     "female",
       "Sig.",     "male",
       "Mme",     "female",
       "Rev.",     "male",
       "Mlle",    "female",
       "Dona",    "female",
       "Sir",     "male",
       "Fr",      "male",
       "Don",     "male",
       "Countess","female",
       "Lady",    "female")
default.honorifics <- matrix(m,length(m)/2,2,byrow = T)


matchName <- function(name, regex.matrix, ignore.case = TRUE, default)
{
  # Matches name to gender code using regular expressions.
  #
  # Args:
  #   name: Character/factor variable of name being checked for gender.
  #   regex.matrix: Two column matrix: 
  #     E.g.
  #     male    (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #     female  (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  #   ignore.case: See getGender.
  #   default: See getGender.
  #
  # Returns:
  #   Where name can be matched to regex.matrix[,2] 
  #     returns corresponding entry regex.matrix[,1]
  #     else returns default.
  
  # Set return value to default argument.
  ret <- default
  # Loop through all values in matrix.
  for (i in 1:nrow(regex.matrix)) {
    # If name matches regular expression...
    if (grepl(regex.matrix[i, 2], name, ignore.case)) {
        ret <- regex.matrix[i, 1]
    }
  }
  if (ret == default) {
    ret <- matchFirstname(name, default)
  }
  # When finished iterating loop, return return variable.
  return (as.character(ret))
}

matchFirstname <- function(name, default) {
  fname <- strsplit(gsub("[^[:alnum:] ]", "", name), " +")[[1]][3]
  ret <- as.character(gender(fname))[4]
  if (!as.character(ret) == "logical(0)") {
    return (ret)
  }
  return (default)
}

transformTable <- function(honorifics) 
{
  # Transforms two column matrix of honorifics and gender code
  #   into two column matrix of gender code and regular expression of honorifics:
  #
  # Args:
  #   honorifics: 
  #     E.g.
  #     Mr      male
  #     Master  male
  #     Mrs     female
  #     Miss    female
  #
  # Returns:
  #   E.g.
  #   male    (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #   female  (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  
  # Set local variable. 
  h <- honorifics
  # Get vector of unique gender codes.
  u <- as.character(unique(h[, 2]))
  # Create 2 column matrix with a row for each gender code.
  ret <- matrix(u, nrow=length(u), ncol=2)
  # Create list of honorifics for each gender code.
  x <- sapply(ret[, 2], function(x) h[h[, 2] == as.character(x), 1])
  # Convert list to regular expressions of honorifics.
  x <- sapply(x, function(x) mkExp(x))
  # Set column 2 of return matrix to regular expressions of honorifics.
  ret[,2] <- x
  return (ret)
}

mkExp <- function(x, pref = "(^|[^A-Za-z])", suff = "($|[^A-Za-z])")
{
  # Converts character vector to regular expression string.
  #
  # Args:
  #   x: Character vector.
  #   pref: Prefix for regex.
  #   suff: Suffix for regex.
  #
  # Returns:
  #   E.g.
  #   (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  
  # Create '|' delimited string from character vector 
  #   and bookend with prefix and suffix.
  ret <- paste0(pref,
               "(",
               paste(x, collapse = "|"),
               ")",
               suff)
  return (ret)
}
