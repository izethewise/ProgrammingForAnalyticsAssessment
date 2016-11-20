getGender <- function(name, honorifics = basic.honorifics(), ignore.case = TRUE, default = "U")
{
  # Matches honorific (mr, mrs, miss...) within name to corresponding gender code.
  #
  # Args:
  #   name: Character or factor variable of name being checked for gender.
  #   honorifics: Character matrix containing honorific and corresponding gender code.
  #     Matrix format: E.g.
  #     Mr     M
  #     Master M
  #     Mrs    F
  #     Miss   F
  #     etc.
  #     Default: see basic.honorifics function.
  #   ignore.case: If TRUE, mr or Mr will both return a match for Mr.
  #                If FALSE, mr will not return a match for Mr.
  #   default: If name cannot be matched, default is returned.
  #   
  # Returns:
  #   Where name can be matched to an entry in honorifics[,1] returns corresponding entry honorifics[,2] 
  #   else returns default.
  
  # Section below validates input and halts exection if arguments not of correct type
  if (!is.character(name) && !is.factor(name)) {
    stop("Error in getGender: 'name' argument must be character or factor.")
  }
  if (!is.character(honorifics)) {
    stop("Error in getGender: 'honorifics' argument must be character.")
  }
  if (!is.logical(ignore.case )) {
    stop("Error in getGender: 'ignore.case' argument must be logical.")
  }
  if (!is.character(default)) {
    stop("Error in getGender: 'default' argument must be character.")
  }
  # Raise an error if honorifics are duplicated:
  #   duplication could create ambiguity.
  a <- honorifics[,1]
  if (!length(a[duplicated(a)])==0) {
    stop("Error in getGender: honorifics must be unique.")
  }
  # Raise an error if honorifics contain empty strings.
  a <- trimws(honorifics)
  if (!length(a[a==""])==0) {
    stop("Error in getGender: honorifics must not contain empty strings.")
  }
  
  # Convert matrix of honorifics and genders 
  #   into matrix of genders and regular expressions
  regex.table = transformTable(honorifics)
  
  # Apply matchName function to name argument and return result.
  return (
    as.character(
      sapply(
        name, 
        function(x) matchName(x, regex.table, ignore.case, default)
      )
    )
  )
}


basic.honorifics <- function()
{
 # Returns:
 #  Basic honorific/gender code matrix for use in main getGender function
  
  m <- c("Mr",      "M",  
         "Master",  "M", 
         "Miss",    "F",  
         "Ms",      "F",
         "Mrs",     "F", 
         "Dr",      "N", 
         "Prof",    "N"
         )
  m <- (matrix(m,length(m)/2,2,byrow = T)) 
  return (m)
}

enhanced.honorifics <- function()
{
  # Returns:
  #  More extensive honorific/gender code matrix for use in main getGender function
  
  m <- c("Mr",      "M",  
         "Master",  "M", 
         "Miss",    "F",  
         "Ms",      "F",
         "Mrs",     "F", 
         "Dr",      "N", 
         "Prof",    "N",
         "Sig",     "M",
         "Colonel", "M",
         "Major",   "M",
         "Captain", "M",
         "Mme",     "F",
         "Rev",     "M",
         "Mlle",    "F",
         "Dona",    "F",
         "Sir",     "M",
         "Fr",      "M",
         "Don",     "M",
         "Countess","F",
         "Lady",    "F")
  m <- (matrix(m,length(m)/2,2,byrow = T)) 
  return (m)
}

matchName <- function(name, regex.table, ignore.case = TRUE, default)
{
  # Matches name to gender code using regular expressions.
  #
  # Args:
  #   name: Character/factor variable of name being checked for gender.
  #   regex.table: Two column matrix: 
  #     E.g.
  #     M   (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #     F   (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  #   ignore.case: See getGender.
  #   default: See getGender.
  #
  # Returns:
  #   Where name can be matched to regex.table[,2] 
  #     returns corresponding entry regex.table[,1]
  #     else returns default.
  
  # Set return value to default argument.
  return.value <- default
  # Initialise logical variable that will indicate if match has been found.
  matched <- FALSE
  # Loop through all values in table.
  for (i in 1:nrow(regex.table)) {
    # If name matches regular expression...
    if (grepl(regex.table[i, 2], name, ignore.case)) {
      # If matched == TRUE at this point, match has already been found.
      if (matched) {
        # If more than one honorific is matched, 
        # return an error.
        return ("Err 1")
        # This is the happy path.
        #   Set return variable to matched gender code and set matched variable to TRUE.
      } else {
        return.value <- regex.table[i, 1]
        matched <- TRUE
      }
    }
  }
  # When finished iterating loop, return return variable.
  return (as.character(return.value))
}

transformTable <- function(honorifics) 
{
  # Transforms two column matrix of honorifics and gender code
  #   into two colum matrix of gender code and reg ex of honorific:
  #
  # Args:
  #   honorifics: 
  #     E.g.
  #     Mr      M
  #     Master  M
  #     Mrs     F
  #     Miss    F
  #
  # Returns:
  #   E.g.
  #   M   (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #   F   (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  
  # Set local variable. 
  h <- honorifics
  # Get array of unique honorifics.
  u <- as.character(unique(h[, 2]))
  # Create 2 column matrix with a row for each honorific.
  m <- matrix(u, nrow=length(u), ncol=2)
  # Create lists of honorifics for each gender code.
  x <- sapply(m[, 2], function(x)
    h[h[, 2] == as.character(x), 1])
  # Convert lists of factors to lists of character vectors.
  x <- sapply(x, function(x)
    as.character(x))
  # Convert character vectors to regular expression.
  x <- sapply(x, function(x)
    mkExp(x))
  # Set column 2 of return matrix to vector of regular expressions.
  m[,2] <- x
  return (m)
}

mkExp <- function(x, pref = "(^|[^A-Za-z])", suff = "($|[^A-Za-z])")
{
  # Converts character vector to regex string
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
  ret <- paste(pref,
               "(",
               paste(x, collapse = "|"),
               ")",
               suff,
               sep = "")
  return (ret)
}
