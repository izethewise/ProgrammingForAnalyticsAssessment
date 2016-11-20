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
  #     returns corresponding entry honorifics[,2]
  #     else returns default.
  
  # Section below validates input and halts exection if arguments not of correct type
  if (!is.character(name) && !is.factor(name)) {
    stop("Error in getGender: 'name' argument must be character or factor.")
  }
  if (!is.character(honorifics) || !is.matrix(honorifics) || !ncol(honorifics) == 2 || !nrow(honorifics) > 1) {
    stop("Error in getGender: 'honorifics' argument must be character matrix nrow > 1; ncol == 2.")
  }
  if (!is.logical(ignore.case )) {
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
  
  # Convert matrix of honorifics and genders into matrix of genders and regular expressions
  regex = transformTable(honorifics)
  
  # Apply matchName function to name argument and return result.
  return (
    as.character(
      sapply(name, function(x) matchName(x, regex, ignore.case, default)
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

matchName <- function(name, regex.matrix, ignore.case = TRUE, default)
{
  # Matches name to gender code using regular expressions.
  #
  # Args:
  #   name: Character/factor variable of name being checked for gender.
  #   regex.matrix: Two column matrix: 
  #     E.g.
  #     M   (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #     F   (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  #   ignore.case: See getGender.
  #   default: See getGender.
  #
  # Returns:
  #   Where name can be matched to regex.matrix[,2] 
  #     returns corresponding entry regex.matrix[,1]
  #     else returns default.
  
  # Set return value to default argument.
  ret <- default
  # Initialise logical variable that will indicate if match has been found.
  matched <- FALSE
  # Loop through all values in matrix.
  for (i in 1:nrow(regex.matrix)) {
    # If name matches regular expression...
    if (grepl(regex.matrix[i, 2], name, ignore.case)) {
      # If matched == TRUE at this point, match has already been found.
      if (matched) {
        # If more than one honorific is matched and comma present, 
        #   reprocess string to right of comma.
        #   Examples of such name: Master, Mrs Jane; Don, Mr James.
        # Check if comma exists.
        pos <- regexpr(',', name)
        if (pos > 0) {
          # Take string to right of comma.
          name <- trimws(substr(name, pos+1, nchar(as.character(name))))
          # If another comma exists, this is an unanticipated format so return an error.
          pos <- regexpr(',', name)
          if (pos > 0) {
            return ("Err 2")
          }
          # Recursively process string to right of comma.
          ret <- matchName(name, regex.matrix, ignore.case = TRUE, default)
        } else {
          # return an error.
          return ("Err 1")
        }
        # This is the happy path.
        #   Set return variable to matched gender code and set matched variable to TRUE.
      } else {
        ret <- regex.matrix[i, 1]
        matched <- TRUE
      }
    }
  }
  # When finished iterating loop, return return variable.
  return (as.character(ret))
}

transformTable <- function(honorifics) 
{
  # Transforms two column matrix of honorifics and gender code
  #   into two column matrix of gender code and regular expression of honorifics:
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
  # Converts character vector to regular expression string
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
