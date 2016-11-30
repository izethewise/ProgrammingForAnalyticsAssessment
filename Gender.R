getGender <- function(name, honorifics = default.honorifics, 
                      firstname.pos = 2, default = "u")
{
  # Matches honorific (mr, mrs, miss...) within name to corresponding gender.
  #   Where gender cannot be matched, uses first name matching in gender package.
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
  #   firstname.pos: Position after comma, of block of alpha characters defining 
  #     first name within name.
  #   default: If name cannot be matched, default is returned.
  #   
  # Returns:
  #   Gender identified from name.
  
  # Check if gender package installed and load library otherwise warn.
  if("gender" %in% rownames(installed.packages()) == FALSE) {
    msg <- "Warning: getGender function uses 'gender' package."
    msg <- paste(msg, "Please install for improved matching")
    warning(msg)
    use.firstname <- FALSE
  } else {
    library(gender)
    use.firstname <- TRUE
  }
  
  # Convert input to correct data type.
  honorifics <- as.matrix(honorifics)
  
  # Section below validates input and halts exection if arguments not of correct type.
  if (!is.character(name) && !is.factor(name)) {
    stop("Error in getGender: 'name' argument must be character or factor.")
  }
  if (!is.character(honorifics) || !is.matrix(honorifics) || 
      !ncol(honorifics) == 2 || !nrow(honorifics) > 0) {
    msg <- "Error in getGender: 'honorifics' argument must coerce to character"
    msg <- paste(msg, "matrix nrow > 0; ncol == 2.")
    stop(msg)
  }
  if (!is.numeric(firstname.pos) || !firstname.pos == floor(firstname.pos)) {
    stop("Error in getGender: 'firstname.pos' argument must be integer.")
  }
  if (!is.character(default)) {
    stop("Error in getGender: 'default' argument must be character.")
  }
  # Raise an error if honorifics are duplicated:
  #   duplication could create ambiguity if one honorific 
  #   mapped to more than one gender.
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
  
  # Convert matrix of honorifics and genders into 
  #   matrix of genders and regular expressions.
  regex = transformTable(honorifics)
  
  # Apply matchName function to name argument and return result.
  return (
    as.character(
      sapply(name, function(x) matchName(x, regex, firstname.pos, 
                                         use.firstname, default)
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
       "Sig",    "male",
       "Mme",     "female",
       "Rev",    "male",
       "Mlle",    "female",
       "Dona",    "female",
       "Sir",     "male",
       "Fr",      "male",
       "Don",     "male",
       "Countess","female",
       "Lady",    "female")
default.honorifics <- matrix(m,length(m)/2,2,byrow = T)


matchName <- function(name, regex.matrix, firstname.pos, use.firstname, default)
{
  # Matches name to gender using regular expressions.
  #
  # Args:
  #   name: Character/factor variable of name being checked for gender.
  #   regex.matrix: Two column matrix: 
  #     E.g.
  #     male    (^|[^A-Za-z])(Mr|Master)($|[^A-Za-z])
  #     female  (^|[^A-Za-z])(Mrs|Miss)($|[^A-Za-z])
  #   firstname.pos: See getGender.
  #   use.firstname: If false, do not attempt firstname processing.
  #   default: See getGender.
  #
  # Returns:
  #   Where name can be matched to regex.matrix[,2] 
  #     returns corresponding entry regex.matrix[,1]
  #     else returns default.

  # Set return value to default argument.
  ret <- default
  # Get position of comma in name.
  pos <- regexpr(',', name)
  # Set local variable to characters to right of comma.
  tmpname <- trimws(substr(name, pos+1, nchar(as.character(name))))
  # Loop through all values in honorifics matrix.               
  for (i in 1:nrow(regex.matrix)) {
    # Check whether remainder of name contains honorific.
    if (grepl(regex.matrix[i, 2], tmpname, ignore.case = TRUE)) {
      # If honorific found, set return value to corresponding gender.
      ret <- regex.matrix[i, 1]
      # Don't bother with rest of loop if we've already found match.
      break
    }
  }
  
  
  # If we've not matched on honorific and gender package installed,
  #   attemp to match on first name.
  if (ret == default && use.firstname) {
    ret <- matchFirstname(tmpname, firstname.pos, default)
  }
  # When finished iterating loop, return return variable.
  return (as.character(ret))
}

matchFirstname <- function(name, firstname.pos, default) 
{
  # Calls gender method of gender package to return gender from first name.
  #
  # Args:
  #   firstname.pos: See getGender.
  #   default: See getGender.
  
  # Extract first name from name by splitting name 
  #   into chunks of alpha characters and taking that
  #   which corresponds to defined first name position.
  fname <- strsplit(gsub("[^A-Za-z -]", "", name), " +")[[1]][firstname.pos]
  # Call the gender method.
  ret <- as.character(gender(fname))[4]
  # If gender match return default,
  #   otherwise return gender.
  if (!as.character(ret) == "logical(0)") {
    return (ret)
  }
  return (default)
}

transformTable <- function(honorifics) 
{
  # Transforms two column matrix of honorifics and gender
  #   into two column matrix of gender and regular expression of honorifics:
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
  # Get vector of genders.
  u <- as.character(unique(h[, 2]))
  # Create 2 column matrix with a row for each gender.
  ret <- matrix(u, nrow=length(u), ncol=2)
  # Create list of honorifics for each gender.
  for(i in 1:nrow(ret)){
    x <- h[h[, 2] == ret[i,1], 1]
    # Transform list into regular expression.
    x <- mkExp(x)
    ret[i,2] <- x
  }
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