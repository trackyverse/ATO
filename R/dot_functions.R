#' Round value to a reasonable number of decimal places
#' 
#' @param value the value to be rounded
#' @param max the maximum number of decimal places allowed
#' 
#' @return the rounded value
#' 
#' @keywords internal
#' 
.dyn_round <- function(value, max = 10) {
  dec <- 2
  while(round(value, dec) == 0 & dec < max) {
    dec <- dec + 1
  }
  output <- round(value, dec)
  if (output == 0) {
    output <- paste0("<0.", paste0(rep(0, max - 1), collapse = ""), "1")
  }
  return(output)
}

#' Check that data.table library is installed
#' 
#' @param error Stop if not installed?
#' 
#' @return TRUE if installed, FALSE if not installed and error = FALSE
#' 
#' @keywords internal
#' 
.data.table_exists <- function(error = TRUE) {
  # data.table's special functions are hard to handle with data.table
  # as a suggested package because this prevents us from using
  # importFrom calls. Instead, this function loads all of data.table
  # if we are about to use it.
  # However, for code clarity and for the sanity of future developers,
  # you should still use data.table:: wherever possible!
  if (!"data.table" %in% utils::installed.packages()) {
    if (error) {
      stop("You must install package data.table to run this function.",
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check that tibble library is installed
#' 
#' @param error Stop if not installed?
#' 
#' @return TRUE if installed, FALSE if not installed and error = FALSE
#' 
#' @keywords internal
#' 
.tibble_exists <- function(error = TRUE) {
  if (!"tibble" %in% utils::installed.packages()) {
    if (error) {
      stop("This function requires package tibble to run.", call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Concatenate vectors with commas
#' 
#' @param x The vector to write out
#' @param max_i maximum number of vector elements to display before
#'  saying "and n others"
#' 
#' @return a string
#' 
#' @keywords internal
#' 
#' tests:
#'   .comma(LETTERS[1])
#'   .comma(LETTERS[1:2])
#'   .comma(LETTERS[1:3])
#'   .comma(LETTERS[1:5])
#'   .comma(LETTERS[1:10])
#'
#'   .comma(LETTERS[1], max_i = 2)
#'   .comma(LETTERS[1:2], max_i = 2)
#'   .comma(LETTERS[1:3], max_i = 2)
#'   .comma(LETTERS[1:5], max_i = 2)
#'   .comma(LETTERS[1:10], max_i = 2)
#'
#'   .comma(LETTERS[1], max_i = 1)
#'   .comma(LETTERS[1:2], max_i = 1)
#'   .comma(LETTERS[1:3], max_i = 1)
#'   .comma(LETTERS[1:5], max_i = 1)
#'   .comma(LETTERS[1:10], max_i = 1)
#'
.comma <- function(x, max_i = 5) {
  if (max_i < 2) {
    if (length(x) == 1) {
      output <- x
    } else {
      output <- paste0(x[1], " and ", length(x)-1,
                      ifelse(length(x)-1 == 1,
                             " other",
                             " others"))
    }
  } else {
    if (length(x) == 1) {
      output <- x
    }
    if (length(x) == 2) {
      output <- paste0(x[1], " and ", x[2])  
    }
    if (length(x) > 2) {
      if (length(x) <= max_i) {
        output <- paste0(paste0(x[-length(x)], collapse = ", "),
                         ", and ", x[length(x)])
      } else {
        output <- paste0(paste0(x[1:max_i], collapse = ", "),
                         " and ", length(x)-max_i,
                         ifelse(length(x)-max_i == 1,
                                " other",
                                " others"))
      }
    }
  }
  return(output)
}

#' Choose between has and have
#' 
#' @param n the number of elements
#' 
#' @return a string
#' 
#' @keywords internal
#' 
.has <- function(n) {
  if (n > 1) {
    return("have")
  } else {
    return("has")
  }
}

#' Choose between was and were
#' 
#' @param n the number of elements
#' 
#' @return a string
#' 
#' @keywords internal
#' 
.was <- function(n) {
  if (n > 1) {
    return("were")
  } else {
    return("was")
  }
}

#' Choose between is and are
#' 
#' @param n the number of elements
#' 
#' @return a string
#' 
#' @keywords internal
#' 
.is <- function(n) {
  if (n > 1) {
    return("are")
  } else {
    return("is")
  }
}

#' Add s
#' 
#' Adds an “s” if n is > 1 and reverse is false (default)
#' Adds “s” if n is == 1 and reverse is true
#' 
#' e.g. detection and detections
#' 
#' @param n the number of elements
#' @param reverse logical, defaults to FALSE (see above)
#' 
#' @return a string
#' 
#' @keywords internal
#' 
.s <- function(n, reverse = FALSE) {
  if (n > 1) {
    if (reverse) {
      return("")
    } else {
      return("s")
    }
  } else {
    if (reverse) {
      return("s")
    } else {
      return("")
    }
  }
}

#' Add es
#' 
#' Adds an “es” if n is > 1 and reverse is false (default)
#' Adds “es” if n is == 1 and reverse is true
#' 
#' e.g. match and matches
#' 
#' @param n the number of elements
#' @param reverse logical, defaults to FALSE (see above)
#' 
#' @return a string
#' 
#' @keywords internal
#' 
.es <- function(n, reverse = FALSE) {
  if (n > 1) {
    if (reverse) {
      return("")
    } else {
      return("es")
    }
  } else {
    if (reverse) {
      return("es")
    } else {
      return("")
    }
  }
}

#' Pick between adding y or ies
#' 
#' e.g. party and parties
#' 
#' @param n the number of elements
#' 
#' @return a string
#' 
#' @keywords internal
#'
.y <- function(n) {
  if (n > 1) {
    return("ies")
  } else {
    return("y")
  }
}
