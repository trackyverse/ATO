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

.data.table_exists <- function(error = TRUE) {
  # data.table's special functions are hard to handle with data.table
  # as a suggested package because this prevents us from using
  # importFrom calls. Instead, this function loads all of data.table
  # if we are about to use it.
  # However, for code clarity and for the sanity of future developers,
  # you should still use data.table:: wherever possible!
  if (!"data.table" %in% utils::installed.packages()) {
    if (error) {
      stop("You must install package data.table to run this function.", call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

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

# .comma(LETTERS[1])
# .comma(LETTERS[1:2])
# .comma(LETTERS[1:3])
# .comma(LETTERS[1:5])
# .comma(LETTERS[1:10])
#
# .comma(LETTERS[1], max_i = 2)
# .comma(LETTERS[1:2], max_i = 2)
# .comma(LETTERS[1:3], max_i = 2)
# .comma(LETTERS[1:5], max_i = 2)
# .comma(LETTERS[1:10], max_i = 2)
#
# .comma(LETTERS[1], max_i = 1)
# .comma(LETTERS[1:2], max_i = 1)
# .comma(LETTERS[1:3], max_i = 1)
# .comma(LETTERS[1:5], max_i = 1)
# .comma(LETTERS[1:10], max_i = 1)
#
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

.has <- function(n) {
  if (n > 1) {
    return("have")
  } else {
    return("has")
  }
}

.was <- function(n) {
  if (n > 1) {
    return("were")
  } else {
    return("was")
  }
}

.is <- function(n) {
  if (n > 1) {
    return("are")
  } else {
    return("is")
  }
}

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

.y <- function(n) {
  if (n > 1) {
    return("ies")
  } else {
    return("y")
  }
}
