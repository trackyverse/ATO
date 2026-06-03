#' Count orphan detections and issue message
#' 
#' Used by the match functions
#' 
#' @param x an ATO object
#' @param silent should a message be issued?
#' 
#' @return The number of orphans.
#' 
#' @keywords internal
#' 
.message_orphan_dets <- function(x, silent = FALSE) {
  orphans <- is.na(x@det$dep_match) & x@det$valid
  if (!silent & any(orphans)) {
    n_rec <- length(unique(x@det$receiver_serial[orphans]))
    message("M: ", sum(orphans), " valid detection", .s(sum(orphans)),
            " (from ", n_rec, " receiver", .s(n_rec),
            ") do not match receivers listed in @dep,",
            " or fall outside their deployment periods",
            " (orphan detection", .s(sum(orphans)), ").")
  }
  return(sum(orphans))
}

#' Count stray detections and issue message
#' 
#' Used by the match functions
#' 
#' @inheritParams .message_orphan_dets
#' 
#' @return The number of stray detections.
#' 
#' @keywords internal
#' 
.message_stray_dets <- function(x, silent = FALSE) {
  if (is.null(x@det$tag_match)) {
    strays <- is.na(x@det$beacon_match) & x@det$valid
  }
  if (is.null(x@det$beacon_match)) {
    strays <- is.na(x@det$tag_match) & x@det$valid
  }
  if (!is.null(x@det$tag_match) & !is.null(x@det$beacon_match)) {
    strays <- is.na(x@det$tag_match) & is.na(x@det$beacon_match) & x@det$valid
  }
  if (!silent & any(strays)) {
    n_tags <- length(unique(x@det$transmitter[strays]))
    if (is.null(x@det$tag_match)) {
      message(
        "M: ", sum(strays), " valid detection", .s(sum(strays)),
        " (from ", n_tags, " transmitter", .s(n_tags),
        ") do not match transmitters listed in @dep,",
        " or fall outside their active times,",
        " but @tag not yet matched.")
    }
    if (is.null(x@det$beacon_match)) {
      message(
        "M: ", sum(strays), " valid detection", .s(sum(strays)),
        " (from ", n_tags, " transmitter", .s(n_tags),
        ") do not match transmitters listed in @tag,",
        " or fall outside their active times,",
        " but @dep not yet matched.")
    }
    if (!is.null(x@det$tag_match) & !is.null(x@det$beacon_match)) {
      message("M: ", sum(strays), " valid detection", .s(sum(strays)),
              " (from ", n_tags, " transmitter", .s(n_tags),
              ") do not match transmitters listed in @tag or @dep,",
              " or fall outside their active times",
              " (stray detection", .s(sum(strays)), ").")
    }
  }
  return(sum(strays))
}

#' Count number of zeros
#' 
#' Used by the match functions
#' 
#' @param x the vector of numbers
#' @param longslot long name of the slot (e.g. animals)
#' @param what what's being counted?
#' @param silent should a message be issued?
#' 
#' @return The number of zeros.
#' 
#' @keywords internal
#' 
.message_n_zero <- function(x, longslot, what, silent) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    x <- x[!is.na(x)]
    check <- sum(x == 0)
    if (!silent & check > 0) {
      message("M: ", check, " valid ", longslot, .s(sum(check)),
              " ", .has(sum(check)), " no ", what, ".")
    }
    return(check)
  }
}

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
  if (n == 1) {
    return("has")
  } else {
    return("have")
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
  if (n == 1) {
    return("was")
  } else {
    return("were")
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
  if (n == 1) {
    return("is")
  } else {
    return("are")
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
  if (n == 1) {
    if (reverse) {
      return("s")
    } else {
      return("")
    }
  } else {
    if (reverse) {
      return("")
    } else {
      return("s")
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
  if (n == 1) {
    if (reverse) {
      return("es")
    } else {
      return("")
    }
  } else {
    if (reverse) {
      return("")
    } else {
      return("es")
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
  if (n == 1) {
    return("y")
  } else {
    return("ies")
  }
}
