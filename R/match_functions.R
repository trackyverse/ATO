match_det_tag <- function(x, silent = FALSE) {
  # column might already exist through deployment match
  if (is.null(x@det$tag_match)) {
    x@det$tag_match <- FALSE
    new_message <- TRUE
  } else {
    new_message <- FALSE
  }
  x@det$tag_match <- x@det$tag_match | x@det$transmitter %in% x@tag$transmitter
  if (!silent & any(!x@det$tag_match)) {
    if (new_message) {
      message("M: ", sum(!x@det$tag_match),
              " off-target detections (from ",
              length(unique(x@det$transmitter[!x@det$tag_match])),
              " stray signals) found in the dataset.")
    } else {
      message("M: Number of off-target detections updated to ",
              sum(!x@det$tag_match),
              " ( ",
              length(unique(x@det$transmitter[!x@det$tag_match])),
              " stray signals).")
    }
  }
  x@tag$det_match <- x@tag$transmitter %in% x@det$transmitter
  if (!silent & any(!x@tag$det_match)) {
    message("M: ", sum(!x@tag$det_match),
            " target trasmitters were never detected.")
  }
  return(x)
}

match_det_dep <- function(x, silent = FALSE) {
  aux <- as.data.table(x@det)
  # assign deployments to detections
  x@det$deploy_match <- NA
  for (i in 1:nrow(x@dep)) {
    link <- x@det$datetime >= x@dep$deploy_datetime[i] &
            x@det$datetime <= x@dep$recover_datetime[i] &
            x@det$receiver_serial == x@dep$receiver_serial[i]
    x@det$deploy_match[link] <- i
  }
  if (!silent & any(is.na(x@det$deploy_match))) {
    message("M: ", sum(is.na(x@det$deploy_match)),
            " detections do not fall within deployment periods",
            " or do not belong to receivers listed in the deployments.")
  }
  # if the receivers have transmitters, check those against detections
  # column might already exist through tags match
  if (is.null(x@det$tag_match)) {
    x@det$tag_match <- FALSE
    new_message <- TRUE
  } else {
    new_message <- FALSE
  }
  x@det$tag_match <- x@det$tag_match | 
                              x@det$transmitter %in% x@dep$transmitter
  if (!silent & any(!x@det$tag_match)) {
    if (new_message) {
      message("M: ", sum(!x@det$tag_match),
              " beacon detections found in the dataset.")
    } else {
      message("M: Number of off-target detections updated to ",
              sum(!x@det$tag_match),
              " after matching beacon transmitters (",
              length(unique(x@det$transmitter[!x@det$tag_match])),
              " stray signals).")
    }
  }
  # include counts of detections per deployment
  x@dep$n_detections <- 0
  aux <- table(x@det$deploy_match)
  x@dep$n_detections[as.numeric(names(aux))] <- aux
  if (!silent & any(x@dep$n_detections == 0)) {
    message("M: ", sum(x@dep$n_detections == 0),
            " deployed receivers have no detections.")
  }
  return(x)
}
