match_dets_tags <- function(x, silent = FALSE) {
  # column might already exist through deployment match
  if (is.null(x@detections$tag_match)) {
    x@detections$tag_match <- FALSE
    new_message <- TRUE
  } else {
    new_message <- FALSE
  }
  x@detections$tag_match <- x@detections$tag_match | 
                              x@detections$transmitter %in% x@tags$transmitter
  if (!silent & any(!x@detections$tag_match)) {
    if (new_message) {
      message("M: ", sum(!x@detections$tag_match),
              " off-target detections (from ",
              length(unique(x@detections$transmitter[!x@detections$tag_match])),
              " stray signals) found in the dataset.")
    } else {
      message("M: Number of off-target detections updated to ",
              sum(!x@detections$tag_match),
              " ( ",
              length(unique(x@detections$transmitter[!x@detections$tag_match])),
              " stray signals).")
    }
  }
  x@tags$det_match <- x@tags$transmitter %in% x@detections$transmitter
  if (!silent & any(!x@tags$det_match)) {
    message("M: ", sum(!x@tags$det_match),
            " target trasmitters were never detected.")
  }
  return(x)
}

match_dets_deps <- function(x, silent = FALSE) {
  aux <- as.data.table(x@detections)
  # assign deployments to detections
  x@detections$deploy_match <- NA
  for (i in 1:nrow(x@deployments)) {
    link <- x@detections$datetime >= x@deployments$deploy_datetime[i] &
            x@detections$datetime <= x@deployments$recover_datetime[i] &
            x@detections$receiver_serial == x@deployments$receiver_serial[i]
    x@detections$deploy_match[link] <- i
  }
  if (!silent & any(is.na(x@detections$deploy_match))) {
    message("M: ", sum(is.na(x@detections$deploy_match)),
            " detections do not fall within deployment periods",
            " or do not belong to receivers listed in the deployments.")
  }
  # if the receivers have transmitters, check those against detections
  # column might already exist through tags match
  if (is.null(x@detections$tag_match)) {
    x@detections$tag_match <- FALSE
    new_message <- TRUE
  } else {
    new_message <- FALSE
  }
  x@detections$tag_match <- x@detections$tag_match | 
                              x@detections$transmitter %in% x@deployments$transmitter
  if (!silent & any(!x@detections$tag_match)) {
    if (new_message) {
      message("M: ", sum(!x@detections$tag_match),
              " beacon detections found in the dataset.")
    } else {
      message("M: Number of off-target detections updated to ",
              sum(!x@detections$tag_match),
              " after matching beacon transmitters (",
              length(unique(x@detections$transmitter[!x@detections$tag_match])),
              " stray signals).")
    }
  }
  # include counts of detections per deployment
  x@deployments$n_detections <- 0
  aux <- table(x@detections$deploy_match)
  x@deployments$n_detections[as.numeric(names(aux))] <- aux
  if (!silent & any(x@deployments$n_detections == 0)) {
    message("M: ", sum(x@deployments$n_detections == 0),
            " deployed receivers have no detections.")
  }
  return(x)
}

