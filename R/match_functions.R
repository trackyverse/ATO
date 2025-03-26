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
              " stray transmitters) found in the dataset.")
    } else {
      message("M: Number of off-target detections updated to ",
              sum(!x@det$tag_match),
              " ( ",
              length(unique(x@det$transmitter[!x@det$tag_match])),
              " stray transmitters).")
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
  x@det$dep_match <- NA
  for (i in 1:nrow(x@dep)) {
    link <- x@det$datetime >= x@dep$deploy_datetime[i] &
            x@det$datetime <= x@dep$recover_datetime[i] &
            x@det$receiver_serial == x@dep$receiver_serial[i]
    x@det$dep_match[link] <- i
  }
  if (!silent & any(is.na(x@det$dep_match))) {
    message("M: ", sum(is.na(x@det$dep_match)),
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
  x@det$tag_match <- x@det$tag_match | x@det$transmitter %in% x@dep$transmitter
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
  aux <- table(x@det$dep_match)
  x@dep$n_detections[as.numeric(names(aux))] <- aux
  if (!silent & any(x@dep$n_detections == 0)) {
    message("M: ", sum(x@dep$n_detections == 0),
            " deployed receivers have no detections.")
  }
  return(x)
}

match_det_ani <- function(x, silent = FALSE) {
  animal_link <- match(x@det$transmitter, x@tag$transmitter)
  x@det$animal <- x@tag$animal[animal_link]
  x@ani$det_match <- x@ani@animal %in% x@det$animal
  if (!silent & any(!x@ani$det_match)) {
    message("M: ", sum(!x@ani$det_match),
            " animals were never detected.")
  }
  return(x)
}

match_obs_tag <- function(x, silent = FALSE) {
  x@obs$tag_match <- x@obs$transmitter %in% x@tag$transmitter
  strays <- !is.na(x@obs$transmitter) & !x@obs$tag_match
  if (!silent & any(strays)) {
    message("M: ", sum(strays),
            " off-target observations (from ",
            length(unique(x@obs$transmitter[strays])),
            " stray transmitters) found in the dataset.")
  }
  x@tag$obs_match <- x@tag$transmitter %in% x@obs$transmitter
  if (!silent & any(!x@tag$obs_match)) {
    message("M: ", sum(!x@tag$obs_match),
            " target trasmitters were never observed.")
  }
  return(x)
}

match_obs_ani <- function(x, silent = FALSE) {
  x@obs$ani_match <- x@obs$animal %in% x@ani$animal
  strays <- !is.na(x@obs$animal) & !x@obs$ani_match
  if (!silent & any(strays)) {
    message("M: ", sum(strays),
            " off-target observations (from ",
            length(unique(x@obs$transmitter[strays])),
            " stray animals) found in the dataset.")
  }
  x@ani$obs_match <- x@ani$transmitter %in% x@obs$transmitter
  if (!silent & any(!x@ani$obs_match)) {
    message("M: ", sum(!x@ani$obs_match),
            " target animals were never observed.")
  }
  return(x)
}

match_tag_ani <- function(x, silent = FALSE) {
  x@tag$ani_match <- x@tag$animal %in% x@ani$animal
  if (!silent & any(!x@tag$ani_match)) {
    message("M: ", sum(!x@tag$ani_match),
            " transmitters in @tag do not belong to",
            " animals listed in @ani.")
  }
  x@ani$tag_match <- x@ani$animal %in% x@tag$animal
  if (!silent & any(!x@ani$tag_match)) {
    message("M: ", sum(!x@ani$tag_match),
            " animals do not have associated transmitters")
  }
  if (nrow(x@det) > 0) {
    x <- match_det_ani(x)
  }
  return(x)
}