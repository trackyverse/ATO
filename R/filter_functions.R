filter_targeted <- function(object) {
  if (nrow(object@det) == 0) {
    message("M: No detections to filter.")
  } else {
    aux1 <- sum(object@det$tag_match)
    aux2 <- nrow(object@det)
    object@det <- object@det[object@det$tag_match, ]
    check(object@det)
    message("M: Filtered ", aux1,
            " detections (",
            .dyn_round(aux1 / aux2 * 100),
            "%) matching target transmitters.")
  }
  if (nrow(object@obs) == 0) {
    message("M: No observations to filter.")
  } else {
    aux1 <- sum(object@obs$tag_match)
    aux2 <- nrow(object@obs)
    object@obs <- object@obs[object@obs$tag_match, ]
    check(object@obs)
    message("M: Filtered ", aux1,
            " observations (",
            .dyn_round(aux1 / aux2 * 100),
            "%) matching target transmitters.")
  }
  return(object)
}

filter_ping_dev <- function(object, bands, grace) {
  if (nrow(object@det) == 0) {
    message("M: No detections to filter.")
  } else {
    if (is.null(object@det$ping_dev)) {
      stop("Ping deviation has not been calculated yet.",
           call. = FALSE)
    }
    if (any(!object@det$tag_match)) {
      stop("The dataset contains off-target detections.",
           " Please discard those first.", call. = FALSE)
    }
    by_receiver <- split(object@det,
                         object@det$receiver)
    # receiver <- by_receiver[[1]]
    recipient1 <- lapply(by_receiver, function(receiver) {
      # cat(receiver$receiver_serial[1], "\n")
      by_tag <- split(receiver,
                      receiver$transmitter)
      tag <- by_tag[[1]]
      recipient2 <- lapply(by_tag, function(tag) {
        # cat(" - ", tag$transmitter[1], "\n")
        tag <- tag[order(tag$datetime), ]
        to_keep <- rep(FALSE, nrow(tag))
        # simple hack to remove the NA at the start
        tag$ping_dev[1] <- Inf
        for (i in bands) {
          new_keep <- tag$ping_dev >= (i - grace) &
                      tag$ping_dev <= (i + grace)
          to_keep <- to_keep | new_keep
        }
        # keep also the detection that leads to the first
        # valid ping deviance, since the deviance relates to
        # the distance between i and i-1.
        aux <- rle(to_keep)
        to_flip <- cumsum(aux$lengths)[!aux$values]
        # the last detection can be picked up to flip.
        # we don't want that.
        if (tail(to_flip, 1) == nrow(tag)) {
          to_flip <- to_flip[-length(to_flip)]
        }
        to_keep[to_flip] <- TRUE
        # remove ping deviation values for the
        # salvaged detections so they don't cause
        # confusion going forward
        tag$ping_dev[to_flip] <- NA
        return(tag[to_keep, ])
      })
      output <- data.table::rbindlist(recipient2)
      return(output)
    })
    new_dets <- data.table::rbindlist(recipient1)

    aux1 <- nrow(new_dets)
    aux2 <- nrow(object@det)

    output <- as.data.frame(new_dets)
    class(output) <- c("ATO_det", class(output))
    object@det <- output

    message("M: Filtered ", aux1,
            " detections (",
            .dyn_round(aux1 / aux2 * 100),
            "%) matching set thresholds.")
  }
  return(object)
}

filter_datetime <- function(object, 
                            from = "1970-01-01",
                            to = "3000-01-01") {
  if (nrow(object@det) == 0) {
    message("M: No detections to filter.")
  } else {
    tz <- attributes(object@det$datetime)$tzone
    det_after_this <- object@det$datetime >= as.POSIXct(from, tz = tz)
    det_before_this <- object@det$datetime <= as.POSIXct(to, tz = tz)
    object@det <- object@det[det_after_this & det_before_this, ]

    aux1 <- nrow(object@det)
    aux2 <- length(det_after_this)
    message("M: Filtered ", aux1,
            " detections (",
            .dyn_round(aux1 / aux2 * 100),
            "%) within time tresholds.")
  }

  if (nrow(object@obs) == 0) {
    message("M: No observations to filter.")
  } else {
    tz <- attributes(object@obs$datetime)$tzone
    obs_after_this <- object@obs$datetime >= as.POSIXct(from, tz = tz)
    obs_before_this <- object@obs$datetime <= as.POSIXct(to, tz = tz)
    object@obs <- object@obs[obs_after_this & obs_before_this, ]

    aux1 <- nrow(object@obs)
    aux2 <- length(obs_after_this)
    message("M: Filtered ", aux1,
            " observations (",
            .dyn_round(aux1 / aux2 * 100),
            "%) within time tresholds.")
  }
  return(object)
}

