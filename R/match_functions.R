#' Match the various slots of the ATO object
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @export
#' 
match_update <- function(x, silent = FALSE) {
  # always match obs > ani before ani > tag 
  # so columns are transferred properly
  
  time_start <- Sys.time()

  if (has(x, c("obs", "ani"))) {
    if (!silent) {
      message("M: Matching @obs to @ani...")
    }
    x <- .match_obs_ani(x, silent = silent)
  } else {
    if (has(x, "obs")) {
      x@obs$ani_match <- NULL
    }
    if (has(x, "ani")) {
      x@ani$n_obs <- NULL
      x@ani$terminal_location <- NULL
      x@ani$terminal_datetime <- NULL
      x@ani$terminal_lat <- NULL
      x@ani$terminal_lon <- NULL
    }
  }

  if (has(x, c("ani", "tag"))) {
    if (!silent) {
      message("M: Matching @ani to @tag...")
    }
    x <- .match_ani_tag(x, silent = silent)
  } else {
    if (has(x, "ani")) {
      x@ani$n_tag <- NULL
    }
    if (has(x, "tag")) {
      x@tag$ani_match
      x@tag$release_location <- NULL
      x@tag$release_datetime <- NULL
      x@tag$release_lat <- NULL
      x@tag$release_lon <- NULL
    }
  }

  if (has(x, c("obs", "tag"))) {
    if (!silent) {
      message("M: Matching @obs to @tag...")
    }
    x <- .match_obs_tag(x, silent = silent)
  } else {
    if (has(x, "obs")) {
      x@obs$tag_match <- NULL
    }
    if (has(x, "tag")) {
      x@tag$n_obs <- NULL
    }
  }

  if (has(x, c("tag", "det"))) {
    if (!silent) {
      message("M: Matching @tag to @det...")
    }
    x <- .match_tag_det(x, silent = silent)
  } else {
    if (has(x, "det")) {
      x@det$tag_match <- NULL
      x@det$animal <- NULL
      x@det$ani_match <- NULL
      x@det$animal <- NULL
    }
    if (has(x, "tag")) {
      x@tag$n_det <- NULL
    }
    if (has(x, "ani")) {
      x@ani$n_det <- NULL
    }
  }

  if (has(x, c("det", "dep"))) {
    if (!silent) {
      message("M: Matching @det to @dep...")
    }
    x <- .match_det_dep(x, silent = silent)
  } else {
    if (has(x, "det")) {
      x@det$dep_match <- NULL
      x@det$beacon_match <- NULL
    }
    if (has(x, "dep")) {
      x@dep$n_det <- NULL
      x@dep$n_beacon_det <- NULL
    }
  }

  time_dur <- difftime(Sys.time(), time_start, units = "s")
  im_msg <- getOption("ATO_match_immediate_msg", default = TRUE)
  if (time_dur > 60 & im_msg) {
    options(ATO_match_immediate_msg = FALSE)
    message("M: If matches are taking very long, consider",
            " switching to data.table ATOs to speed them up.")
  }
  return(x)
}

#' Match animals in the observations to the target animals
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_obs_ani <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("obs", "ani"), error = TRUE)

  # match only valid animals
  aux <- x@ani$animal
  aux[!x@ani$valid] <- NA
  x@obs$ani_match <- match(x@obs$animal, aux)

  # issue message if there are valid observations for non-valid animals
  strays <- !is.na(x@obs$animal) &
            is.na(x@obs$ani_match) &
            x@obs$valid
  if (!silent & any(strays)) {
    n_animals <- length(unique(x@obs$animal[strays]))
    message("M: ", sum(strays),
            " off-target valid observation", .s(sum(strays)),
            " (from ", n_tags, " stray animal", .s(n_animals),
            ") found in the dataset.")
  }

  # include counts of valid observations per animal
  x@ani$n_obs <- 0
  aux <- table(x@obs$ani_match[x@obs$valid])
  x@ani$n_obs[as.numeric(names(aux))] <- aux

  # issue message if some valid animals have no observations
  check <- sum(x@ani$n_obs[x@ani$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid animal", .s(check),
            " ", .has(check), " no valid observations.")
  }

  # include terminal details
  x@ani$terminal_location <- NA_character_
  the_tz <- attributes(x@obs$datetime)$tzone
  x@ani$terminal_datetime <- as.POSIXct(NA_real_, tz = the_tz)
  x@ani$terminal_lat <- NA_real_
  x@ani$terminal_lon <- NA_real_

  for (i in 1:nrow(x@ani)) {
    if (x@ani$n_obs[i] > 0) {
      # there should always be something valid or else the n_obs would be 0.
      link <- x@obs$valid & x@obs$ani_match == i
      aux <- tail(x@obs[link, ], 1)
      if (aux$terminal) {
        x@ani$terminal_location <- aux$location
        x@ani$terminal_datetime <- aux$datetime
        x@ani$terminal_lat <- aux$lat
        x@ani$terminal_lon <- aux$lon
      }
    }
  }

  return(x)
}

#' Match animals listed in the target tags to the target animals
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_ani_tag <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("tag", "ani"), error = TRUE)

  # match only valid animals
  aux <- x@ani$animal
  aux[!x@ani$valid] <- NA
  x@tag$ani_match <- match(x@tag$animal, aux)

  # issue message if there are valid tags for non-valid animals
  strays <- is.na(x@tag$ani_match) &
            x@tag$valid
  if (!silent & any(strays)) {
    n_ani <- length(unique(x@tag$animal[strays]))
    message("M: ", sum(strays),
            " off-target valid tag", .s(sum(strays)),
            " (from ", n_ani, " stray animal", .s(n_ani),
            ") found in the dataset.")
  }

  # include number of valid tags per animal
  x@ani$n_tag <- 0
  aux <- table(x@tag$ani_match[x@tag$valid])
  x@ani$n_tag[as.numeric(names(aux))] <- aux

  # issue message if some valid animals have no tags
  check <- sum(x@ani$n_tag[x@ani$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid animal", .s(check),
            " ", .has(check), " no valid tags.")
  }

  # check if a tag was used for more than one animal
  valid_tags <- x@tag[x@tag$valid, ]
  check <- duplicated(valid_tags$transmitter)
  if (any(check)) {
    # extract duplicated tags
    link <- valid_tags$transmitter %in% unique(valid_tags$transmitter[check])
    dup_tags <- valid_tags[link, ]
    if (any(is.na(dup_tags$animal))) {
      stop("Duplicated transmitters found in @tag but no @ani match.",
           " Fatal ambiguity. Can't assign detections correctly.",
           call. = FALSE)
    }
    if (is.null(x@tag$terminal_datetime)) {
      stop("Duplicated transmitters found in @tag but ato has no @obs",
           " or @obs and @ani have not been matched yet.",
           " Fatal ambiguity. Can't assign detections correctly.",
           call. = FALSE)      
    }
    # sort relevant animals by release time to confirm dates are acceptable
    link <- match(x@ani@animal %in% dup_tags$animal)
    sub_ani <- x@ani[link, ]
    sub_ani <- sub_ani[order(sub_ani$release_datetime), ]
    # now check timestamps by tag
    by_tag <- split(dup_tags, dup_tags$transmitter)
    recipient <- lapply(by_tag, function(x) {
      sub_ani_tag <- sub_ani[sub_ani$animal %in% x$animal, ]
      # the release time of animal 2 must come 
      # after the terminal time of animal 1
      check <- sub_ani_tag$terminal_datetime[-nrow(sub_ani_tag)] < 
               sub_ani_tag$release_datetime[-1]
      if (any(!check)) {
        stop("Transmitter ", x$transmitter[1],
             " was used in animals ", .comma(x$transmitter$animal),
             " but these animals were out in the field at the same time.",
             " Fatal ambiguity. Can't assign detections correctly.",
             call. = FALSE)
      }
    })
  }
  # include release details
  x@tag$release_location <- NA_character_
  the_tz <- attributes(x@ani$release_datetime)$tzone
  x@tag$release_datetime <- as.POSIXct(NA_real_, tz = the_tz)
  x@tag$release_lat <- NA_real_
  x@tag$release_lon <- NA_real_
  x@tag$release_location[!is.na(x@tag$ani_match)] <- x@ani$release_location[x@tag$ani_match]
  x@tag$release_datetime[!is.na(x@tag$ani_match)] <- x@ani$release_datetime[x@tag$ani_match]
  x@tag$release_lat[!is.na(x@tag$ani_match)] <- x@ani$release_lat[x@tag$ani_match]
  x@tag$release_lon[!is.na(x@tag$ani_match)] <- x@ani$release_lon[x@tag$ani_match]

  # include terminal details
  if (!is.null(x@ani$terminal_location)) {
    x@tag$terminal_location <- NA_character_
    the_tz <- attributes(x@ani$terminal_datetime)$tzone
    x@tag$terminal_datetime <- as.POSIXct(NA_real_, tz = the_tz)
    x@tag$terminal_lat <- NA_real_
    x@tag$terminal_lon <- NA_real_
    x@tag$terminal_location[!is.na(x@tag$ani_match)] <- x@ani$terminal_location[x@tag$ani_match]
    x@tag$terminal_datetime[!is.na(x@tag$ani_match)] <- x@ani$terminal_datetime[x@tag$ani_match]
    x@tag$terminal_lat[!is.na(x@tag$ani_match)] <- x@ani$terminal_lat[x@tag$ani_match]
    x@tag$terminal_lon[!is.na(x@tag$ani_match)] <- x@ani$terminal_lon[x@tag$ani_match]
  }
  return(x)
}

#' Match transmitters in the observations to the target tags
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_obs_tag <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("obs", "tag"), error = TRUE)

  # assign tags to observations
  x@obs$tag_match <- NA
  for (i in 1:nrow(x@tag)) {
    if (x@tag$valid[i]) {
      # placeholders
      transmitter <- x@tag$transmitter[i]
      first_time <- -Inf
      last_time <- Inf

      # if the tag has an associated activation time,
      # detections must be after that.
      if (!is.na(x@tag$activation_datetime[i])) {
        first_time <- x@tag$activation_datetime[i]
      }
      # if the tag has an associated release time,
      # detections must be after that.
      if (!is.null(x@tag$release_datetime) &&
          !is.na(x@tag$release_datetime[i])) {
        first_time <- x@tag$release_datetime[i]
      }
      # if the tag has an associated terminal time,
      # detections must be before that.
      if (!is.null(x@tag$terminal_datetime) &&
          !is.na(x@tag$terminal_datetime[i])) {
        last_time <- x@tag$terminal_datetime[i]
      }

      if ("atools" %in% rownames(installed.packages())) {
        checks <- list(transmitter = transmitter,
                       datetime = c(first_time, last_time))
        link <- atools::create_filter_vec(x, "obs", checks)
      } else {
        link <- x@obs$transmitter == transmitter &
                x@obs$datetime >= first_time &
                x@obs$datetime <= last_time
      }

      # check for ambiguity
      if (any(!is.na(x@obs$tag_match[link]))) {
        r <- which(!is.na(x@obs$tag_match[link]))
        stop("@obs row", .s(length(r)), " ", .comma(r),
             " match", .es(length(r), TRUE), " @tag rows ",
             .comma(c(unique(x@obs$tag_match[link]), i)), ".",
             " Fatal ambiguity. Can't assign observations correctly.",
             call. = FALSE)
      }

      # and assign the match
      x@obs$tag_match[link] <- i
    }
  }

  # issue message if there are valid observations for non-valid tagmals
  strays <- !is.na(x@obs$transmitter) &
            is.na(x@obs$tag_match) &
            x@obs$valid
  if (!silent & any(strays)) {
    n_tags <- length(unique(x@obs$transmitter[strays]))
    message("M: ", sum(strays),
            " off-target valid observation", .s(sum(strays)),
            " (from ", n_tags, " stray tag", .s(n_tags),
            ") found in the dataset.")
  }

  # include counts of valid observations per transmitter
  x@tag$n_obs <- 0
  aux <- table(x@obs$tag_match[x@obs$valid])
  x@tag$n_obs[as.numeric(names(aux))] <- aux

  # issue message if some valid tags have no observations
  check <- sum(x@tag$n_obs[x@tag$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid tag", .s(sum(check)),
            " ", .has(sum(check)), " no valid observations.")
  }

  return(x)
}

#' Match transmitters in the detections to the target tags
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_tag_det <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("det", "tag"), error = TRUE)

  # assign tags to detections
  x@det$tag_match <- NA
  if (!is.null(x@tag$ani_match)) {
    x@det$ani_match <- NA
    x@det$animal <- NA
  }

  cat(as.character(Sys.time()), ": for loop starting\n")
  for (i in 1:nrow(x@tag)) {
    cat(as.character(Sys.time()), " i = ", i, "\n")
    if (x@tag$valid[i]) {
      # placeholders
      transmitter <- x@tag$transmitter[i]
      first_time <- -Inf
      last_time <- Inf

      # if the tag has an associated activation time,
      # detections must be after that.
      if (!is.na(x@tag$activation_datetime[i])) {
        first_time <- x@tag$activation_datetime[i]
      }
      # if the tag has an associated release time,
      # detections must be after that.
      if (!is.null(x@tag$release_datetime) &&
          !is.na(x@tag$release_datetime[i])) {
        first_time <- x@tag$release_datetime[i]
      }
      # if the tag has an associated battery life,
      # detections must be before battery runs out.
      if (!is.na(x@tag$activation_datetime[i]) &
          !is.na(x@tag$battery_life[i])) {
        last_time <- x@tag$activation_datetime[i] +
                     x@tag$battery_life[i] * 3600
      }
      # if the tag has an associated terminal time,
      # detections must be before that.
      if (!is.null(x@tag$terminal_datetime) &&
          !is.na(x@tag$terminal_datetime[i])) {
        last_time <- x@tag$terminal_datetime[i]
      }
      if ("atools" %in% rownames(installed.packages())) {
        checks <- list(transmitter = transmitter,
                       datetime = c(first_time, last_time))
        link <- atools::create_filter_vec(x, "det", checks)
        cat(as.character(Sys.time()), ": vector done\n")
      } else {
        link <- x@det$transmitter == transmitter &
                x@det$datetime >= first_time &
                x@det$datetime <= last_time
      }

      # check for ambiguity
      if (any(!is.na(x@det$tag_match[link]))) {
        r <- which(!is.na(x@det$tag_match[link]))
        stop("@det row", .s(length(r)), " ", .comma(r),
             " match", .es(length(r), TRUE), " @tag rows ",
             .comma(c(unique(x@det$tag_match[link]), i)), ".",
             " Fatal ambiguity. Can't assign detections correctly.",
             call. = FALSE)
      }

      # and assign the match
      x@det$tag_match[link] <- i

      # last check: detections can't match both tags and beacons
      if (!is.null(x@det$beacon_match)) {
        check <- !is.na(x@det$tag_match) & !is.na(x@det$beacon_match)
        if (any(check)) {
          r <- which(check)
          stop("@det row", .s(length(r)), " ", .comma(r),
               " match", .es(length(r), TRUE), " both a row in @tag",
               " and a row in @dep. Fatal ambiguity.",
               " Can't assign detections correctly.",
               call. = FALSE)
        }
      }

      if (!is.null(x@tag$ani_match) && !is.na(x@tag$ani_match[i])) {
        x@det$ani_match[link] <- x@tag$ani_match[i]
        x@det$animal[link] <- x@tag$animal[i]
      }
    }
  }
  cat(as.character(Sys.time()), ": for loop done\n")

  # issue message if there are detections with no associated transmitter
  if (is.null(x@det$beacon_match)) {
    strays <- is.na(x@det$tag_match) & x@det$valid
  } else {
    strays <- is.na(x@det$tag_match) & is.na(x@det$beacon_match) & x@det$valid
  }
  if (!silent & any(strays)) {
    n_tags <- length(unique(x@det$transmitter[strays]))
    message("M: ", sum(strays), " valid detection", .s(sum(strays)),
            " (from ", n_tags, " transmitter", .s(n_tags),
            ") do not match transmitters listed on @tag or @dep.",
            " (stray detection", .s(sum(strays)), ").")
  }

  # include counts of valid detections per tag
  x@tag$n_det <- 0
  aux <- table(x@det$tag_match[x@det$valid])
  x@tag$n_det[as.numeric(names(aux))] <- aux

  # issue message if some valid tags have no detections
  check <- sum(x@tag$n_det[x@tag$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid tag", .s(sum(check)),
            " ", .has(sum(check)), " no valid detections.")
  }

  if (has(x, "ani")) {
    # include counts of valid detections per animal
    x@ani$n_det <- 0
    aux <- table(x@det$ani_match[x@det$valid])
    x@ani$n_det[as.numeric(names(aux))] <- aux
  }

  return(x)
}

#' Match transmitters in the detections to
#' beacon/reference transmitters in the deployments
#' 
#' Automatically called by the \code{\link{add}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_det_dep <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("det", "dep"), error = TRUE)

  # assign deps to detections
  x@det$dep_match <- NA
  x@det$beacon_match <- NA
  for (i in 1:nrow(x@dep)) {
    if (x@dep$valid[i]) {
      # placeholders
      first_time <- x@dep$deploy_datetime[i]
      last_time <- x@dep$recover_datetime[i]

      # receiver link
      if (!is.na(x@dep$receiver_serial[i])) {
        receiver_serial <- x@dep$receiver_serial[i]

        if ("atools" %in% rownames(installed.packages())) {
          checks <- list(receiver_serial = receiver_serial,
                         datetime = c(first_time, last_time))
          link <- atools::create_filter_vec(x, "det", checks)
        } else {
          link <- x@det$receiver_serial == receiver_serial &
                  x@det$datetime >= first_time &
                  x@det$datetime <= last_time
        }

        # check for ambiguity
        if (any(!is.na(x@det$dep_match[link]))) {
          r <- which(!is.na(x@det$dep_match[link]))
          stop("@det row", .s(length(r)), " ", .comma(r),
               " match", .es(length(r), TRUE), " @dep rows ",
               .comma(c(unique(x@det$dep_match[link]), i)),
               " (receiver_serial). Fatal ambiguity.",
               " Can't assign detections correctly.",
               call. = FALSE)
        }

        # and assign the match
        x@det$dep_match[link] <- i
      }

      # beacon link
      if (!is.na(x@dep$transmitter[i])) {
        transmitter <- x@dep$transmitter[i]

        if ("atools" %in% rownames(installed.packages())) {
          checks <- list(transmitter = transmitter,
                         datetime = c(first_time, last_time))
          link <- atools::create_filter_vec(x, "det", checks)
        } else {
          link <- x@det$receiver_serial == transmitter &
                  x@det$datetime >= first_time &
                  x@det$datetime <= last_time
        }

        # check for ambiguity
        if (any(!is.na(x@det$beacon_match[link]))) {
          r <- which(!is.na(x@det$beacon_match[link]))
          stop("@det row", .s(length(r)), " ", .comma(r),
               " match", .es(length(r), TRUE), " @dep rows ",
               .comma(c(unique(x@det$dep_match[link]), i)),
               " (transmitter). Fatal ambiguity.",
               " Can't assign detections correctly.",
               call. = FALSE)
        }

        # and assign the match
        x@det$beacon_match[link] <- i
      }

      # last check: detections can't match both tags and beacons
      if (!is.null(x@det$tag_match)) {
        check <- !is.na(x@det$tag_match) & !is.na(x@det$beacon_match)
        if (any(check)) {
          r <- which(check)
          stop("@det row", .s(length(r)), " ", .comma(r),
               " match", .es(length(r), TRUE), " both a row in @tag",
               " and a row in @dep. Fatal ambiguity.",
               " Can't assign detections correctly.",
               call. = FALSE)
        }
      }
    }
  }

  # issue message if there are detections with no associated deployment
  orphans <- is.na(x@det$dep_match) & x@det$valid
  if (!silent & any(orphans)) {
    n_rec <- length(unique(x@det$receiver_serial[orphans]))
    message("M: ", sum(orphans), " valid detection", .s(sum(orphans)),
            " (from ", n_rec, " receiver", .s(n_rec),
            ") do not match deployment periods",
            " (orphan detection", .s(sum(orphans)), ").")
  }

  # issue message if there are detections with no associated transmitter
  if (is.null(x@det$tag_match)) {
    strays <- is.na(x@det$beacon_match) & x@det$valid
  } else {
    strays <- is.na(x@det$tag_match) & is.na(x@det$beacon_match) & x@det$valid
  }
  if (!silent & any(strays)) {
    n_tags <- length(unique(x@det$transmitter[strays]))
    message("M: ", sum(strays), " valid detection", .s(sum(strays)),
            " (from ", n_tags, " transmitter", .s(n_tags),
            ") do not match transmitters listed on @tag or @dep.",
            " (stray detection", .s(sum(strays)), ").")
  }

  # include counts of valid detections per dep
  x@dep$n_det <- 0
  aux <- table(x@det$dep_match[x@det$valid])
  x@dep$n_det[as.numeric(names(aux))] <- aux

  # issue message if some valid deps have no detections
  check <- sum(x@dep$n_det[x@dep$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid receiver deployment", .s(sum(check)),
            " recorded no valid detections.")
  }

  # include counts of valid beacon detections per dep
  x@dep$n_beacon_det <- 0
  aux <- table(x@det$beacon_match[x@det$valid])
  x@dep$n_beacon_det[as.numeric(names(aux))] <- aux

  # issue message if some valid deps have no detections
  check <- sum(x@dep$n_beacon_det[x@dep$valid] == 0)
  if (!silent & check > 0) {
    message("M: ", check, " valid transmitter deployment", .s(sum(check)),
            " ", .was(sum(check)), " never detected.")
  }

  return(x)
}
