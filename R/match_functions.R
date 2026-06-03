.sub_na <- function(x, value){
  x[is.na(x)] <- value
  return(x)
}

#' Match the various slots of the ATO object
#' 
#' Automatically called by the \code{\link{set}} functions. You should not need
#' to call this function directly unless you have deactivated the ATO's
#' automatic matching feature (see \code{\link{ato_match_immediate}}).
#' 
#' For developers: You may deactivate ato_match_immediate() to be able to
#' to perform multiple actions in the ATO in quick sequence without incurring
#' the computational cost of repeated, needless matches. You can then run
#' match_update() on the ATO to match everything up in the end.
#' Remember to leave ato_match_immediate() in the same state that the user
#' set it to. See the code inside \code{\link{init_ato}} for an example.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @examples
#' old_match_immediate <- ato_match_immediate()
#' ato_match_immediate(FALSE)
#' x <- init_ato(ani = ani(example_ato),
#'              tag = tag(example_ato),
#'              det = det(example_ato),
#'              dep = dep(example_ato))
#' x <- match_update(x)
#' 
#' # clean up
#' ato_match_immediate(old_match_immediate)
#' rm(x, old_match_immediate)
#' 
#' @export
#' 
match_update <- function(x, silent = FALSE) {
  always_base <- getOption("ATO_force_base", default = FALSE)
  time_start <- last_break <- Sys.time()

  # NOTE:
  # always match obs to ani before ani to tag 
  # so columns are transferred properly
  # match functions are sorted by order of matching

  if (has(x, c("obs", "ani"))) {
    if (!silent) {
      message("M: Matching @obs to @ani...")
    }
    x <- .match_ani_obs(x, silent = silent)
    if (!silent & ato_debug()) {
      time_dur <- difftime(Sys.time(), last_break, units = "s")
      message("M: Matching @obs to @ani took ", round(time_dur, 3), "s")
      last_break <- Sys.time()
    }
  } else {
    if (has(x, "obs")) {
      if (!is.null(x@obs$ani_match)) {
        x@obs$ani_match <- NULL
      }
    }
    if (has(x, "ani")) {
      if (!is.null(x@ani$n_obs)) {
        x@ani$n_obs <- NULL
      }
      if (!is.null(x@ani$terminal_location)) {
        x@ani$terminal_location <- NULL
      }
      if (!is.null(x@ani$terminal_datetime)) {
        x@ani$terminal_datetime <- NULL
      }
      if (!is.null(x@ani$terminal_lat)) {
        x@ani$terminal_lat <- NULL
      }
      if (!is.null(x@ani$terminal_lon)) {
        x@ani$terminal_lon <- NULL
      }
    }
  }

  if (has(x, c("ani", "tag"))) {
    if (!silent) {
      message("M: Matching @ani to @tag...")
    }
    x <- .match_ani_tag(x, silent = silent)
    if (!silent & ato_debug()) {
      time_dur <- difftime(Sys.time(), last_break, units = "s")
      message("M: Matching @ani to @tag took ", round(time_dur, 3), "s")
      last_break <- Sys.time()
    }
  } else {
    if (has(x, "ani")) {
      if (!is.null(x@ani$n_tag)) {
        x@ani$n_tag <- NULL
      }
    }
    if (has(x, "tag")) {
      if (!is.null(x@tag$ani_match)) {
        x@tag$ani_match <- NULL
      }
      if (!is.null(x@tag$release_location)) {
        x@tag$release_location <- NULL
      }
      if (!is.null(x@tag$release_datetime)) {
        x@tag$release_datetime <- NULL
      }
      if (!is.null(x@tag$release_lat)) {
        x@tag$release_lat <- NULL
      }
      if (!is.null(x@tag$release_lon)) {
        x@tag$release_lon <- NULL
      }
    }
  }

  if (has(x, c("obs", "tag"))) {
    if (!silent) {
      message("M: Matching @obs to @tag...")
    }
    x <- .match_obs_tag(x, silent = silent)
    if (!silent & ato_debug()) {
      time_dur <- difftime(Sys.time(), last_break, units = "s")
      message("M: Matching @obs to @tag took ", round(time_dur, 3), "s")
      last_break <- Sys.time()
    }
  } else {
    if (has(x, "obs")) {
      if (!is.null(x@obs$tag_match)) {
        x@obs$tag_match <- NULL
      }
    }
    if (has(x, "tag")) {
      if (!is.null(x@tag$n_obs)) {
        x@tag$n_obs <- NULL
      }
    }
  }

  if (has(x, c("det", "tag"))) {
    if (!silent) {
      message("M: Matching @det to @tag...")
    }
    if (!always_base & .check_data.table_exists(FALSE)) {
      x <- .match_det_tag_datatable(x, silent = silent)
    } else {
      x <- .match_det_tag_base(x, silent = silent)
    }
    if (!silent & ato_debug()) {
      time_dur <- difftime(Sys.time(), last_break, units = "s")
      message("M: Matching @det to @tag took ", round(time_dur, 3), "s")
      last_break <- Sys.time()
    }
  } else {
    if (has(x, "det")) {
      if (!is.null(x@det$tag_match)) {
        x@det$tag_match <- NULL
      }
      if (!is.null(x@det$animal)) {
        x@det$animal <- NULL
      }
      if (!is.null(x@det$ani_match)) {
        x@det$ani_match <- NULL
      }
      if (!is.null(x@det$animal)) {
        x@det$animal <- NULL
      }
    }
    if (has(x, "tag")) {
      if (!is.null(x@tag$n_det)) {
        x@tag$n_det <- NULL
      }
    }
    if (has(x, "ani")) {
      if (!is.null(x@ani$n_det)) {
        x@ani$n_det <- NULL
      }
    }
  }

  if (has(x, c("dep", "det"))) {
    if (!silent) {
      message("M: Matching @dep to @det...")
    }
    if (!always_base & .check_data.table_exists(FALSE)) {
      x <- .match_dep_det_datatable(x, silent = silent)
    } else {
      x <- .match_dep_det_base(x, silent = silent)
    }
    if (!silent & ato_debug()) {
      time_dur <- difftime(Sys.time(), last_break, units = "s")
      message("M: Matching @dep to @det took ", round(time_dur, 3), "s")
      last_break <- Sys.time()
    }
  } else {
    if (has(x, "dep")) {
      if (!is.null(x@dep$n_det)) {
        x@dep$n_det <- NULL
      }
      if (!is.null(x@dep$n_beacon_det)) {
        x@dep$n_beacon_det <- NULL
      }
    }
    if (has(x, "det")) {
      if (!is.null(x@det$dep_match)) {
        x@det$dep_match <- NULL
      }
      if (!is.null(x@det$beacon_match)) {
        x@det$beacon_match <- NULL
      }
    }
  }

  time_dur <- difftime(Sys.time(), time_start, units = "s")
  im_msg <- getOption("ATO_match_immediate_msg", default = TRUE)
  if (time_dur > 60 & im_msg & !.check_data.table_exists(FALSE)) {
    options(ATO_match_immediate_msg = FALSE)
    message("Note: If matches are taking very long, consider",
            " installing data.table to speed them up.")
  }
  return(x)
}

#' Match animals in the observations to the target animals
#' 
#' Automatically called by the \code{\link{set}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_ani_obs <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("ani", "obs"), error = TRUE)

  if (!any(x@ani$valid)) {
    warning(
      "No valid animals, skipping ani-obs match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@ani$n_obs <- NULL
    x@obs$ani_match <- NULL
    return(x)
  }

  # match only valid animals
  aux <- x@ani$animal
  aux[!x@ani$valid] <- NA_character_
  x@obs$ani_match <- match(x@obs$animal, aux)

  # issue message if there are valid observations for non-valid animals
  strays <- !is.na(x@obs$animal) &
            is.na(x@obs$ani_match) &
            x@obs$valid
  if (!silent & any(strays)) {
    n_animals <- length(unique(x@obs$animal[strays]))
    message("M: ", sum(strays),
            " off-target valid observation", .s(sum(strays)),
            " (from ", n_animals, " stray animal", .s(n_animals),
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
      aux <- utils::tail(x@obs[link, ], 1)
      if (aux$terminal) {
        x@ani$terminal_location[i] <- aux$location
        x@ani$terminal_datetime[i] <- aux$datetime
        x@ani$terminal_lat[i] <- aux$lat
        x@ani$terminal_lon[i] <- aux$lon
      }
    }
  }

  return(x)
}

#' Match animals listed in the target tags to the target animals
#' 
#' Automatically called by the \code{\link{set}} functions.
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

  if (!any(x@ani$valid)) {
    warning(
      "No valid animals, skipping ani-tag match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@tag$ani_match <- NULL
    x@ani$n_tag <- NULL
    x@ani$n_det <- NULL
    return(x)
  }
  if (!any(x@tag$valid)) {
    warning(
      "No valid tags, skipping ani-tag match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@tag$ani_match <- NULL
    x@ani$n_tag <- NULL
    x@ani$n_det <- NULL
    return(x)
  }

  # match only valid animals
  aux <- x@ani$animal
  aux[!x@ani$valid] <- NA_character_
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
    if (any(is.na(dup_tags$ani_match))) {
      stop("Duplicated transmitters found in @tag but no @ani match.",
           " Fatal ambiguity. Can't assign detections correctly.",
           call. = FALSE)
    }
    if (is.null(x@ani$terminal_datetime)) {
      stop("Duplicated transmitters found in @tag but ato has no @obs",
           " (or @obs and @ani have not been matched yet).",
           " Fatal ambiguity. Can't assign detections correctly.",
           call. = FALSE)      
    }
    # sort relevant animals by release time to confirm dates are acceptable
    link <- x@ani$animal %in% dup_tags$animal
    sub_ani <- x@ani[link, ]
    sub_ani <- sub_ani[order(sub_ani$release_datetime), ]
    # now check timestamps by tag
    by_tag <- split(dup_tags, dup_tags$transmitter)
    recipient <- lapply(by_tag, function(the_tag) {
      sub_ani_tag <- sub_ani[sub_ani$animal %in% the_tag$animal, ]
      # all animals but the last must have terminal_datetime
      check <- is.na(sub_ani_tag$terminal_datetime[-nrow(sub_ani_tag)])
      if (any(check)) {
        stop(
          "Transmitter ", the_tag$transmitter[1],
          " was used in animals ", .comma(sub_ani_tag$animal),
          " but animal ", .s(sum(check)),
          .comma(sub_ani_tag$animal[which(check)]),
          " do", .es(sum(check), TRUE),
          " not have terminal_datetime information.",
          " Add terminal observations to allow tag reuse.",
          " Fatal ambiguity. Can't assign detections correctly.",
          call. = FALSE
        )        
      }
      # the release time of animal 2 must come 
      # after the terminal time of animal 1
      check <- sub_ani_tag$terminal_datetime[-nrow(sub_ani_tag)] < 
               sub_ani_tag$release_datetime[-1]
      if (any(!check)) {
        stop("Transmitter ", the_tag$transmitter[1],
             " was used in animals ", .comma(sub_ani_tag$animal),
             " but these animals were out in the field at the same time.",
             " Fatal ambiguity. Can't assign detections correctly.",
             call. = FALSE)
      }
    })
  }
  # include animal details
  # this is the first match, so reset the columns
  x@tag$release_location <- NA_character_
  the_tz <- attributes(x@ani$release_datetime)$tzone
  x@tag$release_datetime <- as.POSIXct(NA_real_, tz = the_tz)
  x@tag$release_lat <- NA_real_
  x@tag$release_lon <- NA_real_
  # then pass all new values in
  x@tag$release_location <- x@ani$release_location[x@tag$ani_match]
  x@tag$release_datetime <- x@ani$release_datetime[x@tag$ani_match]
  x@tag$release_lat <- x@ani$release_lat[x@tag$ani_match]
  x@tag$release_lon <- x@ani$release_lon[x@tag$ani_match]

  # include terminal details
  if (!is.null(x@ani$terminal_location)) {
    # this is the first match, so reset the columns
    x@tag$terminal_location <- NA_character_
    the_tz <- attributes(x@ani$terminal_datetime)$tzone
    x@tag$terminal_datetime <- as.POSIXct(NA_real_, tz = the_tz)
    x@tag$terminal_lat <- NA_real_
    x@tag$terminal_lon <- NA_real_
    # then pass all new values in
    x@tag$terminal_location <- x@ani$terminal_location[x@tag$ani_match]
    x@tag$terminal_datetime <- x@ani$terminal_datetime[x@tag$ani_match]
    x@tag$terminal_lat <- x@ani$terminal_lat[x@tag$ani_match]
    x@tag$terminal_lon <- x@ani$terminal_lon[x@tag$ani_match]
  }

  # issue warning if ani release time is after tag activation time
  check <- .sub_na(x@tag$activation_datetime > x@tag$release_datetime, FALSE)
  if (any(check)) {
    warning(
      "The activation time for transmitter",
      .s(sum(check)),
      " ", .comma(x@tag$transmitter[check]),
      " comes after the release time of the respective animal(s).",
      " Expect issues downstream.",
      call. = FALSE, immediate. = TRUE
    )
  }

  # issue warning if ani release time is after tag activation time
  check <- .sub_na(x@tag$activation_datetime > x@tag$terminal_datetime, FALSE)
  if (any(check)) {
    warning(
      "The activation time for transmitter",
      .s(sum(check)),
      " ", .comma(x@tag$transmitter[check]),
      " comes after the terminal time of the respective animal(s).",
      " Expect issues downstream.",
      call. = FALSE, immediate. = TRUE
    )
  }

  return(x)
}

#' Match transmitters in the observations to the target tags
#' 
#' Automatically called by the \code{\link{set}} functions.
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

  if (!any(x@tag$valid)) {
    warning(
      "No valid tags, skipping ani-tag match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@obs$tag_match <- NULL
    x@tag$n_obs <- NULL
    return(x)
  }

  # assign tags to observations
  x@obs$tag_match <- NA_integer_
  for (i in 1:nrow(x@tag)) {
    if (x@tag$valid[i]) {
      # placeholders
      transmitter <- x@tag$transmitter[i]
      first_time <- -Inf
      last_time <- Inf

      # if the tag has an associated activation time,
      # observations must be after that.
      if (!is.na(x@tag$activation_datetime[i])) {
        first_time <- x@tag$activation_datetime[i]
      }
      # if the tag has an associated release time,
      # observations must be after that.
      if (!is.null(x@tag$release_datetime) &&
          !is.na(x@tag$release_datetime[i])) {
        # activation time should be before release. but in case it
        # isn't, pick the highest value of the two.
        first_time <- max(first_time, x@tag$release_datetime[i])
      }
      # if the tag has an associated terminal time,
      # observations must be before that.
      if (!is.null(x@tag$terminal_datetime) &&
          !is.na(x@tag$terminal_datetime[i])) {
        last_time <- x@tag$terminal_datetime[i]
      }

      link <- x@obs$transmitter == transmitter &
              x@obs$datetime >= first_time &
              x@obs$datetime <= last_time

      # check for ambiguity
      check <- !is.na(x@obs$tag_match[link])
      if (any(check)) {
        # I don't think this can ever happen. at this point, tag and
        # ani have already matched, so all tag information should be
        # valid. I tried testing this and landed on
        # "Duplicated transmitters found in @tag but no @ani match"
        r <- which(check)
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

  # tag might already have mached info from ani.
  # Don't overwrite those
  if (is.null(x@tag$terminal_location)) {
    x@tag$terminal_location <- NA_character_
    the_tz <- attributes(x@obs$datetime)$tzone
    x@tag$terminal_datetime <- as.POSIXct(NA_real_, tz = the_tz)
    x@tag$terminal_lat <- NA_real_
    x@tag$terminal_lon <- NA_real_
  }

  if (any(!is.na(x@obs$tag_match))) {
    sub_obs <- x@obs[!is.na(x@obs$tag_match), ]
    by_tag <- split(sub_obs, sub_obs$transmitter)
    for (the_tag in by_tag) {
      if (all(!the_tag$terminal)) {
        next()
      }
      term <- the_tag[the_tag$terminal, ]
      # make sure terminal detections are in chronological order
      term <- term[order(term$datetime), ]
      if (nrow(term) > 1) {
      # make_obs should stop this from happening.
        stop(
          "more than one terminal datetime found for tag ",
          term$transmitter[1],
          call. = FALSE
        )
      }
      for (i in 1:nrow(term)) {
        link <- x@tag$transmitter == term$transmitter[i] &
          .sub_na(x@tag$activation_datetime <= term$datetime[i], TRUE) &
          .sub_na(x@tag$terminal_datetime >= term$datetime[i], TRUE)
        
        if (!is.null(x@tag$release_datetime)) {
          link <- link &
            .sub_na(x@tag$release_datetime <= term$datetime[i], TRUE)
        }
        if (sum(link) > 1) {
          # placeholder error for when someone somehow trips into this
          stop(
            "too many matches. contact dev.",
            call. = FALSE
          )
        }
        if (sum(link) == 0) {
          # placeholder error for when someone somehow trips into this
          stop(
            "no matches. contact dev.",
            call. = FALSE
          )
        }
        x@tag$terminal_location[link] <- term$location[i]
        x@tag$terminal_datetime[link] <- term$datetime[i]
        x@tag$terminal_lat[link] <- term$lat[i]
        x@tag$terminal_lon[link] <- term$lon[i]
      }
    }
  }

  # issue warning if terminal time is after activation time
  check <- .sub_na(x@tag$activation_datetime > x@tag$terminal_datetime, FALSE)
  if (any(check)) {
    warning(
      "The activation time for transmitter",
      .s(sum(check)),
      " ", .comma(x@tag$transmitter[check]),
      " comes after the terminal time of the respective transmitter(s).",
      " Expect issues downstream.",
      call. = FALSE, immediate. = TRUE
    )
  }
  
  return(x)
}

#' Match transmitters in the detections to the target tags
#' 
#' Automatically called by the \code{\link{set}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_det_tag_base <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("det", "tag"), error = TRUE)

  # assign tags to detections
  x@det$tag_match <- NA_integer_
  if (!is.null(x@tag$ani_match)) {
    x@det$ani_match <- NA_integer_
    x@det$animal <- NA_character_
  }

  if (!any(x@tag$valid)) {
    warning(
      "No valid tags, skipping det-tag match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@det$tag_match <- NULL
    x@det$ani_match <- NULL
    x@det$animal <- NULL
    x@tag$n_det <- NULL
    x@ani$n_det <- NULL
    return(x)
  }

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
        # activation time should be before release. but in case it
        # isn't, pick the highest value of the two.
        first_time <- max(first_time, x@tag$release_datetime[i])
      }
      # if the tag has an associated battery life,
      # detections must be before battery runs out.
      if (!is.na(x@tag$activation_datetime[i]) &
          !is.na(x@tag$battery_life[i])) {
        last_time <- x@tag$activation_datetime[i] +
                     x@tag$battery_life[i] * 24 * 3600
      }
      # if the tag has an associated terminal time,
      # detections must be before that.
      if (!is.null(x@tag$terminal_datetime) &&
          !is.na(x@tag$terminal_datetime[i])) {
        # only replace if battery hasn't run out yet
        last_time <- min(last_time, x@tag$terminal_datetime[i])
      }

      link <- x@det$transmitter == transmitter &
              x@det$datetime >= first_time &
              x@det$datetime <= last_time

      # check for ambiguity
      .check_dup_match_base(x@det$tag_match[link], i, "det", "tag")

      # and assign the match
      x@det$tag_match[link] <- i

      # last check: detections can't match both tags and beacons
      .check_tag_beacon(x)

      if (!is.null(x@tag$ani_match) && !is.na(x@tag$ani_match[i])) {
        x@det$ani_match[link] <- x@tag$ani_match[i]
        x@det$animal[link] <- x@tag$animal[i]
      }
    }
  }

  # issue message if there are detections with no associated transmitter
  .message_stray_dets(x, silent = silent)

  # include counts of valid detections per tag
  x@tag$n_det <- 0
  aux <- table(x@det$tag_match[x@det$valid])
  x@tag$n_det[as.numeric(names(aux))] <- aux

  # issue message if some valid tags have no detections
  .message_n_zero(x@tag$n_det[x@tag$valid], 
                  "tag", "valid detections", silent = silent)

  if (has(x, "ani")) {
    # include counts of valid detections per animal
    x@ani$n_det <- 0
    aux <- table(x@det$ani_match[x@det$valid])
    x@ani$n_det[as.numeric(names(aux))] <- aux
  }

  return(x)
}

#' Match transmitters in the detections to the target tags
#' 
#' Much faster than \code{\link{.match_det_tag_base}}
#' 
#' Automatically called by the \code{\link{match_update}} function.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_det_tag_datatable <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, c("det", "tag"), error = TRUE)
  .check_data.table_exists()

  if (!any(x@tag$valid)) {
    warning(
      "No valid tags, skipping det-tag match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@det$tag_match <- NULL
    x@det$ani_match <- NULL
    x@det$animal <- NULL
    x@tag$n_det <- NULL
    x@ani$n_det <- NULL
    return(x)
  }
  
  first_time <- sapply(
    1:nrow(x@tag),
    function(i) {
      suppressWarnings(
        max(
          x@tag$release_datetime[i],
          x@tag$activation_datetime[i],
          na.rm = TRUE
        )
      )
    }
  )
  the_tz <- attributes(x@tag$activation_datetime)$tzone
  first_time <- as.POSIXct(first_time, tz = the_tz)

  # the preferred last time is the animal terminal observation time
  last_time <- sapply(
    1:nrow(x@tag),
    function(i) {
      suppressWarnings(
        min(
          x@tag$terminal_datetime[i],
          x@tag$activation_datetime[i] + x@tag$battery_life[i] * 24 * 3600,
          na.rm = TRUE
        )
      )
    }
  )
  the_tz <- attributes(x@tag$activation_datetime)$tzone
  last_time <- as.POSIXct(last_time, tz = the_tz)

  # start manipulating the slots
  x@det$original_row_order <- 1:nrow(x@det)
  x@tag$original_row_order <- 1:nrow(x@tag)

  original_det_classes <- class(x@det)
  original_tag_classes <- class(x@tag)

  if (data.table::haskey(x@det)) {
    original_det_keys <- data.table::key(x@det)
  } else {
    original_det_keys <- NULL
  }

  if (data.table::haskey(x@tag)) {
    original_tag_keys <- data.table::key(x@tag)
  } else {
    original_tag_keys <- NULL
  }

  data.table::setDT(x@det)
  class(x@det) <- c("ATO_det", class(x@det))
  data.table::setDT(x@tag)
  class(x@tag) <- c("ATO_tag", class(x@tag))

  # data.table::foverlaps needs two time columns
  data.table::set(x@det, j = "datetime_dummy", value = x@det$datetime)
  data.table::set(x@tag, j = "first_time", value = first_time)
  data.table::set(x@tag, j = "last_time", value = last_time)

  on.exit(expr = {
    # reset row order
    data.table::setkeyv(x@det, "original_row_order")
    data.table::setkeyv(x@tag, "original_row_order")
    # reset original keys
    if (is.null(original_det_keys)) {
      data.table::setkey(x@det, NULL)
    } else {
      data.table::setkeyv(x@det, original_det_keys)
    }
    if (is.null(original_tag_keys)) {
      data.table::setkey(x@tag, NULL)
    } else {
      data.table::setkeyv(x@tag, original_tag_keys)
    }
    # remove temporary columns
    data.table::set(x@det, j = "datetime_dummy", value = NULL)
    data.table::set(x@det, j = "original_row_order", value = NULL)
    data.table::set(x@tag, j = "first_time", value = NULL)
    data.table::set(x@tag, j = "last_time", value = NULL)
    data.table::set(x@tag, j = "original_row_order", value = NULL)

    # reset original classes
    if (!"data.table" %in% original_det_classes) {
      data.table::setDF(x@det)
      data.table::setattr(x@det, "class", original_det_classes)
    }
    if (!"data.table" %in% original_tag_classes) {
      data.table::setDF(x@tag)
      data.table::setattr(x@tag, "class", original_tag_classes)
    }
  })

  # perform the match using only valid tags
  valid_tags <- x@tag[x@tag$valid, ]

  data.table::setkeyv(valid_tags, 
                      c("transmitter",
                        "first_time",
                        "last_time"))
  data.table::setkeyv(x@det, 
                      c("transmitter",
                        "datetime",
                        "datetime_dummy"))
  result <- data.table::foverlaps(
    x@det,
    valid_tags,
    nomatch = NA,
    which = TRUE)

  # check for ambiguous matches
  .check_dup_match_datatable(
    result$xid,
    result$yid,
    "dep",
    "tag"
  )

  # assign the match - only from the valid subset of tags
  data.table::set(
    x@det,
    i = result$xid,
    j = "tag_match",
    value = valid_tags$original_row_order[result$yid]
    )

  # last check: detections can't match both tags and beacons
  # Note: this check is placed here instead of further down 
  # in .match_det_tag because the base method runs this check
  # on a per-tag basis to avoid wasted computing time.
  .check_tag_beacon(x)

  # issue message if there are detections with no associated transmitter
  .message_stray_dets(x, silent = silent)

  # include counts of valid detections per tag
  x@tag$n_det <- 0
  aux <- table(x@det$tag_match[x@det$valid])
  row_link <- match(as.numeric(names(aux)), x@tag$original_row_order)
  x@tag$n_det[row_link] <- aux

  # issue message if some valid tags have no detections
  .message_n_zero(x@tag$n_det[x@tag$valid],
                  "tag", "valid detections", silent = silent)

  if (has(x, "ani")) {
    # match animals through tags
    # Make sure tag is in the original order
    # so we can use tag_match as a row identifier
    data.table::setkeyv(x@tag, "original_row_order")
    # now set things
    data.table::set(
      x@det,
      j = "ani_match",
      value = x@tag$ani_match[x@det$tag_match]
    )
    data.table::set(
      x@det,
      j = "animal",
      value = x@tag$animal[x@det$tag_match]
    )

    # include counts of valid detections per animal
    x@ani$n_det <- 0
    aux <- table(x@det$animal[x@det$valid])
    row_link <- match(names(aux), x@ani$animal)
    x@ani$n_det[row_link] <- aux
  } else {
    # remove any old matches
    if (!is.null(x@det$ani_match)) {
      data.table::set(x@det, j = "ani_match", value = NULL)
      data.table::set(x@det, j = "animal", value = NULL)
    }
  }

  # on.exit handles cleaning up the data.table modifications
  return(x)
}

#' Match transmitters in the detections to
#' beacon/reference transmitters in the deployments
#' 
#' Automatically called by the \code{\link{set}} functions.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.match_dep_det_base <- function(x, silent = FALSE) {
  is_ato(x)
  has(x, "dep",
      c("receiver_serial", "deploy_datetime", "recover_datetime"),
      allow_NA = FALSE, error = TRUE)
  has(x, "det",
      c("receiver_serial", "datetime"),
      allow_NA = FALSE, error = TRUE)

  if (!any(x@dep$valid)) {
    warning(
      "No valid deployments, skipping dep-det match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@det$dep_match <- NULL
    x@det$beacon_match <- NULL
    x@dep$n_det <- NULL
    x@dep$n_beacon_det <- NULL
    return(x)
  }

  # assign deps to detections
  x@det$dep_match <- NA_integer_
  x@det$beacon_match <- NA_integer_
  for (i in 1:nrow(x@dep)) {
    if (x@dep$valid[i]) {
      # placeholders
      first_time <- x@dep$deploy_datetime[i]
      last_time <- x@dep$recover_datetime[i]

      # receiver link
      if (!is.na(x@dep$receiver_serial[i])) {
        receiver_serial <- x@dep$receiver_serial[i]

        link <- x@det$receiver_serial == receiver_serial &
                x@det$datetime >= first_time &
                x@det$datetime <= last_time

        # check for ambiguity
        .check_dup_match_base(x@det$dep_match[link], i,
                              "det", "dep (receiver_serial)")

        # and assign the match
        x@det$dep_match[link] <- i
      }

      # beacon link
      if (!is.na(x@dep$transmitter[i])) {
        transmitter <- x@dep$transmitter[i]

        link <- x@det$transmitter == transmitter &
                x@det$datetime >= first_time &
                x@det$datetime <= last_time

        # check for ambiguity
        .check_dup_match_base(x@det$beacon_match[link], i,
                              "det", "dep (transmitter)")

        # and assign the match
        x@det$beacon_match[link] <- i
      }

      # last check: detections can't match both tags and beacons
      .check_tag_beacon(x)
    }
  }

  # issue message if there are detections with no associated deployment
  .message_orphan_dets(x, silent = silent)

  # issue message if there are detections with no associated transmitter
  .message_stray_dets(x, silent = silent)

  # include counts of valid detections per dep
  x@dep$n_det <- NA_integer_
  x@dep$n_det[!is.na(x@dep$receiver_serial)] <- 0
  aux <- table(x@det$dep_match[x@det$valid])
  x@dep$n_det[as.numeric(names(aux))] <- aux

  # issue message if some valid deps have no detections
  .message_n_zero(x@dep$n_det[x@dep$valid],
                  "receiver deployment", "valid detections",
                  silent = silent)

  # include counts of valid beacon detections per dep
  x@dep$n_beacon_det <- NA_integer_
  x@dep$n_beacon_det[!is.na(x@dep$transmitter)] <- 0
  aux <- table(x@det$beacon_match[x@det$valid])
  x@dep$n_beacon_det[as.numeric(names(aux))] <- aux

  # issue message if some valid deps have no detections
  .message_n_zero(x@dep$n_beacon_det[x@dep$valid],
                  "transmitter deployment", "valid detections",
                  silent = silent)

  return(x)
}

#' Match transmitters in the detections to
#' beacon/reference transmitters in the deployments
#' 
#' Much faster than \code{\link{.match_dep_det_base}}
#' 
#' Automatically called by the \code{\link{match_update}} function.
#' 
#' @param x an \code{\link{ATO}}
#' @param silent Supresses summary messages
#' 
#' @return the updated ATO
#' 
#' @keywords internal
.match_dep_det_datatable <- function(x, silent) {
  is_ato(x)
  has(x, "dep",
      c("receiver_serial", "deploy_datetime", "recover_datetime"),
      allow_NA = FALSE, error = TRUE)
  has(x, "det",
      c("receiver_serial", "datetime"),
      allow_NA = FALSE, error = TRUE)

  .check_data.table_exists()

  if (!any(x@dep$valid)) {
    warning(
      "No valid deployments, skipping dep-det match.",
      immediate. = TRUE,
      call. = FALSE
    )
    x@det$dep_match <- NULL
    x@det$beacon_match <- NULL
    x@dep$n_det <- NULL
    x@dep$n_beacon_det <- NULL
    return(x)
  }

  # start manipulating the slots
  x@det$original_row_order <- 1:nrow(x@det)
  x@dep$original_row_order <- 1:nrow(x@dep)

  original_det_classes <- class(x@det)
  original_dep_classes <- class(x@dep)

  if (data.table::haskey(x@det)) {
    original_det_keys <- data.table::key(x@det)
  } else {
    original_det_keys <- NULL
  }

  if (data.table::haskey(x@dep)) {
    original_dep_keys <- data.table::key(x@dep)
  } else {
    original_dep_keys <- NULL
  }

  data.table::setDT(x@det)
  class(x@det) <- c("ATO_det", class(x@det))
  data.table::setDT(x@dep)
  class(x@dep) <- c("ATO_dep", class(x@dep))

  # data.table::foverlaps needs two time columns
  data.table::set(x@det, j = "datetime_dummy", value = x@det$datetime)

  on.exit(expr = {
    # reset row order
    data.table::setkeyv(x@det, "original_row_order")
    data.table::setkeyv(x@dep, "original_row_order")
    # reset original keys
    if (is.null(original_det_keys)) {
      data.table::setkey(x@det, NULL)
    } else {
      data.table::setkeyv(x@det, original_det_keys)
    }
    if (is.null(original_dep_keys)) {
      data.table::setkey(x@dep, NULL)
    } else {
      data.table::setkeyv(x@dep, original_dep_keys)
    }
    # remove temporary columns
    data.table::set(x@det, j = "datetime_dummy", value = NULL)
    data.table::set(x@det, j = "original_row_order", value = NULL)
    data.table::set(x@dep, j = "original_row_order", value = NULL)

    # reset original classes
    if (!"data.table" %in% original_dep_classes) {
      data.table::setDF(x@dep)
      data.table::setattr(x@dep, "class", original_dep_classes)
    }
    if (!"data.table" %in% original_det_classes) {
      data.table::setDF(x@det)
      data.table::setattr(x@det, "class", original_det_classes)
    }
  })

  # perform the match using only valid deployments
  valid_deps <- x@dep[x@dep$valid, ]
  data.table::setkeyv(valid_deps,
                      c("receiver_serial",
                        "deploy_datetime",
                        "recover_datetime"))
  data.table::setkeyv(x@det,
                      c("receiver_serial",
                        "datetime",
                        "datetime_dummy"))
  result_rec <- data.table::foverlaps(
    x@det,
    valid_deps,
    nomatch = NA,
    which = TRUE
  )

  # check for ambiguous matches
  .check_dup_match_datatable(result_rec$xid, result_rec$yid, "det", "dep")

  # assign the match - from the set of valid deployments
  data.table::set(
    x@det,
    i = result_rec$xid,
    j = "dep_match",
    value = valid_deps$original_row_order[result_rec$yid]
  )

  # now do the same thing for the beacon tags
  # perform the match
  data.table::setkeyv(valid_deps,
                      c("transmitter",
                        "deploy_datetime",
                        "recover_datetime"))
  data.table::setkeyv(x@det,
                      c("transmitter",
                        "datetime",
                        "datetime_dummy"))
  result_bea <- data.table::foverlaps(
    x@det,
    valid_deps,
    nomatch = NA,
    which = TRUE
  )

  # check for ambiguous matches
  .check_dup_match_datatable(result_bea$xid, result_bea$yid, "det", "dep")

  # assign the match - using only valid deployments
  data.table::set(
    x@det,
    i = result_bea$xid,
    j = "beacon_match",
    value = valid_deps$original_row_order[result_bea$yid]
  )

  # last check: detections can't match both deps and beacons
  # Note: this check is placed here instead of further down 
  # in .match_det_dep because the base method runs this check
  # on a per-dep basis to avoid wasted computing time.
  .check_tag_beacon(x)

  # issue message if there are detections with no associated deployment
  .message_orphan_dets(x, silent = silent)

  # issue message if there are detections with no associated transmitter
  .message_stray_dets(x, silent = silent)

  # include counts of valid detections per dep
  x@dep$n_det <- NA_integer_
  x@dep$n_det[!is.na(x@dep$receiver_serial)] <- 0

  aux <- table(x@det$dep_match[x@det$valid])
  row_link <- match(as.numeric(names(aux)), x@dep$original_row_order)
  x@dep$n_det[row_link] <- aux

  # issue message if some valid deployments have no detections
  .message_n_zero(x@dep$n_det[x@dep$valid],
                  "receiver deployment", "valid detections",
                  silent = silent)

  # include counts of valid beacon detections per dep
  x@dep$n_beacon_det <- NA_integer_
  x@dep$n_beacon_det[!is.na(x@dep$transmitter)] <- 0

  aux <- table(x@det$beacon_match[x@det$valid])
  row_link <- match(as.numeric(names(aux)), x@dep$original_row_order)
  x@dep$n_beacon_det[row_link] <- aux

  # issue message if some valid deps have no detections
  .message_n_zero(x@dep$n_beacon_det[x@dep$valid],
                  "transmitter deployment", "valid detections",
                  silent = silent)

  # on.exit handles cleaning up the data.table modifications
  return(x)
}

#' Helper function to turn automatic matching on or off
#' 
#' Matching of the slots' contents is essential to keep the ATO operational.
#' Do not turn automatic matching off unless you are working with extremely
#' large datasets that would cause heavy computation delays.
#' 
#' @param value logical. Should matching be done on the fly?
#' @param silent Supresses messages
#' 
#' @return the current value for ATO_match_immediate if no new value is provided
#' 
#' @examples
#' old_match_immediate <- ato_match_immediate()
#' ato_match_immediate(FALSE)
#' x <- init_ato(ani = ani(example_ato),
#'              tag = tag(example_ato),
#'              det = det(example_ato),
#'              dep = dep(example_ato))
#' x <- match_update(x)
#' 
#' # clean up
#' ato_match_immediate(old_match_immediate)
#' rm(x, old_match_immediate)
#' 
#' @export
#' 
ato_match_immediate <- function(value = TRUE, silent = FALSE) {
  if (missing(value)) {
    return(getOption("ATO_match_immediate", default = TRUE))
  } else {
    if (length(value) != 1) {
      stop ("value must of length 1.", call. = FALSE)
    }
    if (!is.logical(value)) {
      stop ("value must be logical.", call. = FALSE)
    }
    options(ATO_match_immediate = value)
    if (!silent) {
      if (value) {
        message("M: Automatic matching resumed.")
      } else {
        message("M: Automatic matching paused.",
                " Use match_update() to force matching when desired.")
      }
    }
  }
}
