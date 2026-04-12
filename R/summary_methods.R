#' Summary method for an ATO_det slot object
#' 
#' @param object an ATO_det slot object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @examples
#' summary(get_det(example_ato))
#' 
#' @export
#' 
setMethod("summary", "ATO_det", function(object) {
  tag_check <- any(colnames(object) %in% "tag_match")
  beacon_check <- any(colnames(object) %in% "beacon_match")
  dep_check <- any(colnames(object) %in% "dep_match")
  cat("@det:\n")
  cat(" - ", nrow(object), " detection",
      .s(nrow(object)), " in total\n", sep = "")
  # count strays
  if (tag_check | beacon_check) {
    stray_link <- rep(TRUE, nrow(object))
    if (tag_check) {
      stray_link <- stray_link & is.na(object$tag_match)
    } else {
      cat(" - Not yet matched to @tag\n")
    }
    if (beacon_check) {
      stray_link <- stray_link & is.na(object$beacon_match)
    } else {
      cat(" - Not yet matched to @dep\n")
    }
    if (sum(stray_link) > 0) {
      cat(" - ", sum(stray_link), " stray detection", .s(sum(stray_link)),
          " (", .dyn_round(sum(stray_link)/nrow(object) * 100, 3), "%)\n", sep = "")
    } else {
      cat(" - No stray detections\n")
    }
  } else {
    cat(" - Not yet matched to @tag and @dep\n")
  }

  # count orphans
  if (dep_check) {
    orphan_link <- is.na(object$dep_match)
    if (sum(orphan_link) > 0) {
      cat(" - ", sum(orphan_link), " orphan detection", .s(sum(orphan_link)),
          " (", .dyn_round(sum(orphan_link)/nrow(object) * 100, 3), "%)\n", sep = "")
    } else {
      cat(" - No orphan detections\n")
    }
  }

  # count invalid
  if (any(!object$valid)) {
    cat(" - ", sum(!object$valid), " invalid detection", .s(sum(!object$valid)),
        " (", .dyn_round(sum(!object$valid)/nrow(object) * 100, 3), "%)\n", sep = "")
  } else {
    cat(" - No invalid detections\n")
  }

  n <- length(unique(object$transmitter))
  cat(" - ", n, " transmitter", .s(n), " detected in total\n", sep = "")
  
  if (tag_check) {
    if (any(stray_link)) {
      n <- length(unique(object$transmitter[stray_link]))
      cat(" - ", n, " stray transmitter", .s(n),
          " (", .dyn_round(n/nrow(object) * 100, 3), "%)\n", sep = "")
    } else {
      # already said no stray detections above.
      # cat(" - No stray transmitters\n")
    }
  }
  
  n <- length(unique(object$receiver_serial))
  cat(" - ", n, " receiver", .s(n), " in total\n", sep = "")
  
  if (dep_check) {
    if (any(orphan_link)) {
      n <- length(unique(object$receiver_serial[orphan_link]))
      cat(" - ", n, " deployment mismatch", .es(n),
          " (", .dyn_round(n/nrow(object) * 100, 3), "%)\n", sep = "")
    } else {
      # already said no orphan detections above.
      # cat(" - No deployment mismatches\n")
    }
  }

  if (nrow(object) > 0) {
    cat(" - Data range: ",
        as.character(min(object$datetime)),
        " to ",
        as.character(max(object$datetime)),
        " (",
        attributes(object$datetime)$tzone,
        ")\n", sep = "")
  }
})

#' Summary method for an ATO_dep object
#' 
#' @param object an ATO_dep object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @examples
#' summary(get_dep(example_ato))
#' 
#' @export
#' 
setMethod("summary", "ATO_dep", function(object) {
  det_check <- any(colnames(object) %in% "n_det")
  beacon_check <- any(colnames(object) %in% "n_beacon_det")

  cat("@dep:\n")
  cat(" - ", nrow(object), " deployment", .s(nrow(object)), " in total\n", sep = "")

  if (det_check | beacon_check) {
    zero_link <- rep(FALSE, nrow(object))
    if (det_check) {
      zero_link <- zero_link & object$n_det == 0
    } else {
      cat(" - Not yet matched to @det\n")
    }
    if (beacon_check) {
      zero_link <- zero_link & object$n_beacon_det == 0
    } else {
      # already said above
      # cat(" - Not yet matched to @det\n")
    }
    if (sum(zero_link) > 0) {
      cat(" - ", sum(zero_link), " deployment", .s(sum(zero_link)),
          "with no detections (", 
          .dyn_round(sum(zero_link)/nrow(object) * 100, 3), "%)\n", sep = "")
    }
  }

  if (any(!object$valid)) {
    cat(" - ", sum(!object$valid), " invalid deployment", .s(sum(!object$valid)),
        " (", .dyn_round(sum(!object$valid)/nrow(object) * 100, 3), "%)\n", sep = "")
  } else {
    cat(" - No invalid deployments\n")
  }

  n <- length(unique(object$receiver_serial))
  cat(" - ", n, " receiver", .s(n), " deployed\n", sep = "")

  n <- length(unique(object$transmitter))
  cat(" - ", n, " beacon transmitter", .s(n), " deployed\n", sep = "")

  if (any(!is.na(object$deploy_location))) {
    n <- length(unique(object$deploy_location))
    cat(" - ", n, " listed deployment location", .s(n), "\n", sep = "")
  } else {
    cat(" - No named deployment locations\n")
  }
})

#' Summary method for an ATO_tag slot object
#' 
#' @param object an ATO_tag slot object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @examples
#' summary(get_tag(example_ato))
#' 
#' @export
#' 
setMethod("summary", "ATO_tag", function(object) {
  ani_check <- any(colnames(object) %in% "ani_match")
  det_check <- any(colnames(object) %in% "n_det")
  obs_check <- any(colnames(object) %in% "n_obs")
  
  cat("@tag:\n")
  cat(" - ", nrow(object), " transmitter code",
      .s(nrow(object)), "\n", sep = "")
  if (ani_check) {
    n <- sum(is.na(object$ani_match))
    if (n > 0) {
      cat(" - ", n, " failed animal match", .es(n), "\n", sep = "")
    } else {
      cat(" - All matched to animals\n")
    }
  } else {
    cat(" - Not yet matched to @ani\n")
  }
  if (det_check) {
    n <- sum(object$n_det == 0)
    if (n > 0) {
      cat(" - ", n, " never detected\n", sep = "")
    } else {
      cat(" - All detected\n")
    }
  } else {
    cat(" - Not yet matched to @det\n")
  }
  if (obs_check) {
    n <- sum(object$n_obs == 0)
    if (n > 0) {
      cat(" - ", n, " never observed\n", sep = "")
    } else {
      cat("- All observed\n")
    }
  } else {
    cat(" - Not yet matched to @obs\n")
  }
})

#' Summary method for an ATO_ani slot object
#' 
#' @param object an ATO_ani slot object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @examples
#' summary(get_ani(example_ato))
#' 
#' @export
#' 
setMethod("summary", "ATO_ani", function(object) {
  ani_check <- any(colnames(object) %in% "n_tag")
  det_check <- any(colnames(object) %in% "n_det")
  obs_check <- any(colnames(object) %in% "n_obs")
  cat("@ani:\n")
  cat(" - ", nrow(object), " animal", .s(nrow(object)), " in total\n", sep = "")
  if (ani_check) {
    n <- sum(object$n_ani == 0)
    if (n > 0) {
      cat(" - ", n, " without associated tags\n", sep = "")
    } else {
      cat(" - All with associated tags\n")
    }
  } else {
    cat(" - Not yet matched to @tag\n")
  }
  if (det_check) {
    n <- sum(object$n_det == 0)
    if (n > 0) {
      cat(" - ", n, " with no detections\n", sep = "")
    } else {
      cat(" - All detected\n")
    }
  } else {
    cat(" - Not yet matched to @det (through @tag)\n")
  }
  if (obs_check) {
    n <- sum(object$n_obs == 0)
    if (n > 0) {
      cat(" - ", n, " with no observations\n", sep = "")
    } else {
      cat(" - All observed\n")
    }
  } else {
    cat(" - Not yet matched to @obs\n")
  }
  n <- length(unique(object$release_location))
  cat(" - ", n, " release location", .s(n), "\n", sep = "")
})

#' Summary method for an ATO_obs slot object
#' 
#' @param object an ATO_obs slot object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @examples
#' summary(get_obs(example_ato))
#' 
#' @export
#' 
setMethod("summary", "ATO_obs", function(object) {
  tag_check <- any(colnames(object) %in% "tag_match")
  ani_check <- any(colnames(object) %in% "ani_match")
  cat("@obs:\n")
  cat(" -", nrow(object), "observations\n", sep = "")


  if (tag_check | ani_check) {
    stray_link <- rep(FALSE, nrow(object))
    if (!tag_check & ani_check) {
      stray_link <- stray_link & is.na(object$ani_match)
    } else {
      cat(" - Not yet matched to @ani\n")
    }
    if (tag_check & !ani_check) {
      stray_link <- stray_link & is.na(object$tag_match)
    } else {
      cat(" - Not yet matched to @tag\n")
    }
    if (sum(stray_link) > 0) {
      cat(" - ", sum(stray_link), " stray observations", .s(sum(stray_link)),
          " (", .dyn_round(sum(stray_link)/nrow(object) * 100, 3), "%)\n", sep = "")
    } else {
      cat(" - No stray observations\n")
    }
  }
  n <- sum(object$terminal)
  if (n) {
    cat(" -", sum(object$terminal), " observations are terminal\n", sep = "")
  } else {
    cat(" - No terminal observations\n")
  }
})

#' Summary method for an ATO_log slot object
#' 
#' @param object an ATO_log slot object
#' 
#' @examples
#' summary(get_log(example_ato))
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_log", function(object) {
  cat("@log:\n", sep = "")
  n1 <- nrow(object)
  n2 <- length(unique(object$pkg))
  cat(" - ", n1, " log entr", .y(n1),
      " from ", n2, " package", .s(n2),
      "\n", sep = "")
})
