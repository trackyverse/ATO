#' Summary method for an ATO_det object
#' 
#' @param object an ATO_det object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_det", function(object) {
  tag_check <- any(colnames(object) %in% "tag_match")
  dep_check <- any(colnames(object) %in% "dep_match")
  cat("@det:\n")
  cat(" -", nrow(object), "detections")
  if (tag_check | dep_check) {
    cat(" (")
    if (tag_check) {
      stray_link <- !object$tag_match
      cat(round(sum(stray_link)/nrow(object) * 100, 2))
      cat("% off-target")
      if (!is.null(object$dep_match)) {
        cat(", ")
      }
    }
    if (dep_check) {
      orphan_link <- is.na(object$dep_match)
      cat(round(sum(orphan_link)/nrow(object) * 100, 2))
      cat("% orphan")
      if (any(!object$valid)) {
        cat(", ")
      }
   }
    if (any(!object$valid)) {
      cat(round(sum(!object$valid)/nrow(object) * 100, 2))
      cat("% marked as invalid")
    }
    cat(")")
  }
  cat("\n")
  cat(" -", length(unique(object$transmitter)), "transmitters")
  if (tag_check) {
    if (any(stray_link)) {
      cat(" (of which",
          length(unique(object$transmitter[stray_link])),
          "are stray transmitters)")
    } else {
      cat(" (all on-target)")
    }
  }
  cat("\n")
  cat(" -", length(unique(object$receiver_serial)), "receivers")
  if (dep_check) {
    if (any(orphan_link)) {
      aux <- length(unique(object$receiver_serial[orphan_link]))
      cat(" (of which",
          aux,
          ifelse(aux > 1,
                 "have deployments mismatches)",
                 "has deployments mismatches)"))
    } else {
      cat(" (all deployed)")
    }
  }
  cat("\n")
  cat(" - Data range:", 
      as.character(min(object$datetime)),
      "to", 
      as.character(max(object$datetime)), "\n")
})

#' Summary method for an ATO_dep object
#' 
#' @param object an ATO_dep object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_dep", function(object) {
  det_check <- any(colnames(object) %in% "n_detections")
  cat("@dep:\n")
  cat(" -",
      nrow(object),
      "deployments")
  aux <- sum(object$n_detections == 0)
  if (any(aux)) {
    cat(" (of which", aux,
        "had no detections)")
  } else {
    cat(" (all with detections)")
  }
  cat("\n")
  cat(" -",
      length(unique(object$receiver_serial)),
      "receivers")
  if (det_check) {
    aux <- aggregate(object$n_detections,
                     list(object$receiver_serial),
                     max)
    if (any(aux$x == 0)) {
      cat(" (of which", sum(aux$x == 0),
          "had no detections)")
    } else {
      cat(" (all with detections)")
    }
  }
  cat("\n")
  cat(" -",
      length(!is.na(object$transmitter)),
      "beacon transmitters")
  cat("\n")
  cat(" -", length(unique(object$deploy_location)), "locations\n")
})

#' Summary method for an ATO_tag object
#' 
#' @param object an ATO_tag object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_tag", function(object) {
  det_check <- any(colnames(object) %in% "det_match")
  cat("@tag:\n")
  cat(" -", 
      nrow(object),
      ifelse(nrow(object) > 1,
              "transmitter codes",
              "transmitter code"))
  if (det_check) {
    det_link <- !object$det_match
    if (any(det_link)) {
      aux <- sum(det_link)
      cat(" (of which",
          aux,
          ifelse(aux > 1,
                 "were never detected)",
                 "was never detected)"))
    } else {
      cat(" (all detected)")
    }
  }
  cat("\n")
  aux <- length(unique(object$serial))
  cat(" -",
      aux,
      ifelse(aux > 1,
             "tags",
             "tag"))
  if (det_check) {
    if (any(det_link)) {
      aux <- length(unique(object$serial[det_link]))
      cat(" (of which",
          aux,
          ifelse(aux > 1,
                 "were never detected)",
                 "was never detected)"))
    } else {
      cat(" (all detected)")
    }
  }
  cat("\n")
})

#' Summary method for an ATO_ani object
#' 
#' @param object an ATO_ani object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_ani", function(object) {
  det_check <- any(colnames(object) %in% "det_match")
  cat("@ani:\n")
  aux <- nrow(object)
  cat(" -",
     aux,
      ifelse(aux > 1,
             "animals",
             "animal"))
  if (det_check) {
    det_link <- !object$det_match
    if (any(det_link)) {
      aux <- sum(det_link)
      cat(" (of which",
          aux,
          ifelse(aux > 1,
                 "were never detected)",
                 "was never detected)"))
    } else {
      cat(" (all detected)")
    }
  }
  cat("\n")
  aux <- length(unique(object$release_location))
  cat(" -",
      aux,
      ifelse(aux > 1,
             "release locations",
             "release location"))
  cat("\n")
})

#' Summary method for an ATO_obs object
#' 
#' @param object an ATO_obs object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_obs", function(object) {
  tag_check <- any(colnames(object) %in% "tag_match")
  ani_check <- any(colnames(object) %in% "ani_match")
  cat("@obs:\n")
  cat(" -", nrow(object), "observations")
  if (tag_check | ani_check) {
    if (tag_check & !ani_check) {
      stray_link <- !object$tag_match
    }
    if (!tag_check & ani_check) {
      stray_link <- !object$ani_match
    }
    if (tag_check & ani_check) {
      stray_link <- !object$ani_match & !object$tag_match
    }
    cat(" (")
    cat(round(sum(stray_link)/nrow(object) * 100, 2))
    cat("% off-target")
    cat(")")
  }
  cat("\n")
  cat(" -", sum(object$terminal), " are terminal")
  cat("\n")
})

#' Summary method for an ATO_log object
#' 
#' @param object an ATO_log object
#' 
#' @return Nothing. Prints a summary.
#' 
#' @export
#' 
setMethod("summary", "ATO_log", function(object) {
  cat("@log:\n")
  aux <- length(unique(object@package))
  cat(" -", nrow(object), "log entries from ",
      aux,
      ifelse(aux > 1,
             " packages",
             " package"))
  cat("\n")
})
