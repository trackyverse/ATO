setMethod("show", "ATO", function(object) {
  cat("ATO object\n----------\n")
  if (nrow(object@detections) == 0) {
    cat("No detections yet added.\n")
  } else {
    print.ATO_detections(object@detections)
  }
  if (nrow(object@deployments) == 0) {
    cat("No deployments yet added.\n")
  } else {
    print.ATO_deployments(object@deployments)
  }
  if (nrow(object@tags) == 0) {
    cat("No tags yet added.\n")
  } else {
    print.ATO_tags(object@tags)
  }
})

print.ATO_detections <- function(object) {
  tag_check <- any(colnames(object) %in% "tag_match")
  dep_check <- any(colnames(object) %in% "deploy_match")
  cat("ATO Detections:\n")
  cat(" -", nrow(object), "detections")
  if (tag_check | dep_check) {
    cat(" (")
    if (tag_check) {
      stray_link <- !object$tag_match
      cat(round(sum(stray_link)/nrow(object) * 100, 2))
      cat("% off-target")
      if (!is.null(object$deploy_match)) {
        cat(", ")
      }
    }
    if (dep_check) {
      orphan_link <- is.na(object$deploy_match)
      cat(round(sum(orphan_link)/nrow(object) * 100, 2))
      cat("% orphan")
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
      cat("(all on-target)")
    }
  }
  cat("\n")
  cat(" -", length(unique(object$receiver_serial)), "receivers")
  if (dep_check) {
    if (any(orphan_link)) {
      cat("(of which",
          length(unique(object$receiver_serial[orphan_link])),
          "have deployments mismatches)")
    } else {
      cat("(all deployed)")
    }
  }
  cat("\n")
  cat(" - Data range:", 
      as.character(min(object$datetime)),
      "to", 
      as.character(max(object$datetime)), "\n")
}

print.ATO_deployments <- function(object) {
  det_check <- any(colnames(object) %in% "n_detections")
  cat("ATO Deployments:\n")
  cat(" -",
      nrow(object),
      "deployments")
  aux <- object$n_detections == 0
  if (any(aux)) {
    cat(" (of which", aux,
        " had no detections)")
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
          " had no detections)")
    } else {
      cat(" (all with detections)")
    }
  }
  cat("\n")
  cat(" -", length(unique(object$deploy_location)), "locations\n")

}
print.ATO_tags <- function(object) {
  det_check <- any(colnames(object) %in% "det_match")
  cat("ATO tags:\n")
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
  aux <- length(unique(object$animal))
  cat(" -",
      aux,
      ifelse(aux > 1,
             "animals",
             "animal"))
  if (det_check) {
    if (any(det_link)) {
      aux <- length(unique(object$animal[det_link]))
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
}
