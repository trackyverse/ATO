setMethod("show", "ATO", function(object) {
  cat(is(object)[[1]], "\n")
  if (nrow(object@detections) == 0) {
    cat("No detections yet added.\n")
  } else {
    cat("Detections:\n")
    cat(" -", nrow(object@detections), "observations\n")
    cat(" -", length(unique(object@detections$transmitter)), "transmitters\n")
    cat(" -", length(unique(object@detections$receiver_serial)), "receivers\n")
    cat(" - Data range:", 
        as.character(min(object@detections$datetime)),
        "to", 
        as.character(max(object@detections$datetime)), "\n")
  }
  if (nrow(object@deployments) == 0) {
    cat("No deployments yet added.\n")
  } else {
    cat("Deployments:\n")
    cat(" -", nrow(object@deployments), "deployments\n")
    cat(" -", length(unique(object@deployments$receiver_serial)), "receivers\n")
    cat(" -", length(unique(object@deployments$deploy_location)), "locations\n")
  }
  if (nrow(object@tags) == 0) {
    cat("No tags yet added.\n")
  } else {
    cat("Tags:\n")
    cat(" -", nrow(object@tags), "transmitter codes\n")
    cat(" -", length(unique(object@tags$serial)), "tags\n")
    cat(" -", length(unique(object@tags$animal)), "animals\n")
    cat(" -", length(unique(object@tags$release_location)), "release locations\n")
  }
})
