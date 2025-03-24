setMethod("show", "ATO", function(object) {
  cat("ATO object\n----------\n")
  if (nrow(object@det) == 0) {
    cat("No detection info yet added.\n")
  } else {
    summary(object@det)
  }
  if (nrow(object@dep) == 0) {
    cat("No deployment info yet added.\n")
  } else {
    summary(object@dep)
  }
  if (nrow(object@tag) == 0) {
    cat("No tag info yet added.\n")
  } else {
    summary(object@tag)
  }
})
