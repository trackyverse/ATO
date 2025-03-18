check_detections <- function(object) {
  if (is(object, "ATO")) {
    object <- object@detections
  }
  # object class check
  if (is(object, "data.table") | is(object, "tibble")) {
    stop("ATO tables must be data.frames (not data.table or tibble)",
         " to avoid compatibility issues.",
         call. = FALSE)
  }
  # column names check
  check <- colnames(.ATO_detections) %in% colnames(object)
  if (any(!check)) {
    stop("The detections are missing the following mandatory columns: ",
         paste0(colnames(.ATO_detections)[!check], collapse = ", "),
         call. = FALSE)
  }
  # column class check
  items <- ""
  for (i in colnames(.ATO_detections)) {
    target_col <- match(i, colnames(object))
    class_check <- class(.ATO_detections[, i]) %in% class(object[, target_col])
    if (any(!class_check)) {
      items <- paste(items, "\n -", i, "is not",
                     paste(class(.ATO_detections[, i])[!class_check],
                           collapse = ", "),
                     collapse = "")
    }
  }
  if (items != "") {
    stop("The following columns are not of the required class:",
         items, call. = FALSE)
  }
}

check_deployments <- function(object) {
  if (is(object, "ATO")) {
    object <- object@deployments
  } 
  # object class check
  if (is(object, "data.table") | is(object, "tibble")) {
    stop("ATO tables must be data.frames (not data.table or tibble)",
         " to avoid compatibility issues.",
         call. = FALSE)
  }
  # column names check
  check <- colnames(.ATO_deployments) %in% colnames(object)
  if (any(!check)) {
    stop("The deployments are missing the following mandatory columns: ",
         paste0(colnames(.ATO_deployments)[!check], collapse = ", "),
         call. = FALSE)
  }
  # column class check
  items <- ""
  for (i in colnames(.ATO_deployments)) {
    target_col <- match(i, colnames(object))
    class_check <- class(.ATO_deployments[, i]) %in% class(object[, target_col])
    if (any(!class_check)) {
      items <- paste(items, "\n -", i, "is not",
                     paste(class(.ATO_deployments[, i])[!class_check],
                           collapse = ", "),
                     collapse = "")
    }
  }
  if (items != "") {
    stop("The following columns are not of the required class:",
         items, call. = FALSE)
  }
}

check_tags <- function(object) {
  if (is(object, "ATO")) {
    object <- object@tags
  } 
  # object class check
  if (is(object, "data.table") | is(object, "tibble")) {
    stop("ATO tables must be data.frames (not data.table or tibble)",
         " to avoid compatibility issues.",
         call. = FALSE)
  }
  # column names check
  check <- colnames(.ATO_tags) %in% colnames(object)
  if (any(!check)) {
    stop("The tags are missing the following mandatory columns: ",
         paste0(colnames(.ATO_tags)[!check], collapse = ", "),
         call. = FALSE)
  }
  # column class check
  items <- ""
  for (i in colnames(.ATO_tags)) {
    target_col <- match(i, colnames(object))
    class_check <- class(.ATO_tags[, i]) %in% class(object[, target_col])
    if (any(!class_check)) {
      items <- paste(items, "\n -", i, "is not",
                     paste(class(.ATO_tags[, i])[!class_check],
                           collapse = ", "),
                     collapse = "")
    }
  }
  if (items != "") {
    stop("The following columns are not of the required class:",
         items, call. = FALSE)
  }
}

check_is_dataframe <- function(object) {
  if (is(object, "data.table")) {
    warning("ATO tables must be data.frames for compatibility.",
            " Downgrading data.table to data.frame.",
            immediate. = TRUE, call. = FALSE)
    object <- as.data.frame(object)
  }
  if (is(object, "tibble")) {
    warning("ATO tables must be data.frames for compatibility.",
            " Downgrading tibble to data.frame.",
            immediate. = TRUE, call. = FALSE)
    object <- as.data.frame(object)
  }
  return(object)
}
