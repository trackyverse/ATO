calc_det_interval <- function(object, silent = FALSE) {
  by_receiver <- split(object@detections,
                       object@detections$receiver)
  recipient1 <- lapply(by_receiver, function(receiver) {
    # cat(receiver$receiver_serial[1], "\n")
    by_tag <- split(receiver,
                    receiver$transmitter)
    recipient2 <- lapply(by_tag, function(tag) {
      # cat(" - ", tag$transmitter[1], "\n")
      tag <- tag[order(tag$datetime), ]
      tag$datetime_num <- as.numeric(tag$datetime) + tag$frac_second
      tag$time_diff <- c(NA,
                         tag$datetime_num[-1] - tag$datetime_num[-nrow(tag)])
      
      ping_rate <- NA
      tag_link <- object@tags$transmitter == tag$transmitter[1]
      dep_link <- object@deployments$transmitter == tag$transmitter[1]
      if (any(tag_link)) {
        ping_rate <- object@tags$ping_rate[tag_link]
      }
      if (any(dep_link)) {
        ping_rate <- object@deployments$transmitter_ping_rate[dep_link]
      }
      if (!is.na(ping_rate)) {
        tag$ping_dev <- tag$time_diff %% ping_rate
        dev_link <- c(FALSE, tag$ping_dev[-1] > (ping_rate / 2))
        tag$ping_dev[dev_link] <- ping_rate - tag$ping_dev[dev_link]
      } else {
        tag$ping_dev <- NA
      }
      return(tag)
    })
    output <- data.table::rbindlist(recipient2)
    return(output)
  })
  output <- data.table::rbindlist(recipient1)
  output <- as.data.frame(output)
  class(output) <- class(object@detections)
  object@detections <- output
  return(object)
}
