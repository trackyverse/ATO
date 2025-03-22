.dyn_round <- function(value, max = 10) {
  dec <- 2
  while(round(value, dec) == 0 & dec < max) {
    dec <- dec + 1
  }
  return(round(value, dec))
}
 
