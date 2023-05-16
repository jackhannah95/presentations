n_day <- function(start, end, n) {
  
  if (!inherits(start, "Date") | !inherits(end, "Date")) {
    stop("The start and end dates must have Date class")
  }
  
  # Note that Sunday is considered to be day 0; not day 7
  if (!n %in% 0:6) {
    stop("The day of the week must be a number between 0 and 6")
  }
  
  sum(format(seq(start, end, by = "day"), "%w") == n)
}
