# Calculate mean of a list
# @param l: vector
calculateMean <- function(l) {
  l <- l[!is.na(l)]
  l <- l[!is.null(l)]
  u <- mean(l)
  return(u)
}

# Calculate SD of a list
# @param l: vector
calculateStd <- function(l) {
  l <- l[!is.na(l)]
  l <- l[!is.null(l)]
  return(sd(l))
}