#' Rearrangement Algorithm
#'
#' @param matrix numeric matrix
#' @param epsilon target variance of row sums is epsilon multiplied by the mean of the matrix variances
#' @param shuffle randomly permute each column of the matrix before rearrangement
#' @return numeric matrix with a minimal row sum variance
#' @export
#' @seealso \code{\link{blockra}} for the block rearrangement algorithm
#' @seealso \code{\link{brave}} for the block rearrangement variance equalizer
#' @examples
#' ra(matrix)
#' ra(matrix(1:5, 5, 3))
#' ra(matrix, epsilon = 0.001, shuffle = FALSE)
ra <- function(matrix, epsilon = 0.1, shuffle = TRUE) {
  if (shuffle) {
    matrix <- apply(matrix, 2, sample)
  }

  var.new   <- var(rowSums(matrix))
  var.old   <- 2 * var.new
  target    <- epsilon * mean(apply(matrix, 2, var))

  while ((var.new > target) && (var.new < var.old)) {

    for (col in 1 : ncol(matrix)) {
      current         <- sort(matrix[, col])
      other.sums      <- rowSums(matrix[ , -col])
      other.sums.rank <- rank(-other.sums, ties.method = "random")
      matrix[, col]   <- current[other.sums.rank]
    }

    var.old <- var.new
    var.new <- var(rowSums(matrix))
  }

  return(matrix)
}
