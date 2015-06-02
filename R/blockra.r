#' Block rearrangement algorithm with a hard convergence criterium
#'
#' @param matrix numeric matrix
#' @param epsilon target variance of row sums is epsilon multiplied by the mean of the matrix variances
#' @param shuffle randomly permute each column of the matrix before rearrangement
#' @return numeric matrix with a minimal row sum variance
#' @export
#' @seealso The \code{\link{ra}} for the rearrangement algorithm
#' @examples
#' blockra(matrix)
#' blockra(matrix(1:5, 5, 3))
#' blockra(matrix, epsilon = 0.001, shuffle = FALSE)
blockra <- function(matrix, epsilon = 0.1, shuffle = TRUE) {
  if (shuffle) {
    matrix <- apply(matrix, 2, sample)
  }

  var.new   <- var(rowSums(matrix))
  var.old   <- 2 * var.new
  target    <- epsilon * mean(apply(matrix, 2, var))

  while ((var.new > target) && (var.new < var.old)) {

    partition <- sample(0 : 1, ncol(matrix), replace = TRUE)
    matrix    <- rearrangepartition(matrix, partition)
    var.old   <- var.new
    var.new   <- var(rowSums(matrix))
  }

  return(matrix)
}
